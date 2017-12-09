package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException

import java.net.ServerSocket
import java.net.Socket

import java.util.concurrent.Phaser

import collection.mutable.HashMap

import sys.process._

class ClusterStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction, isServer: Boolean,
  serverIP: String, serverPort: Int, clients: Alloy => HashMap[String,DataRange],
  thisName: String, keyFile: Option[String]) extends Strategy {

  val a = Alloy(width, height, depth, materialsDef)
  val b = a.mirror()

  stampPattern(a)
  stampDots(a)

  val clientsRanges = clients(a)

  for ((k,v) <- clientsRanges) {
    println(f"$k => $v")
  }

  def run(): Unit = {
    if (isServer) {
      server()
    } else {
      client()
    }
  }

  private def server(): Unit = {
    try {
      val phaser = new Phaser(clientsRanges.size + 1)

      onThread(() => {
        for (i <- 0 until iterations) {
          phaser.arriveAndAwaitAdvance()
          val alloy = if (i % 2 == 0) {
            a
          } else {
            b
          }

          displayFunction(alloy, i)
        }
      })

      val serverSocket = new ServerSocket(serverPort)

      try {
        println(f"Started server: $serverIP:$serverPort")

        remoteExecuteClients()

        for (_ <- 0 until clientsRanges.size) {
          val clientSocket = serverSocket.accept()
          onThread(() => {
            val input = new DataInputStream(clientSocket.getInputStream())
            val output = new DataOutputStream(clientSocket.getOutputStream())

            val clientName = input.readUTF()

            println(f"Got client: $clientName")

            val range = clientsRanges(clientName)

            //val range = DataRange(0, width - 1, width, height, depth, false, false)
            val protocol = new ClusterServerProtocol(a, b, iterations, range, phaser,
              input, output)
            protocol.start()
          })
        }
      } finally {
        serverSocket.close()
      }
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  private def client(): Unit = {
    println(f"Attempting to connect to: $serverIP:$serverPort")
    val socket = new Socket(serverIP, serverPort)

    println("Connected to server")

    val input = new DataInputStream(socket.getInputStream())
    val output = new DataOutputStream(socket.getOutputStream())

    output.writeUTF(thisName)

    val protocol = new ClusterClientProtocol("rho", input, output)
    protocol.start()
  }

  private def onThread[A](function: () => A): Unit = {
    val thread = new Thread {
      override def run {
        function()
      }
    }

    thread.start()
  }

  private def remoteExecuteClients(): Unit = {
    val key = keyFile.get
    for ((c, _) <- clientsRanges) {
      onThread(() => {
        //val clientCommand = f"screen -d -m java -jar alloysimulation.jar client $serverIP $serverPort $c"
        val clientCommand = f"java -jar alloysimulation.jar client $serverIP $serverPort $c"
        val sshCommand = f"ssh -i $key $c $clientCommand"

        println(sshCommand)
        sshCommand !
      })
    }
    println("Finished executing clients")
  }
}
