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
  serverIP: String, serverPort: Int,
  clients: Option[Alloy => HashMap[String,DataRange]],
  thisName: String, keyFile: Option[String]) extends Strategy {

  val a = Alloy(width, height, depth, materialsDef)
  val b = a.mirror()

  stampPattern(a)
  stampDots(a)

  val clientsRanges = clients.map(f => f(a))

  printClientRanges()

  def run(): Unit = {
    if (isServer) {
      server()
    } else {
      client()
    }
  }

  private def server(): Unit = {
    val numClients = clientsRanges.get.size

    val phaser = new Phaser(numClients + 1)

    onThread(() => {
      handleAlloyDisplaying(phaser)
    })

    val serverSocket = new ServerSocket(serverPort)

    try {
      println(f"Started server: $serverIP:$serverPort")

      remoteExecuteClients()

      for (_ <- 0 until numClients) {
        val clientSocket = serverSocket.accept()
        onThread(() => {
          handleClient(clientSocket, phaser)
        })
      }
    } finally {
      serverSocket.close()
    }
  }

  private def handleAlloyDisplaying(phaser: Phaser): Unit = {
    for (i <- 0 until iterations) {
      phaser.arriveAndAwaitAdvance()
      val alloy = if (i % 2 == 0) {
        a
      } else {
        b
      }

      displayFunction(alloy, i)
    }
  }

  private def handleClient(clientSocket: Socket, phaser: Phaser): Unit = {
    val input = new DataInputStream(clientSocket.getInputStream())
    val output = new DataOutputStream(clientSocket.getOutputStream())

    val clientName = input.readUTF()

    println(f"Got client: $clientName")

    val range = (clientsRanges.get)(clientName)

    val protocol = new ClusterServerProtocol(a, b, iterations, range, phaser,
      input, output)
    protocol.start()
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
    for ((c, _) <- clientsRanges.get) {
      onThread(() => {
        startClientViaSsh(key, c)
      })
    }
    println("Finished executing clients")
  }

  private def startClientViaSsh(key: String, client: String): Unit = {
    val javaCommand = "/home/dl/jdk/jdk9/bin/java"
    val clientCommand = f"$javaCommand -jar alloysimulation.jar client $serverIP $serverPort $client"
    val sshCommand = f"ssh -i $key $client $clientCommand"

    println(sshCommand)
    sshCommand !
  }

  private def printClientRanges(): Unit = {
    clientsRanges.map(cr =>{
      for ((k,v) <- cr) {
        println(f"$k => $v")
      }
    })
  }
}
