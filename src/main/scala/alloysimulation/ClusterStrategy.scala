package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException

import java.net.ServerSocket
import java.net.Socket

import java.util.concurrent.Phaser

class ClusterStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction, isServer: Boolean,
  serverIP: String, serverPort: Int) extends Strategy {

  val a = Alloy(width, height, depth, materialsDef)
  val b = a.mirror()

  stampPattern(a)
  stampDots(a)

  def run(): Unit = {
    if (isServer) {
      server()
    } else {
      client()
    }
  }

  private def server(): Unit = {
    try {
      val phaser = new Phaser(1 + 1)

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

        // TODO(chris): Do for all child machines on separate threads
        //onThread(() => {
          val clientSocket = serverSocket.accept()

          println("Got client")

          val input = new DataInputStream(clientSocket.getInputStream())
          val output = new DataOutputStream(clientSocket.getOutputStream())

          val range = DataRange(0, width - 1, width, height, depth, false, false)
          val protocol = new ClusterServerProtocol(a, b, iterations, range, phaser,
            input, output)
          protocol.start()
        //})
      } finally {
        serverSocket.close()
      }
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  private def client(): Unit = {
    val socket = new Socket(serverIP, serverPort)

    println("Connected to server")

    val input = new DataInputStream(socket.getInputStream())
    val output = new DataOutputStream(socket.getOutputStream())

    val protocol = new ClusterClientProtocol("rho", input, output)
    protocol.start()
  }

  private def onThread(function: () => Unit): Unit = {
    val thread = new Thread {
      override def run {
        function()
      }
    }

    thread.start()
  }
}
