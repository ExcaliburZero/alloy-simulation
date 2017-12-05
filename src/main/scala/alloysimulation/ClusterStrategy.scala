package alloysimulation

import java.io._

class ClusterStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction, isServer: Boolean) extends Strategy {

  val alloy = Alloy(width, height, depth, materialsDef)

  stampPattern(alloy)
  stampDots(alloy)

  def run(): Unit = {
    val port = 5694
    if (isServer) {
      server(port)
    } else {
      client(port)
    }
  }

  private def server(port: Int): Unit = {
    /*try {
      val serverSocket = new ServerSocket(port)
      val clientSocket = serverSocket.accept()
      val out = new PrintWriter(clientSocket.getOutputStream(), true)
      val in = new BufferedReader(
        new InputStreamReader(clientSocket.getInputStream())
      )
    } catch {
      case e: IOException => e.printStackTrace()
    }*/
  }

  private def client(port: Int): Unit = {
  }
}
