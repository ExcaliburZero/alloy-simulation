package alloysimulation

import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import collection.mutable.HashMap

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 100

    val width = 1024
    val height = 1024
    val depth = 1

    val writeImage = false
    val strategyType: StrategyType = Cluster()

    val displayFunction = if (writeImage) {
      writeAlloyToFile(_,_)
    } else {
      (_: Alloy, gen: Alloy.Generation) => println(gen)
    }

    val strategy: Strategy = strategyType match {
      case SingleCore() =>
        new SingleThreadStrategy(width, height, depth, materialsDef,
          iterations, displayFunction)

      case ForkJoin() =>
        val smallThreshold = 16384

        new ForkJoinStrategy(width, height, depth, materialsDef, iterations,
          displayFunction, smallThreshold)

      case Cluster() =>
        val (isServer, serverIP, serverPort, thisName, keyFile, clientsFile) =
          if (args.head == "server") {
            val ip = args(1)
            val port = args(2).toInt
            val key = Some(args(3))
            val clientsFile = args(4)

            (true, ip, port, "localhost", key, Some(clientsFile))
          } else {
            val ip = args(1)
            val port = args(2).toInt
            val name = args(3)

            (false, ip, port, name, None, None)
          }

        val clients = clientsFile.map(getClients)

        new ClusterStrategy(width, height, depth, materialsDef, iterations,
          displayFunction, isServer, serverIP, serverPort, clients, thisName,
          keyFile)
    }

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    val filepath = "frames/%05d.png".format(generation)
    RGBBitmap.writeToPNG(alloy, filepath)

    println(generation)
  }

  def getClients(clientsFile: String): Alloy => HashMap[String, DataRange] = {
    val contents = readFile(clientsFile)


    val clients = for (line <- contents.split("\n")) yield {
      val parts = line.split(",")

      val name = parts.head
      val number = parts(1).toInt

      (name, number)
    }

    val total = clients.map(_._2).sum

    (alloy: Alloy) => {
      val width = alloy.width
      val height = alloy.height
      val depth = alloy.depth

      val per: Int = width / total

      val clientsRanges = new HashMap[String, DataRange]()
      var start = 0
      for (i <- 0 until clients.size) {
        val end = if (i == clients.size -1) {
          val before = start + clients(i)._2 * per

          before + (width - before - 1)
        } else {
          start + clients(i)._2 * per
        }

        val hasAbove = i > 0
        val hasBelow = i < clients.size - 1

        val range = DataRange(start, end, width, height, depth, hasAbove,
          hasBelow)

        start = end

        clientsRanges.put(clients(i)._1, range)
      }

      clientsRanges
    }
  }

  def readFile(filepath: String): String = {
    new String(Files.readAllBytes(Paths.get(filepath)), UTF_8)
  }
}
