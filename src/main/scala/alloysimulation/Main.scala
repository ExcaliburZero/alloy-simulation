package alloysimulation

import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import collection.mutable.HashMap

object Main {
  type ClusterArgs = (Boolean, String, Int, String, Option[String], Option[String])

  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 100

    val width = 1024
    val height = 1024
    val depth = 1

    val writeImage = false
    val strategyType: StrategyType = Cluster()
    val dotsTemp = 10000
    val patternTemp = 7000

    val displayFunction = if (writeImage) {
      writeAlloyToFile(_,_)
    } else {
      (_: Alloy, gen: Alloy.Generation) => println(gen)
    }

    val alloy = Alloy(width, height, depth, materialsDef)

    stampPattern(alloy, patternTemp)
    stampDots(alloy, dotsTemp)

    val strategy: Strategy = strategyType match {
      case SingleCore() =>
        new SingleThreadStrategy(alloy, iterations, displayFunction)

      case ForkJoin() =>
        val smallThreshold = 16384

        new ForkJoinStrategy(alloy, iterations, displayFunction,
          smallThreshold)

      case Cluster() =>
        val (isServer, serverIP, serverPort, thisName, keyFile, clientsFile) =
          if (args.head == "server") {
            getServerArgs(args)
          } else {
            getClientArgs(args)
          }

        val clients = clientsFile.map(getClients)

        new ClusterStrategy(alloy, iterations, displayFunction, isServer,
          serverIP, serverPort, clients, thisName, keyFile)
    }

    strategy.run()
  }

  private def getServerArgs(args: Array[String]): ClusterArgs = {
    try {
      val ip = args(1)
      val port = args(2).toInt
      val key = Some(args(3))
      val clientsFile = args(4)

      (true, ip, port, "localhost", key, Some(clientsFile))
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        sys.error(
          "Server arguments are incorrect.\n" +
          "Correct arguements are: IP PORT KEY_FILE CLIENTS_FILE"
        )
    }
  }

  private def getClientArgs(args: Array[String]): ClusterArgs = {
    try {
      val ip = args(1)
      val port = args(2).toInt
      val name = args(3)

      (false, ip, port, name, None, None)
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        sys.error(
          "Client arguments are incorrect.\n" +
          "Correct arguements are: IP PORT CLIENT_NAME"
        )
    }
  }

  private def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    val filepath = "frames/%05d.png".format(generation)
    RGBBitmap.writeToPNG(alloy, filepath)

    println(generation)
  }

  private def getClients(clientsFile: String): Alloy => HashMap[String, DataRange] = {
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

  private def readFile(filepath: String): String = {
    new String(Files.readAllBytes(Paths.get(filepath)), UTF_8)
  }

  private def stampDots(alloy: Alloy, temperature: Double): Unit = {
    val width = alloy.getWidth()
    val height = alloy.getHeight()

    for (
      a <- List(1, 9);
      b <- List(1, 9)
    ) {
      val x = a * (width / 10)
      val y = b * (height / 10)

      alloy.update(x.toInt, y.toInt, 0, temperature * (a + b))
    }
  }

  private def stampPattern(alloy: Alloy, temperature: Double): Unit = {
    val width = alloy.getWidth()
    val height = alloy.getHeight()

    val widthBrush = width / 10
    val heightBrush = height / 10

    val boundary = Math.min(
      ((width / 2) * 0.8).toInt,
      ((height / 2) * 0.8).toInt
    )
    val numRings = 12
    val interval = boundary / numRings

    val centerX = width / 2
    val centerY = height / 2
    for (
      r <- 1 until boundary;
      if r % interval == 0
    ) {
      val numRotations = r / interval * 2
      val rotationDeg = 360 / numRotations

      for (d <- 0 until numRotations) {
        val points = for(i <- 0 until 6) yield {
          (centerX + r * Math.cos(d * rotationDeg + i * 2 * Math.PI / 6),
           centerY + r * Math.sin(d * rotationDeg + i * 2 * Math.PI / 6))
        }

        for ((x, y) <- points) {
          alloy.update(x.toInt, y.toInt, 0, temperature)
        }
      }
    }
  }
}
