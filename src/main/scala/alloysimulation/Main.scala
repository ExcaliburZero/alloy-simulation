package alloysimulation

import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import collection.mutable.HashMap

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 500//250
    val smallThreshold = 16384

    val clients = getClients("clients.csv")

    val (isServer, serverIP, serverPort, thisName) = if (args.head == "server") {
      (true, "localhost", 4658, "localhost")
    } else {
      val ip = args(1)
      val port = args(2).toInt
      val name = args(3)

      (false, ip, port, name)
    }

    val width = 1024 * 1
    val height = 1024 * 1

    val writeImage = false

    val displayFunction = if (writeImage) {
      writeAlloyToFile(_,_)
    } else {
      (_: Alloy, gen: Alloy.Generation) => println(gen)
    }

    val strategy: Strategy =
      //new SingleThreadStrategy(width, height, 1, materialsDef, iterations,
      //  displayFunction)
      //new ForkJoinStrategy(width, height, 1, materialsDef, iterations,
      //  displayFunction, smallThreshold)
      new ClusterStrategy(width, height, 1, materialsDef, iterations,
        displayFunction, isServer, serverIP, serverPort, clients, thisName)

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
        //val j = i * clients(i)._2
        

        /*val start = if (i == 0) {
          j * per
        } else {
          j * per - 1
        }*/

        val end = if (i == clients.size -1) {
          //(j + 1) * per - 1 + (width - (j + 1) * per)
          val before = start + (i + 1) * clients(i)._2 * per

          before + (width - before - 1)
        } else {
          start + (i + 1) * clients(i)._2 * per
          //((i + 1) * clients(i + 1)._2 + 1) * per - 1
        }

        //val end = (j + 1) * per - 1 + (if (i == clients.size - 1)
        //  width - (j + 1) * per else 0)

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
