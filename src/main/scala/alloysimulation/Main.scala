package alloysimulation

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 500//250
    val smallThreshold = 16384

    val (isServer, serverIP, serverPort) = if (args.head == "server") {
      (true, "localhost", 4658)
    } else {
      val ip = args(1)
      val port = args(2).toInt

      (false, ip, port)
    }

    val width = 1024 * 4
    val height = 1024 * 4

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
        displayFunction, isServer, serverIP, serverPort)

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    val filepath = "frames/%05d.png".format(generation)
    RGBBitmap.writeToPNG(alloy, filepath)

    println(generation)
  }
}
