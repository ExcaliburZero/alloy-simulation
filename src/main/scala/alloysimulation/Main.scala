package alloysimulation

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 500//250
    val displayFunction = writeAlloyToFile(_,_)
    val smallThreshold = 16384
    val isServer = args.length > 0 && args.head == "server"

    val width = 256 * 2
    val height = 256 * 2

    val strategy: Strategy =
      //new SingleThreadStrategy(width, height, 1, materialsDef, iterations,
      //  displayFunction)
      new ForkJoinStrategy(width, height, 1, materialsDef, iterations,
        displayFunction, smallThreshold)
      //new ClusterStrategy(width, height, 1, materialsDef, iterations,
      //  displayFunction, isServer)

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    //val filepath = f"frames/$generation.ppm"
    //alloy.toRGBBitmap.writeToPPM(filepath)
    //RGBBitmap.writeToPPM(alloy, filepath)
    val filepath = "frames/%05d.png".format(generation)
    RGBBitmap.writeToPNG(alloy, filepath)

    println(generation)
  }
}
