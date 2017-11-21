package alloysimulation

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 100
    val displayFunction = writeAlloyToFile(_,_)
    val smallThreshold = 16384

    val strategy: Strategy =
      //new SingleThreadStrategy(1024, 1024, 1, materialsDef, iterations,
      //  displayFunction)
      new ForkJoinStrategy(1024, 1024, 1, materialsDef, iterations,
        displayFunction, smallThreshold)

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    //val filepath = f"frames/$generation.ppm"
    //alloy.toRGBBitmap.writeToPPM(filepath)
    //RGBBitmap.writeToPPM(alloy, filepath)
    val filepath = f"frames/$generation.png"
    RGBBitmap.writeToPNG(alloy, filepath)

    println(generation)
  }
}
