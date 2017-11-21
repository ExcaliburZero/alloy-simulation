package alloysimulation

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val iterations = 201
    val displayFunction = writeAlloyToFile(_,_)
    val smallThreshold = 4096

    val strategy: Strategy =
      //new SingleThreadStrategy(100, 100, 1, materialsDef, iterations,
      //  displayFunction)
      new ForkJoinStrategy(256, 256, 1, materialsDef, iterations,
        displayFunction, smallThreshold)

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    val filepath = f"frames/$generation.ppm"
    alloy.toRGBBitmap.writeToPPM(filepath)

    println(generation)
  }
}
