package alloysimulation

object Main {
  def main(args: Array[String]): Unit = {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = writeAlloyToFile(_,_)

    val strategy: Strategy =
      new SingleThreadStrategy(5, 8,10, materialsDef, iterations,
        displayFunction)

    strategy.run()
  }

  def writeAlloyToFile(alloy: Alloy, generation: Alloy.Generation): Unit = {
    val filepath = f"frames/$generation.txt"
    alloy.saveGNUPlot(filepath)
  }
}
