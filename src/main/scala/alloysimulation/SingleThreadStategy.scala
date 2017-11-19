package alloysimulation

class SingleThreadStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction) extends Strategy {

  val alloy = new Alloy(width, height, depth, materialsDef)
  alloy.randomizeTemps()

  def run(): Unit = {
    for (i <- 0 until iterations) {
      displayFunction(alloy, i)
      alloy.calculateNextTemp()
    }
  }
}
