package alloysimulation

class SingleThreadStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction) extends Strategy {

  val alloy = new Alloy(width, height, depth, materialsDef)
  val alloy2 = alloy.mirror()
  alloy.randomizeTemps()

  def run(): Unit = {
    for (i <- 0 until iterations) {
      val (a, b) = if (i % 2 == 0) {
        (alloy, alloy2)
      } else {
        (alloy2, alloy)
      }

      displayFunction(a, i)
      a.calculateNextTemp(b)
    }
  }
}
