package alloysimulation

class SingleThreadStrategy(alloy: Alloy, iterations: Int,
  displayFunction: Alloy.DisplayFunction) extends Strategy {

  val alloy2 = alloy.mirror()

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
