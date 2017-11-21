package alloysimulation

abstract class Strategy {
  def run(): Unit

  protected def stampDots(alloy: Alloy): Unit = {
    val temperature = 10000

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

  protected def stampPattern(alloy: Alloy): Unit = {
    val temperature = 7000

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
