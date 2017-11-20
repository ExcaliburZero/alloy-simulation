package alloysimulation

import java.io.PrintWriter

class MaterialsDefinition(const1: Double, const2: Double, const3: Double,
  private val ratios: (Int,Int,Int)) {

  private val totalRatios: Double = ratios._1 + ratios._2 + ratios._3
  val percent1 = ratios._1 / totalRatios
  val percent2 = ratios._2 / totalRatios
  val percent3 = ratios._3 / totalRatios

  def getConstant(i: Int): Double = {
    if (i == 0) {
      const1
    } else if (i == 1) {
      const2
    } else {
      const3
    }
  }
}

object Alloy {
  type Point = Double
  type Material = Array[Double]

  type Generation = Int
  type DisplayFunction = (Alloy, Generation) => Unit
}

class Alloy(width: Int, height: Int, depth: Int, materialsDef: MaterialsDefinition) {
  private val points = Array.ofDim[Alloy.Point](width, height, depth)

  val materials = Array.ofDim[Double](width, height, depth, 3)

  for (
    x <- 0 until width;
    y <- 0 until height;
    z <- 0 until depth
  ) yield updateMaterial(x, y, z, randomMaterial())

  def apply(x: Int, y: Int, z: Int): Alloy.Point = {
    points(x)(y)(z)
  }

  def update(x: Int, y: Int, z: Int, value: Alloy.Point): Unit = {
    points(x)(y).update(z, value)
  }

  def material(x: Int, y: Int, z: Int): Alloy.Material = {
    materials(x)(y)(z)
  }

  def getConstant(m: Int): Double = {
    materialsDef.getConstant(m)
  }

  private def updateMaterial(x: Int, y: Int, z: Int, value: Alloy.Material): Unit = {
    materials(x)(y).update(z, value)
  }

  private def randomMaterial(): Alloy.Material = {
    val p1 = Math.min(1.0, materialsDef.percent1 + (scala.util.Random.nextDouble() * 50.0) - 25.0)
    val p2 = Math.min(1.0, materialsDef.percent2 + (scala.util.Random.nextDouble() * 50.0) - 25.0)
    val p3 = Math.min(1.0, materialsDef.percent3 + (scala.util.Random.nextDouble() * 50.0) - 25.0)

    val total = p1 + p2 + p3

    List(p1 / total, p2 / total, p3 / total).map(Math.abs(_)).toArray
  }

  def randomizeTemps(): Unit = {
    val r = scala.util.Random
    val lines = for (
        x <- 0 until width;
        y <- 0 until height;
        z <- 0 until depth
      ) yield update(x, y, z, 10.0 * r.nextDouble)
  }

  def calculateNextTemp(): Unit = {
    for (
      x <- 0 until width;
      y <- 0 until height;
      z <- 0 until depth
    ) {
      update(x, y, z, nextPositionTemp(x, y, z))
    }
  }
  
  private def nextPositionTemp(w: Int, h: Int, d: Int): Alloy.Point = {
    val neighbors = for (
        x <- -1 to 1;
        y <- -1 to 1;
        z <- -1 to 1;
        wx = w + x;
        hy = h + y;
        dz = d + z;
        if wx >= 0 && hy >= 0 && dz >= 0 && wx < width && hy < height && dz < depth
      ) yield (wx, hy, dz)

    (for (m <- 0 until 3) yield {
      (for ((x, y, z) <- neighbors) yield {
        val temp = this(x, y, z)
        val p = material(x, y, z)(m)

        temp * p
      }).sum * getConstant(m)
    }).sum / neighbors.size
  }

  def toGNUPlot(): String = {
    val lines = for (
        x <- 0 until width;
        y <- 0 until height;
        z <- 0 until depth
      ) yield x + " " + y + " " + z + " " + points(x)(y)(z)

    lines.mkString("\n")
  }

  def saveGNUPlot(filename: String): Unit = {
    new PrintWriter(filename) {
      try { write(toGNUPlot) }
      finally { close }
    }
  }
}
