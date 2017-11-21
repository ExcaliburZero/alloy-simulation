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
  var points = Array.ofDim[Alloy.Point](width, height, depth)
  var materials = Array.ofDim[Double](width, height, depth, 3)

  private var startWidth = 0
  private var startHeight = 0
  private var endWidth = width
  private var endHeight = height

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

  def updateMaterial(x: Int, y: Int, z: Int, value: Alloy.Material): Unit = {
    materials(x)(y).update(z, value)
  }

  private def randomMaterial(): Alloy.Material = {
    val p1 = Math.min(1.0, materialsDef.percent1 + (scala.util.Random.nextDouble() * 50.0) - 25.0)
    val p2 = Math.min(1.0, materialsDef.percent2 + (scala.util.Random.nextDouble() * 50.0) - 25.0)
    val p3 = Math.min(1.0, materialsDef.percent3 + (scala.util.Random.nextDouble() * 50.0) - 25.0)

    val total = p1 + p2 + p3

    //List(p1 / total, p2 / total, p3 / total).map(Math.abs(_)).toArray
    //List(0.333, 0.333, 0.333).toArray
    List(0.0, 1.0, 0.0).toArray
  }

  def mirror(): Alloy = {
    val other = new Alloy(width, height, depth, materialsDef)

    for (
      x <- 0 until width;
      y <- 0 until height;
      z <- 0 until depth
    ) {
      other.updateMaterial(x, y, z, material(x, y, z))
    }

    other
  }

  def randomizeTemps(): Unit = {
    var stdTemp = 255.0 / 2.5

    val r = scala.util.Random
    val lines = for (
        x <- 0 until width;
        y <- 0 until height;
        z <- 0 until depth
      ) yield update(x, y, z, stdTemp * Math.abs(r.nextGaussian()))
  }

  def calculateNextTemp(other: Alloy): Unit = {
    for (
      x <- startWidth until endWidth;
      y <- startHeight until endHeight;
      z <- 0 until depth
    ) {
      other.update(x, y, z, nextPositionTemp(x, y, z))
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

    val bs = for (m <- 0 until 3) yield {
      val as = for ((x, y, z) <- neighbors) yield {
        val temp = this(x, y, z)
        val p = material(x, y, z)(m)

        temp * p
      }

      as.sum * getConstant(m)
    }

    bs.sum / neighbors.size
  }

  def setSubDimensions(sw: Int, sh: Int, ew: Int, eh: Int): Unit = {
    startWidth = sw
    startHeight = sh
    endWidth = ew
    endHeight = eh
  }

  def getWorkLoad(): Int = {
    (endWidth - startWidth) * (endHeight - startHeight)
  }

  def split(): List[Alloy] = {
    val numPieces = 2
    val pieceSize = (endWidth - startWidth) / numPieces

    (for (i <- 0 until numPieces) yield {
      val subAlloy = new Alloy(width, height, depth, materialsDef)
      subAlloy.points = points
      subAlloy.materials = materials

      val sw = startWidth + pieceSize * i
      val sh = startHeight
      val ew = endWidth - (pieceSize * (numPieces - (i + 1)))
      val eh = endHeight
      subAlloy.setSubDimensions(sw, sh, ew, eh)

      subAlloy
    }).toList
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

  def toRGBBitmap(): RGBBitmap = {
    val rgbBitmap = new RGBBitmap(width, height)

    for (
      x <- 0 until width;
      y <- 0 until height
    ) {
      rgbBitmap.updateColor(x, y, (this(x, y, 0) + 1.0).toInt, 0, 0)
    }

    rgbBitmap
  }
}
