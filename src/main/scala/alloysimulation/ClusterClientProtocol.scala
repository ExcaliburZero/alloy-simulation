package alloysimulation

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter

import java.util.Scanner

import java.util.concurrent.locks.ReentrantLock

class ClusterClientProtocol(name: String, input: InputStream,
  output: OutputStream) {
  private var a: Option[Alloy] = None
  private var b: Option[Alloy] = None

  private var closed = false
  private var closedLock = new ReentrantLock()

  private val inputReader = new Scanner(input)
  private val outputWriter = new PrintWriter(output)

  def run(): Unit = {
    recieveInitialData()

    while (!isClosed) {
      calculateNewTemperatures()
      sendNewTemperatures()

      if (isDone()) {
        return
      }

      waitForBorderTemperatures()
      storeBorderTemperatures()

      swapAlloys()
    }
  }

  def close(): Unit = {
    closedLock.lock()
    try {
      closed = true
    } finally {
      closedLock.unlock()
    }
  }

  private def isClosed(): Boolean = {
    closedLock.lock()
    try {
      return closed
    } finally {
      closedLock.unlock()
    }
  }

  private def isDone(): Boolean = {
    ???
  }

  private def recieveInitialData(): Unit = {
    val materialsDef = recieveMaterialsDefinition()
    val points = recievePointsSection()
    val materials = recieveMaterialsSection()

    val width = points.length
    val height = points(0).length
    val depth = points(0)(0).length

    val startWidth, startHeight = 0
    val endWidth = width
    val endHeight = height

    val alloy = Alloy(width, height, depth, materialsDef, points, materials,
      startWidth, startHeight, endWidth, endHeight)

    a = Some(alloy)
    b = Some(alloy.mirror())
  }

  private def recieveMaterialsDefinition(): MaterialsDefinition = {
    val const1 = inputReader.nextDouble()
    val const2 = inputReader.nextDouble()
    val const3 = inputReader.nextDouble()

    val ratios1 = inputReader.nextInt()
    val ratios2 = inputReader.nextInt()
    val ratios3 = inputReader.nextInt()

    val ratios = (ratios1, ratios2, ratios3)

    MaterialsDefinition(const1, const2, const3, ratios)
  }

  private def recievePointsSection(): Array[Array[Array[Alloy.Point]]] = {
    val width = inputReader.nextInt()
    val height = inputReader.nextInt()
    val depth = inputReader.nextInt()

    var points = Array.ofDim[Alloy.Point](width, height, depth)

    for (
      x <- 0 until width;
      y <- 0 until height;
      z <- 0 until depth
    ) {
      points(x)(y).update(z, inputReader.nextDouble())
    }

    points
  }

  private def recieveMaterialsSection(): Array[Array[Array[Array[Double]]]] = {
    val width = inputReader.nextInt()
    val height = inputReader.nextInt()
    val depth = inputReader.nextInt()

    var materials = Array.ofDim[Double](width, height, depth, 3)

    for (
      x <- 0 until width;
      y <- 0 until height;
      z <- 0 until depth;
      m <- 0 until 3
    ) {
      materials(x)(y)(z).update(m, inputReader.nextDouble())
    }

    materials
  }

  private def calculateNewTemperatures(): Unit = {
    ???
  }

  private def sendNewTemperatures(): Unit = {
    ???
  }

  private def waitForBorderTemperatures(): Unit = {
    ???
  }

  private def storeBorderTemperatures(): Unit = {
    ???
  }

  private def swapAlloys(): Unit = {
    val temp = a
    a = b
    b = temp
  }
}
