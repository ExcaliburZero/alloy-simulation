package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter

import java.util.Scanner

import java.util.concurrent.locks.ReentrantLock

object ClusterClientProtocol {
  def recieveMaterialsDefinition(input: InputStream): MaterialsDefinition = {
    withInput(input, i => {
      val const1 = i.readDouble()
      val const2 = i.readDouble()
      val const3 = i.readDouble()

      val ratios1 = i.readInt()
      val ratios2 = i.readInt()
      val ratios3 = i.readInt()

      val ratios = (ratios1, ratios2, ratios3)

      MaterialsDefinition(const1, const2, const3, ratios)
    })
  }

  def recieveDataRange(input: InputStream): DataRange = {
    withInput(input, i => {
      val start = i.readInt()
      val end = i.readInt()
      val width = i.readInt()
      val height = i.readInt()
      val depth = i.readInt()
      val hasAbove = i.readBoolean()
      val hasBelow = i.readBoolean()

      DataRange(start, end, width, height, depth, hasAbove, hasBelow)
    })
  }

  def recievePointsSection(input: InputStream): Alloy.Points = {
    withInput(input, i => {
      val width = i.readInt()
      val height = i.readInt()
      val depth = i.readInt()

      var points = Array.ofDim[Alloy.Point](width, height, depth)

      for (
        x <- 0 until width;
        y <- 0 until height;
        z <- 0 until depth
      ) {
        points(x)(y).update(z, i.readDouble())
      }

      points
    })
  }

  def recieveMaterialsSection(input: InputStream): Alloy.Materials = {
    withInput(input, i => {
      val width = i.readInt()
      val height = i.readInt()
      val depth = i.readInt()

      var materials = Array.ofDim[Double](width, height, depth, 3)

      for (
        x <- 0 until width;
        y <- 0 until height;
        z <- 0 until depth;
        m <- 0 until 3
      ) {
        materials(x)(y)(z).update(m, i.readDouble())
      }

      materials
    })
  }

  def sendNewTemperatures(output: OutputStream, range: DataRange,
    points: Alloy.Points): Unit = {
    withOutput(output, o => {
      val start = range.start
      val end = range.end

      val borderAbove = if (range.hasAbove) 1 else 0
      val borderBelow = if (range.hasBelow) 1 else 0

      val width = end - start + 1 - borderAbove - borderBelow
      val height = range.height
      val depth = range.depth

      for (
        x <- start + borderAbove to end - borderBelow;
        y <- 0 until height;
        z <- 0 until depth
      ) {
        o.writeDouble(points(x)(y)(z))
      }
    })
  }

  private def withOutput[A](output: OutputStream,
    function: (DataOutputStream => A)): A = {
    ClusterServerProtocol.withOutput(output, function)
  }

  private def withInput[A](input: InputStream,
    function: (DataInputStream => A)): A = {
    ClusterServerProtocol.withInput(input, function)
  }
}

class ClusterClientProtocol(name: String, input: InputStream,
  output: OutputStream) {
  private var a: Option[Alloy] = None
  private var b: Option[Alloy] = None
  private var range: Option[DataRange] = None

  private var closed = false
  private var closedLock = new ReentrantLock()

  private val inputReader = new Scanner(input)
  private val outputWriter = new PrintWriter(output)

  def run(): Unit = {
    val (alloy, dr) = recieveInitialData()
    a = Some(alloy)
    b = Some(alloy.mirror())
    range = Some(dr)

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

  private def recieveInitialData(): (Alloy, DataRange) = {
    val materialsDef = ClusterClientProtocol.recieveMaterialsDefinition(
     input) 
    val dataRange = ClusterClientProtocol.recieveDataRange(input)
    val points = ClusterClientProtocol.recievePointsSection(input)
    val materials = ClusterClientProtocol.recieveMaterialsSection(input)

    val width = points.length
    val height = points(0).length
    val depth = points(0)(0).length

    val startWidth, startHeight = 0
    val endWidth = width
    val endHeight = height

    val alloy = Alloy(width, height, depth, materialsDef, points, materials,
      startWidth, startHeight, endWidth, endHeight)

    (alloy, dataRange)
  }

  private def calculateNewTemperatures(): Unit = {
    // TODO(chris): Do this using ForkJoin
    a.get.calculateNextTemp(b.get)
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
