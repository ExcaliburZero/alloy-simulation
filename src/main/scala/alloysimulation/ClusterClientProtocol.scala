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

  private def withInput[A](input: InputStream,
    function: (DataInputStream => A)): A = {
    val in = new DataInputStream(input)

    try {
      function(in)
    } finally {
      in.close()
    }
  }
}

class ClusterClientProtocol(name: String, input: InputStream,
  output: OutputStream) {
  private var a: Option[Alloy] = None
  private var b: Option[Alloy] = None

  private var closed = false
  private var closedLock = new ReentrantLock()

  private val inputReader = new Scanner(input)
  private val outputWriter = new PrintWriter(output)

  def run(): Unit = {
    val alloy = recieveInitialData()
    a = Some(alloy)
    b = Some(alloy.mirror())

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

  private def recieveInitialData(): Alloy = {
    val materialsDef = ClusterClientProtocol.recieveMaterialsDefinition(
      ???)
    val points = ClusterClientProtocol.recievePointsSection(???)
    val materials = recieveMaterialsSection()

    val width = points.length
    val height = points(0).length
    val depth = points(0)(0).length

    val startWidth, startHeight = 0
    val endWidth = width
    val endHeight = height

    Alloy(width, height, depth, materialsDef, points, materials,
      startWidth, startHeight, endWidth, endHeight)
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
