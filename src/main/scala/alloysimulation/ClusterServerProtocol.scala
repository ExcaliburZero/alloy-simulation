package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
//import java.io.ObjectInputStream
//import java.io.ObjectOutputStream

import java.util.Scanner

import java.util.concurrent.Phaser
import java.util.concurrent.locks.ReentrantLock

case class DataRange(start: Int, end: Int, width: Int, height: Int,
  depth: Int, hasAbove: Boolean, hasBelow: Boolean)

object ClusterServerProtocol {
  def sendMaterialsDefinition(output: DataOutputStream,
    matDef: MaterialsDefinition): Unit = {
    withOutput(output, o => {
      o.writeDouble(matDef.const1)
      o.writeDouble(matDef.const2)
      o.writeDouble(matDef.const3)

      o.writeInt(matDef.ratios._1)
      o.writeInt(matDef.ratios._2)
      o.writeInt(matDef.ratios._3)
    })
  }

  def sendDataRange(output: DataOutputStream, range: DataRange): Unit = {
    withOutput(output, o => {
      o.writeInt(range.start)
      o.writeInt(range.end)
      o.writeInt(range.width)
      o.writeInt(range.height)
      o.writeInt(range.depth)
      o.writeBoolean(range.hasAbove)
      o.writeBoolean(range.hasBelow)
    })
  }

  def sendPointsSection(output: DataOutputStream, range: DataRange,
    points: Alloy.Points): Unit = {
    // TODO(chris): Add handling for borders
    val start = range.start
    val end = range.end

    val width = end - start + 1
    val height = range.height
    val depth = range.depth

    withOutput(output, o => {
      o.writeInt(width)
      o.writeInt(height)
      o.writeInt(depth)

      for (
        x <- start to end;
        y <- 0 until height;
        z <- 0 until depth
      ) {
        o.writeDouble(points(x)(y)(z))
      }
    })
  }

  def sendMaterialsSection(output: DataOutputStream, range: DataRange,
    materials: Alloy.Materials): Unit = {
    val start = range.start
    val end = range.end

    val width = end - start + 1
    val height = range.height
    val depth = range.depth

    withOutput(output, o => {
      o.writeInt(width)
      o.writeInt(height)
      o.writeInt(depth)

      for (
        x <- start to end;
        y <- 0 until height;
        z <- 0 until depth;
        m <- 0 until 3
      ) {
        o.writeDouble(materials(x)(y)(z)(m))
      }
    })
  }

  def recieveNewTemperatures(input: DataInputStream, range: DataRange,
    points: Alloy.Points): Unit = {
    val start = range.start
    val end = range.end

    val borderAbove = if (range.hasAbove) 1 else 0
    val borderBelow = if (range.hasBelow) 1 else 0

    val width = end - start + 1 - borderAbove - borderBelow
    val height = range.height
    val depth = range.depth

    withInput(input, i => {
      for (
        x <- start + borderAbove to end - borderBelow;
        y <- 0 until height;
        z <- 0 until depth
      ) {
        points(x)(y).update(z, i.readDouble())
      }
    })
  }

  def sendIsDone(output: DataOutputStream, isDone: Boolean): Unit = {
    withOutput(output, o => {
      o.writeBoolean(isDone)
    })
  }

  def sendBorderTemperatures(output: DataOutputStream, range: DataRange,
    points: Alloy.Points): Unit = {
    withOutput(output, o => {
      if (range.hasAbove) {
        val x = range.start
        for (
          y <- 0 until range.height;
          z <- 0 until range.depth
        ) {
          o.writeDouble(points(x)(y)(z))
        }
      }

      if (range.hasBelow) {
        val x = range.end
        for (
          y <- 0 until range.height;
          z <- 0 until range.depth
        ) {
          o.writeDouble(points(x)(y)(z))
        }
      }
    })
  }

  def withInput[A](input: DataInputStream,
    function: (DataInputStream => A)): A = {
    try {
      function(input)
    } finally {
      //input.close()
    }
  }

  def withOutput[A](output: DataOutputStream,
    function: (DataOutputStream => A)): A = {
    try {
      function(output)
    } finally {
      output.flush()
      //out.close()
      // TODO: Move this
    }
  }
}

class ClusterServerProtocol(private var a: Alloy, private var b: Alloy,
  numGenerations: Int, range: DataRange, phaser: Phaser,
  input: InputStream, output: OutputStream) {
  private var closed: Boolean = false
  private val closedLock = new ReentrantLock()

  private val dataInput = new DataInputStream(input)
  private val dataOutput = new DataOutputStream(output)

  def start(): Unit = {
    try {
      sendInitialData()

      while (!isClosed) {
        ClusterServerProtocol.recieveNewTemperatures(dataInput, range, a.points)

        waitForOthers()
        if (isDone()) {
          ClusterServerProtocol.sendIsDone(dataOutput, true)
          return
        } else {
          ClusterServerProtocol.sendIsDone(dataOutput, false)
        }

        ClusterServerProtocol.sendBorderTemperatures(dataOutput, range, a.points)

        swapAlloys()
      }
    } finally {
      dataInput.close()
      dataOutput.close()
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
    return phaser.getPhase() == numGenerations
  }

  private def sendInitialData(): Unit = {
    ClusterServerProtocol.sendMaterialsDefinition(dataOutput, a.materialsDef)
    ClusterServerProtocol.sendDataRange(dataOutput, range)
    ClusterServerProtocol.sendPointsSection(dataOutput, range, a.points)
    ClusterServerProtocol.sendMaterialsSection(dataOutput, range, a.materials)
  }

  private def waitForOthers(): Unit = {
    phaser.arriveAndAwaitAdvance()
  }

  private def swapAlloys(): Unit = {
    val temp = a
    a = b
    b = temp
  }
}
