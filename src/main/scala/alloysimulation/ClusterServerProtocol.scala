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

case class DataRange(start: Int, end: Int, width: Int, height:Int,
  depth: Int, hasAbove: Boolean, hasBelow: Boolean)

object ClusterServerProtocol {
  def sendMaterialsDefinition(output: OutputStream,
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

  private def withOutput[A](output: OutputStream,
    function: (DataOutputStream => A)): A = {
    val out = new DataOutputStream(output)

    try {
      function(out)
    } finally {
      out.flush()
      out.close()
    }
  }
}

class ClusterServerProtocol(private var a: Alloy, private var b: Alloy,
  numGenerations: Int, range: DataRange, phaser: Phaser,
  input: InputStream, output: OutputStream) {
  private var closed: Boolean = false
  private val closedLock = new ReentrantLock()

  private val inputReader = new Scanner(input)
  private val outputWriter = new PrintWriter(output)

  /*private val objectInput = new ObjectInputStream(input)
  private val objectOutput = new ObjectOutputStream(output)*/

  def start(): Unit = {
    sendInitialData()

    while (!isClosed) {
      recieveNewTemperatures()

      waitForOthers()
      if (isDone()) {
        sendDoneMessage()
        return
      } else {
        sendContinueMessage()
      }

      sendBorderTemperatures()

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
    return phaser.getPhase() == numGenerations
  }

  private def sendInitialData(): Unit = {
    ClusterServerProtocol.sendMaterialsDefinition(???, a.materialsDef)
    sendPointsSection()
    sendMaterialsSection()
  }

  /*private def sendMaterialsDefinition(matDef: MaterialsDefinition): Unit = {
    outputWriter.print(matDef.const1)
    outputWriter.print(matDef.const2)
    outputWriter.print(matDef.const3)

    outputWriter.print(matDef.ratios._1)
    outputWriter.print(matDef.ratios._2)
    outputWriter.print(matDef.ratios._3)
  }*/

  private def sendPointsSection(): Unit = {
    // TODO(chris): Add handling for borders

    outputWriter.print(range.width)
    outputWriter.print(range.height)
    outputWriter.print(range.depth)

    for (
      x <- 0 until range.width;
      y <- 0 until range.height;
      z <- 0 until range.depth
    ) {
      outputWriter.print(a(x, y, z))
    }
  }

  private def sendMaterialsSection(): Unit = {
    outputWriter.print(range.width)
    outputWriter.print(range.height)
    outputWriter.print(range.depth)

    for (
      x <- 0 until range.width;
      y <- 0 until range.height;
      z <- 0 until range.depth
    ) {
      outputWriter.print(a.material(x, y, z))
    }
  }

  private def recieveNewTemperatures(): Unit = {
    ???
  }

  private def waitForOthers(): Unit = {
    phaser.arriveAndAwaitAdvance()
  }

  private def sendDoneMessage(): Unit = {
    ???
  }

  private def sendContinueMessage(): Unit = {
    ???
  }

  private def sendBorderTemperatures(): Unit = {
    ???
  }

  private def swapAlloys(): Unit = {
    val temp = a
    a = b
    b = temp
  }
}
