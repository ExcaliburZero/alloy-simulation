package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.PrintWriter

import java.util.Scanner

import java.util.concurrent.CountDownLatch
import java.util.concurrent.Phaser
import java.util.concurrent.TimeUnit

import org.scalatest.{Assertion, FlatSpec, Matchers}

class ClusterProtocolsTests extends FlatSpec with Matchers {
  "ClusterProtocols" should "be able to send and recieve MaterialsDefinitions" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendMaterialsDefinition(output, materialsDef)
    val actual = ClusterClientProtocol.recieveMaterialsDefinition(input)

    actual shouldBe materialsDef
  }

  it should "be able to send and recieve DataRanges" in {
    val range = DataRange(0, 2, 3, 2, 1, false, false)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendDataRange(output, range)
    val actual = ClusterClientProtocol.recieveDataRange(input)

    actual shouldBe range
  }

  it should "be able to send and recieve temperature points" in {
    val range = DataRange(0, 2, 3, 2, 1, false, false)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(2)(0).update(0, -1.34)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendPointsSection(output, range, points)
    val actual = ClusterClientProtocol.recievePointsSection(input)

    actual shouldBe points
  }

  it should "be able to send and recieve materials points" in {
    val range = DataRange(0, 2, 3, 2, 1, false, false)
    val materials = Array.ofDim[Alloy.Point](3, 2, 1, 3)

    materials(0)(1)(0).update(0, 0.24)
    materials(2)(0)(0).update(2, -1.34)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendMaterialsSection(output, range, materials)
    val actual = ClusterClientProtocol.recieveMaterialsSection(input)

    actual shouldBe materials
  }

  it should "be able to send and recieve new temperature values with borders" in {
    val range = DataRange(0, 2, 3, 2, 1, true, true)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterClientProtocol.sendNewTemperatures(output, range, points)
    ClusterServerProtocol.recieveNewTemperatures(input, range,
      pointsDestination)

    // Borders should not be updated
    points(0)(1).update(0, 0.0)
    points(2)(0).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve new temperature values with border above" in {
    val range = DataRange(0, 2, 3, 2, 1, true, false)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterClientProtocol.sendNewTemperatures(output, range, points)
    ClusterServerProtocol.recieveNewTemperatures(input, range,
      pointsDestination)

    // Borders should not be updated
    points(0)(1).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve new temperature values with border below" in {
    val range = DataRange(0, 2, 3, 2, 1, false, true)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterClientProtocol.sendNewTemperatures(output, range, points)
    ClusterServerProtocol.recieveNewTemperatures(input, range,
      pointsDestination)

    // Borders should not be updated
    points(2)(0).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve new temperature values with no borders" in {
    val range = DataRange(0, 2, 3, 2, 1, false, false)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterClientProtocol.sendNewTemperatures(output, range, points)
    ClusterServerProtocol.recieveNewTemperatures(input, range,
      pointsDestination)

    pointsDestination shouldBe points
  }

  it should "be able to send done or continue messages" in {
    val (input, output) = getInOut()

    val expected = true

    ClusterServerProtocol.sendIsDone(output, expected)
    val actual = ClusterClientProtocol.recieveIsDone(input)

    actual shouldBe expected
  }

  it should "be able to send and recieve border temperature values with borders on both sides" in {
    val range = DataRange(0, 2, 3, 2, 1, true, true)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendBorderTemperatures(output, range, points)
    ClusterClientProtocol.recieveBorderTemperatures(input, range,
      pointsDestination)

    // Non-Borders should not be updated
    points(1)(1).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve border temperature values with border above" in {
    val range = DataRange(0, 2, 3, 2, 1, true, false)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendBorderTemperatures(output, range, points)
    ClusterClientProtocol.recieveBorderTemperatures(input, range,
      pointsDestination)

    points(1)(1).update(0, 0.0)
    points(2)(0).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve border temperature values with border below" in {
    val range = DataRange(0, 2, 3, 2, 1, false, true)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendBorderTemperatures(output, range, points)
    ClusterClientProtocol.recieveBorderTemperatures(input, range,
      pointsDestination)

    points(0)(1).update(0, 0.0)
    points(1)(1).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "be able to send and recieve border temperature values with no borders" in {
    val range = DataRange(0, 2, 3, 2, 1, false, false)
    val points = Array.ofDim[Alloy.Point](3, 2, 1)

    points(0)(1).update(0, 0.24)
    points(1)(1).update(0, -2.2)
    points(2)(0).update(0, -1.34)

    val pointsDestination = Array.ofDim[Alloy.Point](3, 2, 1)

    val (input, output) = getInOut()

    ClusterServerProtocol.sendBorderTemperatures(output, range, points)
    ClusterClientProtocol.recieveBorderTemperatures(input, range,
      pointsDestination)

    points(0)(1).update(0, 0.0)
    points(1)(1).update(0, 0.0)
    points(2)(0).update(0, 0.0)

    pointsDestination shouldBe points
  }

  it should "work with a server and client" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)

    val width = 3
    val height = 4
    val depth = 1

    val numGenerations = 10
    val phaser = new Phaser(1)

    val a = Alloy(width, height, depth, materialsDef)
    val b = a.mirror()

    val range = DataRange(0, 2, width, height, depth, true, true)

    val (inputA, outputA) = getInOut()
    val (inputB, outputB) = getInOut()

    val client = new ClusterClientProtocol("rho", inputA, outputB, 16384)
    val server = new ClusterServerProtocol(a, b, numGenerations, range,
      phaser, inputB, outputA)

    val countdown = new CountDownLatch(2)

    onThread(() => {
      client.start()
      countdown.countDown()
    })

    onThread(() => {
      server.start()
      countdown.countDown()
    })

    countdown.await(1, TimeUnit.SECONDS)
  }

  it should "handle borders correctly with multiple clients" in {
    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(1.0, 1.0, 1.0, ratios)

    val width = 8
    val height = 2
    val depth = 1

    val numGenerations = 20

    val a = Alloy(width, height, depth, materialsDef)
    val b = a.mirror()

    a.update(0, 0, 0, 100000.0)

    val clients = Main.splitClients(Array(
      ("rho", 2),
      ("pi", 1)
    ))(a)

    val phaser = new Phaser(clients.size)

    val countdown = new CountDownLatch(clients.size * 2)

    for ((clientName, range) <- clients) {
      onThread(() => {
        val (inputA, outputA) = getInOut()
        val (inputB, outputB) = getInOut()

        val client = new ClusterClientProtocol(clientName, inputA, outputB,
          16384)
        val server = new ClusterServerProtocol(a, b, numGenerations, range,
          phaser, inputB, outputA)

        onThread(() => {
          client.start()
          countdown.countDown()
        })

        onThread(() => {
          server.start()
          countdown.countDown()
        })
      })
    }

    countdown.await(1, TimeUnit.SECONDS)

    // The heated area should be still heated
    assert(b(0, 0, 0) > 10.0)

    // The heat should spread to other client
    assert(b(width - 1, height - 1, 0) > 10.0)
  }

  private def getInOut(): (DataInputStream, DataOutputStream) = {
    val in = new PipedInputStream()
    val out = new PipedOutputStream(in)

    val dataInput = new DataInputStream(in)
    val dataOutput = new DataOutputStream(out)

    (dataInput, dataOutput)
  }

  private def onThread(function: () => Unit): Unit = {
    val thread = new Thread {
      override def run {
        function()
      }
    }

    thread.start()
  }
}
