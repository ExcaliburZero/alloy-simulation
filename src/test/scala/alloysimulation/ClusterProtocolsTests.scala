package alloysimulation

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.PrintWriter

import java.util.Scanner

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

  it should "be able to send and recieve new temperature values" in {
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

  it should "be able to send done or continue messages" in {
    val (input, output) = getInOut()

    val expected = true

    ClusterServerProtocol.sendIsDone(output, expected)
    val actual = ClusterClientProtocol.recieveIsDone(input)

    actual shouldBe expected
  }

  private def getInOut(): (DataInputStream, OutputStream) = {
    val in = new PipedInputStream()
    val out = new PipedOutputStream(in)

    val scanner = new DataInputStream(in)

    (scanner, out)
  }
}
