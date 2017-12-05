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

  private def getInOut(): (DataInputStream, OutputStream) = {
    val in = new PipedInputStream()
    val out = new PipedOutputStream(in)

    val scanner = new DataInputStream(in)

    (scanner, out)
  }
}
