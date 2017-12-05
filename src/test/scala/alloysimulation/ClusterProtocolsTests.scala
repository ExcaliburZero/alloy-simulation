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

  private def getInOut(): (DataInputStream, OutputStream) = {
    val in = new PipedInputStream()
    val out = new PipedOutputStream(in)

    val scanner = new DataInputStream(in)

    (scanner, out)
  }
}
