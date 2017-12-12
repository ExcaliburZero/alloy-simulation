package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class MainTests extends FlatSpec with Matchers {
  "Main.splitClients" should "work with a single client" in {
    val clients = Array(
      ("one", 1)
    )

    val width = 8
    val height = 5

    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val alloy = Alloy(width, height, 1, materialsDef)

    val clientRanges = Main.splitClients(clients)(alloy)

    clientRanges(clients.head._1) shouldBe DataRange(0, width - 1, width, height, 1, false, false)
  }

  it should "work with two clients" in {
    val clients = Array(
      ("one", 1),
      ("two", 1)
    )

    val width = 8
    val height = 5

    val ratios = (33, 33, 33)
    val materialsDef = new MaterialsDefinition(0.75, 1.0, 1.25, ratios)
    val alloy = Alloy(width, height, 1, materialsDef)

    val clientRanges = Main.splitClients(clients)(alloy)

    clientRanges(clients.head._1) shouldBe DataRange(0, 4, width, height, 1, false, true)
    clientRanges(clients(1)._1) shouldBe DataRange(3, 7, width, height, 1, true, false)
  }
}
