package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class AlloyTests extends FlatSpec with Matchers {
  "Alloy" should "be able to be instantiated" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val alloy = new Alloy(5, 8, 10, materialsDef)
  }

  it should "be able to calculate the next temperatures" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val alloy = new Alloy(5, 8, 10, materialsDef)

    alloy.calculateNextTemp()
  }

  it should "be able to be converted to a GNU plot definition" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val alloy = new Alloy(1, 2, 3, materialsDef)
    
    val expected = Array(
      "0 0 0 0.0",
      "0 0 1 0.0",
      "0 0 2 0.0",
      "0 1 0 0.0",
      "0 1 1 0.0",
      "0 1 2 0.0"
    ).mkString("\n")
    val actual = alloy.toGNUPlot()

    actual shouldBe expected
  }

  it should "be able to randomize its temperature values" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val alloy = new Alloy(5, 8, 10, materialsDef)

    alloy.randomizeTemps()
  }
}
