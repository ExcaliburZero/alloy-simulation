package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class MainTests extends FlatSpec with Matchers {
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
}
