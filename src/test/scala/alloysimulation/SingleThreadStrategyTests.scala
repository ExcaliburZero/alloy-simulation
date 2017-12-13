package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class SingleThreadStrategyTests extends FlatSpec with Matchers {
  "SingleThreadStrategy" should "be able to be instantiated" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()

    val width = 10
    val height = 5
    val depth = 1

    val alloy = Alloy(width, height, depth, materialsDef, 0)

    val strategy = new SingleThreadStrategy(alloy, iterations, displayFunction)
  }

  it should "be able to run" in {
    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()

    val width = 10
    val height = 5
    val depth = 1

    val alloy = Alloy(width, height, depth, materialsDef, 0)

    val strategy = new SingleThreadStrategy(alloy, iterations, displayFunction)

    strategy.run()
  }
}
