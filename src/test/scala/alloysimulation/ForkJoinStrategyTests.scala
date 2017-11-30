package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class ForkJoinStrategyTests extends FlatSpec with Matchers {
  "ForkJoinStrategy" should "be able to be instantiated" in {
    val ratios = (25, 15, 1)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()
    val smallThreshold = 100

    val strategy = new ForkJoinStrategy(5, 8,10, materialsDef,
      iterations, displayFunction, smallThreshold)
  }

  it should "be able to run" in {
    val ratios = (25, 15, 1)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()
    val smallThreshold = 100

    val strategy = new ForkJoinStrategy(5, 8,10, materialsDef,
      iterations, displayFunction, smallThreshold)

    strategy.run()
  }
}
