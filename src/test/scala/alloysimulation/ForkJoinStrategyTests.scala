package alloysimulation

import org.scalatest.{Assertion, FlatSpec, Matchers}

class ForkJoinStrategyTests extends FlatSpec with Matchers {
  "ForkJoinStrategy" should "be able to be instantiated" in {
    val ratios = (25, 15, 1)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()
    val smallThreshold = 100

    val width = 10
    val height = 5
    val depth = 1

    val alloy = Alloy(width, height, depth, materialsDef)

    val strategy = new ForkJoinStrategy(alloy, iterations, displayFunction,
      smallThreshold)
  }

  it should "be able to run" in {
    val ratios = (25, 15, 1)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val iterations = 50
    val displayFunction = (_: Alloy, _: Int) => ()
    val smallThreshold = 100

    val width = 40
    val height = 20
    val depth = 1

    val alloy = Alloy(width, height, depth, materialsDef)

    val strategy = new ForkJoinStrategy(alloy, iterations, displayFunction,
      smallThreshold)

    strategy.run()
  }
}
