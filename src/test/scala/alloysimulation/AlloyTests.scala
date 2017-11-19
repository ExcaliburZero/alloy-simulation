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

  it should "have no negative temperatures on randomization" in {
    for (_ <- 0 until 10) {
      val (w, h, d) = (50, 80, 100)

      val ratios = (25, 15, 60)
      val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
      val alloy = new Alloy(w, h, d, materialsDef)

      def hasNoNegatives(): Unit = {
        for (
          x <- 0 until w;
          y <- 0 until h;
          z <- 0 until d
        ) {
          val temperature = alloy(x, y, z)

          assert(temperature >= 0.0)
        }
      }

      alloy.randomizeTemps()
      hasNoNegatives()
    }
 }

  it should "have no negative temperatures as generations pass" in {
    for (_ <- 0 until 10) {
      val (w, h, d) = (50, 80, 100)

      val ratios = (25, 15, 60)
      val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
      val alloy = new Alloy(w, h, d, materialsDef)

      def hasNoNegatives(): Unit = {
        for (
          x <- 0 until w;
          y <- 0 until h;
          z <- 0 until d
        ) {
          val temperature = alloy(x, y, z)

          assert(temperature >= 0.0)
        }
      }

      alloy.randomizeTemps()
      hasNoNegatives()

      for (_ <- 0 until 10) {
        alloy.calculateNextTemp()
        hasNoNegatives()
      }
    }
  }

  it should "not change if all temperatures are 0.0" in {
    val (w, h, d) = (5, 8, 10)

    val ratios = (25, 15, 60)
    val materialsDef = new MaterialsDefinition(0.15, 0.21, 0.51, ratios)
    val alloy = new Alloy(w, h, d, materialsDef)

    def hasAllZeros(): Unit = {
      for (
        x <- 0 until w;
        y <- 0 until h;
        z <- 0 until d
      ) {
        val temperature = alloy(x, y, z)

        temperature shouldBe 0.0
      }
    }

    hasAllZeros()

    for (_ <- 0 until 10) {
      alloy.calculateNextTemp()
      hasAllZeros()
    }
  }
}
