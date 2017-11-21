package alloysimulation

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.RecursiveTask

class ForkJoinStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction,
  smallThreshold: Int) extends Strategy {

  val alloy = Alloy(width, height, depth, materialsDef)
  val alloy2 = alloy.mirror()

  stampPattern()
  stampDots()

  private def stampDots(): Unit = {
    val temperature = 10000

    for (
      a <- List(1, 9);
      b <- List(1, 9)
    ) {
      val x = a * (width / 10)
      val y = b * (height / 10)

      alloy.update(x.toInt, y.toInt, 0, temperature * (a + b))
    }
  }

  private def stampPattern(): Unit = {
    val temperature = 7000

    val widthBrush = width / 10
    val heightBrush = width / 10

    val boundary = ((width / 2) * 0.8).toInt
    val numRings = 12
    val interval = boundary / numRings

    val centerX = width / 2
    val centerY = height / 2
    for (
      r <- 1 until boundary;
      if r % interval == 0
    ) {
      val numRotations = r / interval * 2
      val rotationDeg = 360 / numRotations

      for (d <- 0 until numRotations) {
        val points = for(i <- 0 until 6) yield {
          (centerX + r * Math.cos(d * rotationDeg + i * 2 * Math.PI / 6),
           centerY + r * Math.sin(d * rotationDeg + i * 2 * Math.PI / 6))
        }

        for ((x, y) <- points) {
          alloy.update(x.toInt, y.toInt, 0, temperature)
        }
      }
    }
  }

  def run(): Unit = {
    for (i <- 0 until iterations) {
      val (a, b) = if (i % 2 == 0) {
        (alloy, alloy2)
      } else {
        (alloy2, alloy)
      }

      displayFunction(a, i)

      val forkJoinPool = new ForkJoinPool()
      val task = new ForkJoinTask(smallThreshold, a, b)

      forkJoinPool.invoke(task)

      //alloy.update(0, 0, 0, 1000000)
      //alloy.update(width - 1, height - 1, 0, 1000000)
    }
  }
}

class ForkJoinTask(smallThreshold: Int, a: Alloy, b: Alloy) extends RecursiveTask[Unit] {
  val workLoad = a.getWorkLoad()

  override protected def compute(): Unit = {
    if (workLoad > smallThreshold) {
      splitTask()
    } else {
      doWork()
    }
  }

  private def doWork(): Unit = {
    a.calculateNextTemp(b)
  }

  private def splitTask(): Unit = {
    val newAs = a.split()
    val newBs = b.split()

    val subtasks = List(
      new ForkJoinTask(smallThreshold, newAs(0), newBs(0)),
      new ForkJoinTask(smallThreshold, newAs(1), newBs(1))
    )

    for(subtask <- subtasks) {
      subtask.fork()
    }

    for(subtask <- subtasks) {
      subtask.join()
    }
  }
}
