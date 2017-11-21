package alloysimulation

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.RecursiveTask

class ForkJoinStrategy(width: Int, height: Int, depth: Int,
  materialsDef: MaterialsDefinition, iterations: Int,
  displayFunction: Alloy.DisplayFunction,
  smallThreshold: Int) extends Strategy {

  val alloy = Alloy(width, height, depth, materialsDef)
  val alloy2 = alloy.mirror()

  alloy.randomizeTemps()

  alloy.update(0, 0, 0, 1000000)
  alloy.update(width - 1, height - 1, 0, 1000000)

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

      alloy.update(0, 0, 0, 1000000)
      alloy.update(width - 1, height - 1, 0, 1000000)
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
