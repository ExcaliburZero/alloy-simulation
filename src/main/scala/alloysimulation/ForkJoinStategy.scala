package alloysimulation

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinTask
import java.util.concurrent.RecursiveTask

class ForkJoinStrategy(alloy: Alloy, iterations: Int,
  displayFunction: Alloy.DisplayFunction,
  smallThreshold: Int) extends Strategy {

  private val alloy2 = alloy.mirror()

  private val forkJoinPool = new ForkJoinPool(Runtime.getRuntime.availableProcessors())

  def run(): Unit = {
    for (i <- 0 until iterations) {
      val (a, b) = if (i % 2 == 0) {
        (alloy, alloy2)
      } else {
        (alloy2, alloy)
      }

      displayFunction(a, i)

      val task = new CustomTask(smallThreshold, a, b)

      forkJoinPool.invoke(task)
    }
  }
}

class CustomTask(smallThreshold: Int, a: Alloy, b: Alloy) extends RecursiveTask[Unit] {
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
      new CustomTask(smallThreshold, newAs(0), newBs(0)),
      new CustomTask(smallThreshold, newAs(1), newBs(1))
    )

    ForkJoinTask.invokeAll(subtasks.head, subtasks(1))
  }
}
