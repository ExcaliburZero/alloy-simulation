package alloysimulation

sealed trait StrategyType
case class SingleCore() extends StrategyType
case class ForkJoin() extends StrategyType
case class Cluster() extends StrategyType

abstract class Strategy {
  def run(): Unit
}
