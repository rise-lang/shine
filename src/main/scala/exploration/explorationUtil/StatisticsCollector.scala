package exploration.explorationUtil

object ExplorationErrorLevel extends Enumeration {
  type ExplorationErrorLevel = Value
  val StrategyError, LoweringError, CodeGenerationError, CompilationError, ExecutionError, ExecutionTimeout, ExecutionFail, ExecutionSuccess = Value
}


