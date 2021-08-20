package rise.autotune

sealed trait AutoTuningErrorLevel

case object NO_ERROR extends AutoTuningErrorLevel
case object CONSTRAINTS_ERROR extends AutoTuningErrorLevel
case object SUBSTITUTION_ERROR extends AutoTuningErrorLevel
case object CODE_GENERATION_ERROR extends AutoTuningErrorLevel
case object COMPILATION_ERROR extends AutoTuningErrorLevel
case object EXECUTION_ERROR extends AutoTuningErrorLevel

case class AutoTuningError(errorLevel: AutoTuningErrorLevel, message: Option[String])