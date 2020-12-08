package shine.OpenCL

sealed trait ParallelismLevel
case object WorkGroup extends ParallelismLevel
case object Global extends ParallelismLevel
case object Local extends ParallelismLevel
case object Sequential extends ParallelismLevel

case object Warp extends ParallelismLevel