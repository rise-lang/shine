package idealised.Core

case class TimeSpan[U <: Time.Unit](value: Double, unit: Time.Unit) {
  override def toString: String = value.toString + " " + unit.toString
}

object TimeSpan {
  def inSeconds(value: Double): TimeSpan[Time.s] = TimeSpan(value, Time.s)
  def inMilliseconds(value: Double): TimeSpan[Time.ms] = TimeSpan(value, Time.ms)
  def inMicroseconds(value: Double): TimeSpan[Time.µs] = TimeSpan(value, Time.µs)
  def inNanoseconds(value: Double): TimeSpan[Time.ns] = TimeSpan(value, Time.ns)
}

object Time {
  sealed trait Unit
  case object Second extends Unit { override def toString = "s" }
  case object Millisecond extends Unit { override def toString = "ms" }
  case object Microsecond extends Unit { override def toString = "µs" }
  case object Nanosecond extends Unit { override def toString = "ns" }

  val s = Second
  val ms = Millisecond
  val µs = Microsecond
  val ns = Nanosecond

  type s = Second.type
  type ms = Millisecond.type
  type µs = Microsecond.type
  type ns = Nanosecond.type
}
