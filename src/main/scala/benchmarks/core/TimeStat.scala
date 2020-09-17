package benchmarks.core

import util.{Time, TimeSpan}

final case class TimeStat[U <: Time.Unit](min: TimeSpan[U], max: TimeSpan[U], median: TimeSpan[U]) {
  override def toString: String = s"$median [$min - $max]"
}
