package benchmarks

import util.{Time, TimeSpan}

package object core {
  def benchmark[R, U <: Time.Unit](sampleCount: Int, run: => TimeSpan[U]): TimeStat[U] = {
    val sorted_samples = (0 until sampleCount).map(_ => run).sorted
    val min = sorted_samples.head
    val max = sorted_samples.last
    val med = sorted_samples(sorted_samples.length / 2)
    TimeStat(min, max, med)
  }
}
