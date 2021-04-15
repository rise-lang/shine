package benchmarks

import util.{Time, TimeSpan}

package object core {
  def hostedBenchmark(sampleCount: Int,
                      buffer_impl: String,
                      init: String,
                      compute: String,
                      finish: String,
                      module: shine.OpenCL.Module,
                     ): TimeStat[Time.s] = {
    val code =
      s"""
         |${util.gen.opencl.hosted.asString(module)}
         |
         |int main() {
         |  assertReasonableTimeResolution();
         |  ${init}
         |  for (int sample = 0; sample < ${sampleCount}; sample++) {
         |    Instant start = now();
         |    ${compute}
         |    Instant stop = now();
         |    printf("sample: %lf\\n", elapsedSeconds(start, stop));
         |  }
         |  ${finish}
         |}
         |""".stripMargin
    timeStat(util.ExecuteOpenCL(code, buffer_impl).linesIterator.flatMap { line =>
      if (line.startsWith("sample: ")) {
        Some(TimeSpan.inSeconds(line.stripPrefix("sample: ").trim.toDouble))
      } else {
        None
      }
    }.toSeq)
  }

  def benchmark[U <: Time.Unit](sampleCount: Int, run: => TimeSpan[U]): TimeStat[U] =
    timeStat((0 until sampleCount).map(_ => run))

  def timeStat[U <: Time.Unit](samples: Seq[TimeSpan[U]]): TimeStat[U] = {
    val sorted_samples = samples.sorted
    val min = sorted_samples.head
    val max = sorted_samples.last
    val med = sorted_samples(sorted_samples.length / 2)
    TimeStat(min, max, med)
  }
}
