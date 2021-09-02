package benchmarks.cgo17

import util.withExecutor

object all {
  def main(args: Array[String]): Unit = {
    val stats = withExecutor {
      Seq(
        ("N-Body", nbody.bench()),
        ("MD", molecularDynamics.bench()),
        ("K-Means", kmeans.bench()),
        ("NN", nearestNeighbour.bench()),
        ("MRI-Q", mriQ.bench()),
        ("Convolution", convolution.bench()), // NOTE: original paper adds row and column times
        // ATAX: computed as aggregate from GEMV times
        ("GEMV", gemv.bench()),
        // GESUMMV: computed as aggregate from GEMV times
        ("MM", mm.bench()),
      )
    }

    println("benchmark size kernel medianTime minTime maxTime")
    stats.foreach { case (benchName, benchStats) =>
      benchStats.foreach { case (sizeName, sizeStats) =>
        sizeStats.foreach { case (kernelName, runtime) =>
          println(s""""$benchName" "$sizeName" "$kernelName" ${runtime.median.value} ${runtime.min.value} ${runtime.max.value}""")
        }
      }
    }
  }
}
