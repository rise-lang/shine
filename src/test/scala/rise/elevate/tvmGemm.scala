package rise.elevate

import elevate.core._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal.default._
import _root_.util.{gen, time, prettyTime, memStats, prettyMem}

// scalastyle:off
class tvmGemm extends test_util.Tests {

  import apps.tvmGemm._

  test("baseline") {
    run("baseline", baseline, openMP = false)
  }

  test("blocking") {
    run("blocking", blocking, openMP = false)
  }

  test("vectorization") {
    run("vectorization", vectorization, openMP = true)
  }

  test("loop permutation") {
    run("loop_permutation", loopPerm, openMP = true)
  }

  test("array packing") {
    run("array_packing", arrayPacking, openMP = true)
  }

  test("cache blocks") {
    run("cache_blocks", cacheBlocks, openMP = true)
  }

  test("parallel") {
    run("parallel", par, openMP = true)
  }


  /// UTILS ////////////////////////////////////////////////////////////////////

  def run(version: String,
          strategy: Strategy[Rise],
          openMP: Boolean // generate C or OpenMP code?
         ): Unit = {

    val generateFiles = false
    val kernelsFolder: String = "/home/artifact/kernels"
    val plotsFolder: String = "/home/artifact/results/fig10/steps"

    def writeToFile(path: String, name: String, content: String, ending: String = ".c"): Unit = {
      import java.io._
      val w = new PrintWriter(new File(s"$path/$name$ending"))
      w.write(content)
      w.flush()
      w.close()
    }

    val versionUC = version.toUpperCase()
    // reset rewrite step counter
    Success.rewriteCount = 0
    elevate.core.applyCount = 0

    // rewrite the matmul input expresssion
    val m1 = memStats().used
    val (rewriteTime1, rewritten1) = time(strategy(mm))
    val m2 = memStats().used
    logger.debug(s"[$versionUC] rewrite time: ${prettyTime(rewriteTime1)}")
    logger.debug(s"[$versionUC] memory use: ${prettyMem(m1 max m2)}")
    logger.debug(s"[$versionUC] required rewrite steps: ${Success.rewriteCount}")
    logger.debug(s"[$versionUC] required rewrite steps (atomic): ${elevate.core.applyCount}\n")

    val (rewriteTime2, rewritten) = time(lowerToC.apply(rewritten1.get))
    logger.debug(s"[$versionUC] rewrite time (lowering): ${prettyTime(rewriteTime2)}")
    logger.debug(s"[$versionUC] required rewrite steps (including lowering): ${Success.rewriteCount}")
    logger.debug(s"[$versionUC] required rewrite steps (including lowering, atomic): ${elevate.core.applyCount}\n")

    val steps = Success.rewriteCount
    if (generateFiles) {
      writeToFile(plotsFolder, version, s"$version,$steps", ".csv")
    }

    // generate the C code
    val (genTime, program) = time(if (openMP) {
      gen.openmp.function(version).asStringFromExpr(rewritten.get)
    } else {
      gen.c.function(version).asStringFromExpr(rewritten.get)
    })
    logger.debug(s"[$versionUC] codegen time: ${prettyTime(genTime)}s")
    logger.debug(s"Program:\n${program}")

    // store the C code
    if (generateFiles) {
      logger.debug(s"[$versionUC] generated code stored as $version in $kernelsFolder")
      writeToFile(kernelsFolder, version, program)
    }
  }
}

