package rise.elevate

import elevate.core._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal.default._
import _root_.util.gen

// scalastyle:off
class tvmGemm extends test_util.Tests {
  import apps.tvmGemm._

  test("baseline") {
    run("baseline",  baseline `;` lowerToC, openMP = false)
  }

  test("blocking") {
    run("blocking", blocking `;` lowerToC, openMP = false)
  }

  test("vectorization") {
    run("vectorization", vectorization `;` lowerToC, openMP = true)
  }

  test("loop permutation") {
    run("loop_permutation", loopPerm `;` lowerToC, openMP = true)
  }

  test("array packing") {
    run("array_packing", arrayPacking `;` lowerToC, openMP = true)
  }

  test("cache blocks") {
    run("cache_blocks", cacheBlocks `;` lowerToC, openMP = true)
  }

  test("parallel") {
    run("parallel", par `;` lowerToC, openMP = true)
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
      val w =new PrintWriter(new File(s"$path/$name$ending"))
      w.write(content)
      w.flush()
      w.close()
    }

    def currentTimeSec: Long = System.currentTimeMillis / 1000

    val versionUC = version.toUpperCase()
    // reset rewrite step counter
    Success.rewriteCount = 0

    // rewrite the matmul input expresssion
    val time0 = currentTimeSec
    val rewritten = strategy(mm)
    val time1 = currentTimeSec
    logger.debug(s"[$versionUC] rewrite time: ${time1 - time0}s")
    if (generateFiles) {
      val steps = Success.rewriteCount
      logger.debug(s"[$versionUC] required rewrite steps: $steps\n")
      writeToFile(plotsFolder, version, s"$version,$steps", ".csv")
    }

    // generate the C code
    val time2 = currentTimeSec
    val program = if(openMP) {
      gen.openmp.function(version).asStringFromExpr(rewritten.get)
    } else {
      gen.c.function(version).asStringFromExpr(rewritten.get)
    }
    val time3 = currentTimeSec
    logger.debug(s"[$versionUC] codegen time: ${time3 - time2}s")
    logger.debug(s"Program:\n${program}")

    // store the C code
    if (generateFiles) {
      logger.debug(s"[$versionUC] generated code stored as $version in $kernelsFolder")
      writeToFile(kernelsFolder, version, program)
    }
  }
}
