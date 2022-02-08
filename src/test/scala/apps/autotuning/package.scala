package apps

import rise.autotune
import rise.autotune.{HostCode, Minimum, Timeouts, Tuner}
import rise.core.Expr
import rise.core.types.Nat

import scala.language.postfixOps
import scala.sys.process._

package object autotuning {

  def runExperiment(configFiles: Seq[String], iterations: Int, output: String, e: Expr, hostCode: HostCode, inputSizes: Seq[Nat]) = {

    // run tuning
    for(i <- 1 to iterations) {
      configFiles.foreach(configFile => runTuning(configFile, output, e, hostCode, inputSizes))
    }

    // plot experiments
    plotExperiment(configFiles, output)
  }

  def runTuning(configFile: String, output: String, e: Expr, hostCode: HostCode, inputSizes: Seq[Nat]) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    val tuner = Tuner(
      hostCode = hostCode,
      inputSizes = inputSizes,
      samples = 20, // defined by config file, value is ignored
      name = version,
      output = s"${output}/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(e)
  }

  def plotExperiment(configs: Seq[String], output: String) = {

    // parse names from configuration files
    var names = ""
    var folders = ""
    configs.foreach(config => {
      val name = rise.autotune.configFileGeneration.parseFromJson(config, "application_name")
      names += name + " "
      folders += output + "/" + name + "/" + name + "_hm "
    })

    // plot
    val command = "hm-plot-optimization-results " +
      "-j autotuning/config/mm/rs_cot_1024.json " +
      "-i " +
      folders +
      "-l " +
      names +
      "-o autotuning/mm_1024_test/mmTuning.pdf " +
      "-log --y_label \"Log Runtime(ms)\" "  +
      "--title MM "

    command !!
  }



}
