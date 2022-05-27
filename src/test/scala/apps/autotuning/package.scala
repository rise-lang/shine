package apps

import rise.autotune
import rise.autotune.{AutoTuningError, HostCode, Minimum, Timeouts, Tuner}
import rise.core.Expr
import rise.core.types.Nat

import java.io.File
import scala.language.postfixOps
import scala.sys.process._

package object autotuning {

  def runExperiment(
                     name: String,
                     configFiles: Seq[String],
                     iterations: Int,
                     output: String,
                     e: Expr,
                     hostCode: HostCode,
                     inputSizes: Seq[Nat],
                     strategyMode: Option[(Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr]] = None, // enable strategy mode
                     executor: Option[Expr => (Either[AutoTuningError, Double], Option[Double], Option[Double], Option[Double])] = None, // todo change this to exeuction result
                     plotOnly: Boolean = false
                   ) = {

    plotOnly match {
      case true => plotExperiment(name, configFiles, output)
      case false =>
        // run tuning
        for (i <- 1 to iterations) {
          configFiles.foreach(configFile =>
            try {
              runTuning(configFile, output, e, hostCode, inputSizes, strategyMode, executor)
            } catch {
              case e: Throwable => println("tuning failed for configFile: " + configFile)
            }
          )

          // plot experiments after each iteration of all configs
          plotExperiment(name, configFiles, output)
        }
    }
  }

  def runTuning(
                 configFile: String,
                 output: String,
                 e: Expr,
                 hostCode: HostCode,
                 inputSizes: Seq[Nat],
                 strategyMode: Option[(Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr]],
                 executor: Option[Expr => (Either[AutoTuningError, Double], Option[Double], Option[Double], Option[Double])]
               ) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    // todo pass strategy mode through
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
      strategyMode = strategyMode,
      executor = executor,
      saveToFile = true
    )

    autotune.search(tuner)(e)
  }

  def plotExperiment(name: String, configs: Seq[String], output: String) = {
    try {


      // parse names from configuration files
      var names = ""
      var folders = ""
      configs.foreach(config => {
        val name = rise.autotune.configFileGeneration.parseFromJson(config, "application_name")
        names += name + " "
        folders += output + "/" + name + "/" + name + "_hm "
      })

      // use first config for printing
      val name = rise.autotune.configFileGeneration.parseFromJson(configs(0), "application_name")
      val configPrinting = output + "/" + name + "/" + name + ".json"
      val configScript = name + "/" + name + ".json"

      // plot
      val command: String => String = config =>
        "hm-plot-optimization-results " +
          s"-j ${config} " +
          "-i " +
          folders +
          "-l " +
          names +
          s"-o ${output}/${name}.pdf " +
          "-log " +
          "--y_label \"Log Runtime(ms)\" " +
          s"--title ${name} "

      println("plot: \n" + command)

      // create unique filepath
      val path = output + "/" + "plot_hm.sh"
      val file = new File(path)
      val uniqueFilepath = if (file.exists()) {
        val timeAppendix = System.currentTimeMillis().toString
        path.substring(0, path.length - 3) + "_" + timeAppendix + ".sh"
      } else {
        path
      }

      // write plotting script
      val header = "#!/bin/bash\n"
      val content = header + command(configScript)
      util.writeToPath(uniqueFilepath, content)

      command(configPrinting) !!
    } catch {
      case e: Throwable => println("printing of experiment failed")
    }
  }


}
