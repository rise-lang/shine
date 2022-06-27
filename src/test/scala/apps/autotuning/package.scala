package apps

import rise.autotune
import rise.autotune.{AutoTuningError, HostCode, Median, Minimum, Timeouts, Tuner}
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
                     plotOnly: Boolean = false,
                     disableChecking: Boolean = false
                   ) = {

    plotOnly match {
      case true =>
        os.isDir(os.Path(output, os.pwd)) match {
          case true => plotExperiment(name, configFiles, output)
          case false => println("experiment's output does not exist - ignore plotting")
        }
      case false =>
        // run tuning
        for (i <- 1 to iterations) {
          configFiles.foreach(configFile =>
            try {
              runTuning(configFile, output, e, hostCode, inputSizes, strategyMode, executor, disableChecking)
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
                 executor: Option[Expr => (Either[AutoTuningError, Double], Option[Double], Option[Double], Option[Double])],
                 disableCheking: Boolean
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
      runtimeStatistic = Median,
      strategyMode = strategyMode,
      executor = executor,
      saveToFile = true,
      disableChecking = disableCheking
    )

    autotune.search(tuner)(e)
  }

  def plotExperiment(name: String, configs: Seq[String], output: String) = {
    try {


      // parse names from configuration files
      var names = ""
      var foldersPrinting = ""
      var foldersScript = ""
      configs.foreach(config => {
        val name = rise.autotune.configFileGeneration.parseFromJson(config, "application_name")
        names += name + " "
        foldersPrinting += output + "/" + name + "/" + name + "_hm "
        foldersScript += name + "/" + name + "_hm "
      })

      // use first config for printing
      val nameForConfig = rise.autotune.configFileGeneration.parseFromJson(configs(0), "application_name")
      val configPrinting = output + "/" + nameForConfig + "/" + nameForConfig + ".json"
      val configScript = nameForConfig + "/" + nameForConfig + ".json"
      val outputPrinting = s"${output}/${name}"
      val outputScript = s"${name}"

      // plot
      val command: (String, String, String) => String = (config, folders, output) =>
        "hm-plot-optimization-results " +
          s"-j ${config} " +
          "-i " +
          folders +
          "-l " +
          names +
          s"-o ${output}.pdf " +
          "-log " +
          "--y_label \"Log Runtime(ms)\" " +
          s"--title ${name} "

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
      val content = header + command(configScript, foldersScript, outputScript)
      util.writeToPath(uniqueFilepath, content)

      println("command: \n" + command(configPrinting, foldersPrinting, outputPrinting))

      command(configPrinting, foldersPrinting, outputPrinting) !!
    } catch {
      case e: Throwable => println("printing of experiment failed")
    }
  }


}
