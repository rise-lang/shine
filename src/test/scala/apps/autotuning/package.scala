package apps

import exploration.explorationUtil.jsonParser.readFile
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
                     disableChecking: Boolean = false,
                     expert: Option[Map[Nat, Nat]] = None,
                     default: Option[Map[Nat, Nat]] = None
                   ) = {

    plotOnly match {
      case true =>
        os.isDir(os.Path(output, os.pwd)) match {
          case true => {

            // if expert -> read from output
            val expertConfiguration = expert match {
              case Some(_) => readConfig(output + "/manual_configs/" + "expert.csv") // read in from output
              case None => None // do nothing
            }

            // if default -> read from output
            val defaultConfiguration = default match {
              case Some(_) => readConfig(output + "/manual_configs/" + "default.csv") // read in from output
              case None => None // do nothing
            }

            plotExperiment(name, configFiles, output, expertConfiguration, defaultConfiguration)
          }
          case false => println("experiment's output does not exist - ignore plotting")
        }
      case false =>

        // if expert -> read from output
        val expertConfiguration = expert match {
          case Some(config) => runConfig(config, e, hostCode, output, "expert")
          case None => None // do nothing
        }

        // if default -> read from output
        val defaultConfiguration = default match {
          case Some(config) => runConfig(config, e, hostCode, output, "default")
          case None => None // do nothing
        }

        // run tuning
        for (i <- 1 to iterations) {
          configFiles.foreach(configFile =>
            try {
              runTuning(configFile, output, e, hostCode, inputSizes, strategyMode, executor, disableChecking)
            } catch {
              case e: Throwable => println("tuning failed for configFile: " + configFile)
            }
          )

          configFiles.size match {
            case 0 => // ignore
            case _ =>
              // plot experiments after each iteration of all configs
              plotExperiment(name, configFiles, output, expertConfiguration, defaultConfiguration)
          }
        }
    }
  }

  // read config values from output
  def readConfig(
                  filePath: String
                ): Option[Double] = {

    val file = new File(filePath)
    val output = if (file.exists()) {
      Some(readFile(filePath).toDouble)
    } else {
      None
    }

    output
  }

  def runConfig(
                 config: Map[Nat, Nat],
                 e: Expr,
                 hostCode: HostCode,
                 output: String,
                 file: String
               ): Option[Double] = {

    val eReplaced = rise.core.substitute.natsInExpr(config, e)

    // todo merge this information and information from runTuning
    val result = autotune.execution.execute(
      expression = eReplaced,
      hostCode = hostCode,
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result: " + result.runtime)

    val configResult = result.runtime match {
      case Left(_) => None
      case Right(value) => Some(value.value)
    }

    // create output directory
    (s"mkdir -p ${output}/manual_configs" !!)

    // write result into file
    val path = output + "/manual_configs/" + file + ".csv"
    util.writeToPath(path, configResult.get.toString)

    // todo print config as well

    configResult
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

  def plotExperiment(
                      name: String,
                      configs: Seq[String],
                      output: String,
                      expertConfiguration: Option[Double] = None,
                      defaultConfiguration: Option[Double] = None
                    ) = {
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
      val commands: (String, String, String, Option[Double], Option[Double]) => Seq[String] = (config, folders, output, expertConfiguration, defaultConfiguration) => {

        val command: String =
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

        val commandExp: String = expertConfiguration match {
          case Some(value) =>
            "hm-plot-optimization-results " +
              s"-j ${config} " +
              "-i " +
              folders +
              "-l " +
              names +
              s"-o ${output}_expert.pdf " +
              "-log " +
              "--y_label \"Log Runtime(ms)\" " +
              s"--exp ${value} " +
              s"--title ${name} "

          case None => ""

        }

        val commandDefault: String = defaultConfiguration match {
          case Some(value) =>
            "hm-plot-optimization-results " +
              s"-j ${config} " +
              "-i " +
              folders +
              "-l " +
              names +
              s"-o ${output}_default.pdf " +
              "-log " +
              "--y_label \"Log Runtime(ms)\" " +
              s"--exp ${value} " +
              s"--title ${name} "
          case None => ""
        }

        Seq(command, commandExp, commandDefault)
      }

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
      val content = header + commands(configScript, foldersScript, outputScript, expertConfiguration, defaultConfiguration).mkString("\n")
      util.writeToPath(uniqueFilepath, content)

      println("command: \n" + commands(configPrinting, foldersPrinting, outputPrinting, expertConfiguration, defaultConfiguration))

      commands(configPrinting, foldersPrinting, outputPrinting, expertConfiguration, defaultConfiguration).foreach(command => {
        command !!
      })

    } catch {
      case e: Throwable => println("printing of experiment failed")
    }
  }
}
