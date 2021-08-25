package rise

import arithexpr.arithmetic.{ArithExpr, RangeUnknown}
import rise.autotune.configFileGeneration._
import rise.autotune.constraints._
import rise.autotune.execution._
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import rise.openCL.DSL.oclRun
import shine.OpenCL.{GlobalSize, LocalSize}
import util.Execute.Exception
import util.{Time, TimeSpan, writeToPath}

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

package object autotune {

  case class Timeouts(codegenerationTimeout: Long, compilationTimeout: Long, executionTimeout: Long)
  case class Tuner(hostCode: HostCode,
                   inputSizes: Seq[Nat] = Seq(),
                   samples: Int = 100,
                   name: String = "RISE",
                   output: String = "autotuning",
                   timeouts: Timeouts = Timeouts(5000, 5000, 5000),
                   executionIterations: Int = 10,
                   speedupFactor: Double = 100,
                   configFile: Option[String] = None,
                   execution: RuntimeStyle = Median,
                   hierarchicalHM: Boolean = false)

  case class Sample(parameters: Map[NatIdentifier, Nat],
                    runtime: Option[TimeSpan[Time.ms]],
                    timestamp: Long,
                    autoTuningError: AutoTuningError,
                    tuningTimes: TuningTimes)

  case class TuningTimes(total: Option[TimeSpan[Time.ms]],
                         codegen: Option[TimeSpan[Time.ms]],
                         compilation: Option[TimeSpan[Time.ms]],
                         execution: Option[TimeSpan[Time.ms]]
                        )

  case class HostCode(init: String, compute: String, finish: String)

  // todo add meta information (configuration, times, samples, ...)
  case class TuningResult(samples: Seq[Sample])
  case class AutoTuningError(errorLevel: AutoTuningErrorLevel, message: Option[String])
  type Parameters = Set[NatIdentifier]

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, RangeUnknown))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, r))

  def search(tuner: Tuner)(e: Expr): TuningResult = {

    val start = System.currentTimeMillis()
    val parameters = collectParameters(e)

    // inject input sizes into constraints
    val inputs = getInputs(e)
    val inputMap = (inputs zip tuner.inputSizes).toMap
    val constraints = collectConstraints(e, parameters)
      .map(constraint => constraint.substitute(inputMap.asInstanceOf[Map[ArithExpr, ArithExpr]]))

    ("mkdir -p " + tuner.output !!)

    // generate json if necessary
    tuner.configFile match {
      case None =>
        println("generate configuration file")
        val file = new PrintWriter(
          new FileOutputStream(
            new File(tuner.output + "/" + tuner.name + ".json"), false))
        file.write(generateJSON(parameters, constraints, tuner))
        file.close()
      case _ => println("use given configuration file")
    }

    println("parameters: \n" + parameters)
    println("constraints: \n" + constraints)

    // compute function value as result for hypermapper
    val computeSample: (Array[String], Array[String]) => Sample = (header, parametersValues) => {
      val totalStart = System.currentTimeMillis()

      val parametersValuesMap: Map[NatIdentifier, Nat] = header.zip(parametersValues).map { case (h, p) =>
        NatIdentifier(h) -> (p.toInt: Nat)
      }.toMap
      if (checkConstraints(constraints, parametersValuesMap)) {
        // execute
        val result = execute(
          rise.core.substitute.natsInExpr(parametersValuesMap.toMap[Nat, Nat], e),
          tuner.hostCode,
          tuner.timeouts,
          tuner.executionIterations,
          tuner.speedupFactor,
          tuner.execution
        )
        val totalTime = Some(TimeSpan.inMilliseconds(
          (System.currentTimeMillis() - totalStart).toDouble)
        )
        Sample(
          parametersValuesMap,
          result.runtime,
          System.currentTimeMillis() - start,
          result.error,
          TuningTimes(
            totalTime, result.codegenTime, result.compilationTime, result.executionTime)
        )
      } else {
        val totalTime = Some(TimeSpan.inMilliseconds((System.currentTimeMillis() - totalStart).toDouble))
        Sample(
          parametersValuesMap,
          None,
          System.currentTimeMillis() - start,
          AutoTuningError(CONSTRAINTS_ERROR, None),
            TuningTimes(totalTime, None, None, None)
          )

      }
    }

    val configFile = tuner.configFile match {
      case Some(filename) =>
        filename.substring(0, 1) match {
          case "/" => os.Path.apply(filename)
          case _ => os.Path.apply(os.pwd.toString() + "/" + filename)
        }
      case None => os.Path.apply(
        os.pwd.toString() + "/" + tuner.output + "/" + tuner.name + ".json"
      )
    }

    println("configFile: " + configFile)

    // check if hypermapper is installed and config file exists
    assert(
      (os.isFile(os.Path.apply("/usr/local/bin/hypermapper"))
        || os.isFile(os.Path.apply("/usr/bin/hypermapper")))
        && os.isFile(configFile)
    )

    val hypermapper = os.proc("hypermapper", configFile).spawn()

    var i = 1
    // main tuning loop
    var samples = new ListBuffer[Sample]()
    var done = false
    while (hypermapper.isAlive() && !done) {
      hypermapper.stdout.readLine() match {
        case null =>
          done = true
          println("End of HyperMapper -- error")
        case "End of HyperMapper" =>
          done = true
          println("End of HyperMapper -- done")
        case "Best point found:" =>
          val headers = hypermapper.stdout.readLine()
          val values = hypermapper.stdout.readLine()
          hypermapper.stdout.readLine() // consume empty line
          println(s"Best point found\nHeaders: ${headers}Values: $values")
        case request if request.contains("warning") =>
          println(s"[Hypermapper] $request")
        case request if request.contains("Request") =>
          println(s"Request: $request")
          val numberOfEvalRequests = request.split(" ")(1).toInt
          // read in header
          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
          // start forming response
          var response = s"${header.mkString(",")},runtime,Valid\n"
          for (_ <- Range(0, numberOfEvalRequests)) {
            // read in parameters values
            val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
            // compute sample (including function value aka runtime)
            print("[" + i.toString + "/" + tuner.samples + "] : ")
            val sample = computeSample(header, parametersValues)
            println(sample.runtime)
            println(sample)
            println(sample.autoTuningError)
            println()
            i += 1
            // append sample to Samples
            samples += sample
            // append response
            sample.runtime match {
              case None => response += s"${parametersValues.mkString(",")},-1,False\n"
              case Some(value) =>
                response += s"${parametersValues.mkString(",")},${value.value},True\n"
            }
          }
          print(s"Response: $response")
          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
        case error => println("error: " + error)
      }
    }

    // save samples to file
    val destination = saveSamples(
      tuner.output + "/" + tuner.name + ".csv",
      TuningResult(samples.toSeq)
    )

    // copy hm result
    ("mv " + tuner.name + "_output_samples.csv " +
      destination.substring(0, destination.length - 4) + "_hm.csv" !!)

    TuningResult(samples.toSeq)
  }

  // wrap ocl run to a function
  def wrapOclRun(localSize: LocalSize, globalSize: GlobalSize)
                (expr: Expr): Expr = {
    expr match {
      // fun(x => e)
      case l@Lambda(x,e) =>
        Lambda(x, wrapOclRun(localSize, globalSize)(e))(l.t)
      // depFun(x => e)
      case dl@DepLambda(kind, x, e) =>
        DepLambda(kind, x, wrapOclRun(localSize, globalSize)(e))(dl.t)
      case e =>
        oclRun(localSize, globalSize)(e)
    }
  }

  def getBest(samples: Seq[Sample]): Option[Sample] = {
    val best = samples.reduceLeft(min)
    best.runtime match {
      case Some(_) => Some(best)
      case None => None
    }
  }

  def applyBest(e: Expr, samples: Seq[Sample]): Expr = {
    val best = getBest(samples)
    best match {
      case Some(_) => rise.core.substitute.natsInExpr(best.get.parameters.toMap[Nat, Nat], e)
      case None => e
    }
  }

  def applySample(e: Expr, sample: Sample): Expr = {
    rise.core.substitute.natsInExpr(sample.parameters.toMap[Nat, Nat], e)
  }

  // write tuning results into csv file
  def saveSamples(path: String, tuningResult: TuningResult): String = {
    // create unique filepath
    val file = new File(path)
    val uniqueFilepath = if (file.exists()) {
      path.substring(0, path.length - 4) + "_" + System.currentTimeMillis() + ".csv"
    } else {
      path
    }

    // write header
    var header = ""
    tuningResult.samples.head.parameters.foreach {
      case (id: NatIdentifier, _:Nat) =>  header += id.name + ","
      case _ => throw Exception("This should not happen")
    }
    header += "runtime" + ","
    header += "timestamp" + ","
    header += "total" + ","
    header += "code generation" + ","
    header += "compilation" + ","
    header += "execution" + ","
    header += "error level" + ","
    header += "error message"
    header += "\n"

    // write content
    var content = ""
    tuningResult.samples.foreach(sample => {

      // write parameter
      sample.parameters.foreach(param =>{
        content += param._2.eval.toString + ","
      })

      // write runtime
      sample.runtime match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }

      // write timestamp
      content += sample.timestamp.toString + ","

      sample.tuningTimes.total match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.codegen match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.compilation match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }
      sample.tuningTimes.execution match {
        case Some(value) => content += value.value.toString + ","
        case None => content += "-1" + ","
      }

      // write error level
      content += sample.autoTuningError.errorLevel.toString + ","

      // write error message
      sample.autoTuningError.message match {
        case Some(value) => content += value + ","
        case None => content += ""
      }

      // finish line
      content += "\n"
    })

    writeToPath(uniqueFilepath, header + content)

    uniqueFilepath
  }

  private def min(s1: Sample, s2: Sample): Sample = {
    s1.runtime match {
      case Some(s1Runtime) =>
        s2.runtime match {
          case Some(s2Runtime) =>
            if (s1Runtime.value < s2Runtime.value) {
              s1
            } else {
              s2
            }
          case None => s1
        }
      case None =>
        s2.runtime match{
          case Some(_) => s2
          case None => s1
        }
    }
  }

  def getInputs(e: Expr): Seq[NatIdentifier] = {
    getInputsRec(Seq.empty[NatIdentifier], e)
  }

  def getInputsRec(inputs: Seq[NatIdentifier], e: Expr): Seq[NatIdentifier] = {
    e match {
      case DepLambda(NatKind, n: NatIdentifier, subexpr) => getInputsRec(inputs :+ n, subexpr)
      case _ => inputs
    }
  }

}
