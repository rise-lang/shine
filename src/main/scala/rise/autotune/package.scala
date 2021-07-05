package rise

import arithexpr.arithmetic.{RangeUnknown}
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, writeToPath}
import java.io.{File, FileOutputStream, PrintWriter}
import rise.openCL.DSL.oclRun
import scala.collection.mutable.ListBuffer

import rise.autotune.constraints._
import rise.autotune.configFileGeneration._
import rise.autotune.execution._

import scala.language.postfixOps
import scala.sys.process._

package object autotune {

  case class Timeouts(codgenerationTimeout: Long, compilationTimeout: Long, executionTimeout: Long)
  case class Tuner(main: String,
                   iterations: Int = 100,
                   name: String = "RISE",
                   output: String = "autotuning",
                   timeouts: Timeouts = Timeouts(5000, 5000, 5000),
                   configFile: Option[String] = None,
                   hierarchicalHM: Boolean = false)

  case class Sample(parameters: Map[NatIdentifier, Nat],
                    runtime: Option[TimeSpan[Time.ms]],
                    timestamp: Long,
                    autoTuningError: AutoTuningError)

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
    val constraints = collectConstraints(e, parameters)
    ("mkdir -p " + tuner.output !!)

    // generate json if necessary
    tuner.configFile match {
      case None => {
        println("generate configuration file")
        val file = new PrintWriter(
          new FileOutputStream(
            new File(tuner.output + "/" + tuner.name + ".json"), false))
        file.write(generateJSON(parameters, constraints, tuner))
        file.close()
      }
      case _ => println("use given configuration file")
    }

    println("parameters: \n" + parameters)
    println("constraints: \n" + constraints)

    // compute function value as result for hypermapper
    val computeSample: (Array[String], Array[String]) => Sample = (header, parametersValues) => {
      val parametersValuesMap = header.zip(parametersValues).map { case (h, p) =>
        NatIdentifier(h) -> (p.toInt: Nat)
      }.toMap
      checkConstraints(constraints, parametersValuesMap) match {
        case true => {
          println("constraints true")
          // execute
          val result = execute(
            rise.core.substitute.natsInExpr(parametersValuesMap, e), tuner.main, tuner.timeouts)
          result._1 match {
            case Some(_) =>
              Sample(parametersValuesMap, result._1, System.currentTimeMillis() - start, result._2)
            case None =>
              Sample(parametersValuesMap, result._1, System.currentTimeMillis() - start, result._2)
          }
        }
        case false => {
          println("constraints false")
          Sample(
            parametersValuesMap,
            None,
            System.currentTimeMillis() - start,
            AutoTuningError(CONSTRAINTS_ERROR, None)
          )
        }
      }
    }

    val configFile = tuner.configFile match {
      case Some(filename) => os.Path.apply(filename)
      case None => os.pwd / tuner.output / (tuner.name + ".json")
    }

    println("configFile: " + configFile)

    // check if hypermapper is installed and config file exists
    assert(
      (os.isFile(os.Path.apply("/usr/local/bin/hypermapper"))
        || os.isFile(os.Path.apply("/usr/bin/hypermapper")))
        && os.isFile(configFile)
    )

    val hypermapper = os.proc("hypermapper", configFile).spawn()

    var i = 0
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
          println(s"Best point found\nHeaders: ${headers}Values: ${values}")
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
            val sample = computeSample(header, parametersValues)
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
      case e => oclRun(localSize, globalSize)(e)
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
      case Some(_) => rise.core.substitute.natsInExpr(best.get.parameters, e)
      case None => e
    }
  }

  def applySample(e: Expr, sample: Sample): Expr = {
    rise.core.substitute.natsInExpr(sample.parameters, e)
  }

  // write tuning results into csv file
  def saveSamples(path: String, tuningResult: TuningResult): Unit = {
    // write header
    var header = ""
    tuningResult.samples(0).parameters.foreach(param => {
      header += param._1.name + ","
    })
    header += "runtime" + ","
    header += "error level" + ","
    header += "error message" + ","
    header += "timestamp"
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

      // write error level
      content += sample.autoTuningError.errorLevel.toString + ","

      // write error message
      sample.autoTuningError.message match {
        case Some(value) => content += value.toString + ","
        case None => content += "" + ","
      }

      // write timestamp
      content += sample.timestamp

      // finish line
      content += "\n"
    })

    writeToPath(path, header + content)
  }

  private def min(s1: Sample, s2: Sample): Sample = {
    s1.runtime match {
      case Some(s1Runtime) => {
        s2.runtime match {
          case Some(s2Runtime) => {
            s1Runtime.value < s2Runtime.value match {
              case true => s1
              case false => s2
            }
          }
          case None => s1
        }
      }
      case None => {
        s2.runtime match{
          case Some(_) => s2
          case None => s1
        }
      }
    }
  }
}
