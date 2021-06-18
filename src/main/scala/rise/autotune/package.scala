package rise

import arithexpr.arithmetic.ArithExpr.{collectVars, freeVariables}
import arithexpr.arithmetic.{ArithExpr, BoolExpr, RangeAdd, RangeMul, RangeUnknown, Var}
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, gen, monads, writeToPath}

import java.io.{File, FileOutputStream, PrintWriter}
import arithexpr.arithmetic.BoolExpr.ArithPredicate
import rise.openCL.DSL.oclRun

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.typeOf


package object autotune {

  // error levels
  sealed trait ErrorLevel
  case object NO_ERROR extends ErrorLevel
  case object CONSTRAINTS_ERROR extends ErrorLevel
  case object SUBSTITUTION_ERROR extends ErrorLevel
  case object CODE_GENERATION_ERROR extends ErrorLevel // how to check?
  case object COMPILATION_ERROR extends ErrorLevel // error code?
  case object EXECUTION_ERROR extends ErrorLevel // e.g. crash, 124 (timeout), -1 correctness?

  // tuner, result and tuning errors
  case class Tuner(main:String,
                   iterations: Int = 100,
                   name:String = "RISE",
                   output:String = "autotuning",
                   configFile:Option[String] = None,
                   hierarchicalHM: Option[String] = None)

  case class TuningResult(samples: Seq[Sample]) // todo add meta information (configuration, times, samples, ...)
  case class AutoTuningError(errorLevel: ErrorLevel, message:Option[String])

  // parameters and sample
  type Parameters = Set[NatIdentifier]
  case class Sample(parameters: Map[NatIdentifier, Nat], runtime: Option[TimeSpan[Time.ms]], timestamp: Long, autoTuningError: AutoTuningError)

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, RangeUnknown))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(TuningParameter(name, r))

  // main search method using custom tuner
  def search(tuner:Tuner)(e:Expr): TuningResult = {
    // timestamp of starting point
    val start = System.currentTimeMillis()

    // collect parameters
    val parameters = collectParameters(e)

    // collect constraints
    val constraints = collectConstraints(e, parameters)

    // generate json if necessary
    tuner.configFile match {
      case None => {
        // open file
        val file = new PrintWriter(new FileOutputStream(new File(tuner.output + "/" + tuner.name + ".json"), false))

        // write to file and close
        file.write(generateJSON(parameters, constraints, tuner))
        file.close()
      }
      case _ => println("no generation of json")
    }

    println("parameters: \n" + parameters)
    println("constraints: \n" + constraints)
//    println("json: \n" + config_file)

    // compute function value as result for hypermapper
    val computeSample: (Array[String], Array[String]) => Sample = (header, parametersValues) => {
      val parametersValuesMap = header.zip(parametersValues).map { case (h, p) =>
        NatIdentifier(h) -> (p.toInt: Nat)
      }.toMap


      checkConstraints(constraints, parametersValuesMap) match {
        case true => {
          // execute
          val result = execute(rise.core.substitute.natsInExpr(parametersValuesMap, e), tuner.main)

          result._1 match {
            case Some(_) =>
              // true
              Sample(parametersValuesMap, result._1, System.currentTimeMillis() - start, result._2)
            case None =>
              // false
              Sample(parametersValuesMap, result._1, System.currentTimeMillis() - start, result._2)
          }
        }
        // constraints error
        case false => {
          Sample(parametersValuesMap, None, System.currentTimeMillis() - start, AutoTuningError(CONSTRAINTS_ERROR, None))
        }
      }
    }

    val hypermapperBinary = tuner.hierarchicalHM match {
      case Some(path) => os.Path.apply(path)
      case None => {
        // todo change this to environment variable

        // check, where hypermapper was installed
        val hypermapperBin = os.Path.expandUser("~") / ".local" / "bin" / "hypermapper"
        os.isFile(hypermapperBin) match {
          case true => hypermapperBin
          case false => os.Path.apply("/usr/local/bin/hypermapper")
        }
      }
    }

    // todo make sure filename can be relative path
    val configFile = tuner.configFile match {
      case Some(filename) => os.Path.apply(filename)
      case None => os.pwd / tuner.output / (tuner.name + ".json")
    }

    // todo adjust this check
    println("configFile: \n " + configFile)
    println("HMBinary: \n " + hypermapperBinary)
    assert( os.isFile(hypermapperBinary) && os.isFile(configFile) )

    // spawn hypermapper process
    val hypermapper = os.proc(hypermapperBinary, configFile).spawn()
//    val hypermapper = os.proc("python", hypermapperBinary, configFile).spawn()

    // create output Seq
    var samples = new ListBuffer[Sample]()

    var done = false
    while (hypermapper.isAlive() && !done) {
      hypermapper.stdout.readLine() match {
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
        case request =>
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
              case Some(value) => response += s"${parametersValues.mkString(",")},${value.value},True\n"
            }
          }
          print(s"Response: $response")
          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
      }
    }


    // delete tmp json file
//    val fileDelete = new File("autotuning/tmp.json")
//    fileDelete.delete()

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
      case dl@DepLambda(kind, x, e) => DepLambda(kind, x, wrapOclRun(localSize, globalSize)(e))(dl.t)
      case e => oclRun(localSize, globalSize)(e)
    }
  }

  // check constraints for given values and returns boolean
  def checkConstraints(constraints: Set[Constraint], values: Map[NatIdentifier, Nat]): Boolean = {
    val map = values.asInstanceOf[Map[ArithExpr, ArithExpr]]
    constraints.forall(c => c.substitute(map).isSatisfied())
  }

  def execute(e: Expr, main: String, timeout: Long = 5000): (Option[TimeSpan[Time.ms]], AutoTuningError)  = {
    val m = util.runWithTimeout(timeout)(gen.opencl.hosted.fromExpr(e))
    m match {
      case Some(_) =>{
        val program = shine.OpenCL.Module.translateToString(m.get) + main

        // execute program
        util.ExecuteOpenCL.executeWithRuntime(program, "zero_copy")
      }
      case None => (None, AutoTuningError(CODE_GENERATION_ERROR, Some("timeout after: " + timeout)))
    }
  }


  def collectParameters(e: Expr): Parameters = {
    var params = scala.collection.mutable.Set[NatIdentifier]()
    traverse.traverse(e, new traverse.PureTraversal {
      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild({
          case n@TuningParameter() =>
            params += n
            n
          case ae => ae
        }))
    })
    params.toSet
  }

  private def collectInputNats(e: Expr): Set[NatIdentifier] = {
    @tailrec
    def iter(e: Expr, inputs: Set[NatIdentifier]): Set[NatIdentifier] = {
      e match {
        case DepLambda(NatKind, x: NatIdentifier, e) => iter(e, inputs + x)
        case DepLambda(_, _, e) => iter(e, inputs)
        case Lambda(_, e) => iter(e, inputs)
        case _ => inputs
      }
    }
    iter(e, Set.empty)
  }

  private def isPowerOf(a: Int, b: Int): Boolean = {
    val p: Int = Math.round(Math.log(a) / Math.log(b)).toInt
    Math.round(Math.pow(b, p)) == a
  }

  sealed trait Constraint {
    def substitute(map: Map[ArithExpr, ArithExpr]): Constraint = this match {
      case PredicateConstraint(n) =>
        PredicateConstraint(n.substitute(map).getOrElse(n))
      case RangeConstraint(n, r) =>
        RangeConstraint(n.substitute(map).getOrElse(n), r.substitute(map).getOrElse(r))
    }

    def isSatisfied(): Boolean = this match {
      case PredicateConstraint(n: ArithPredicate) =>
        n.evaluate.contains(true)
      // TODO: it feels like checking if a nat is inside a range should be part of arithexpr
      case RangeConstraint(n, RangeAdd(start, stop, step)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        n % step == (0: Nat)
      case RangeConstraint(n, RangeMul(start, stop, mul)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
        isPowerOf(n.eval, mul.eval)
      case _ =>
        throw new Exception(s"no support for checking $this")
    }
  }
  case class PredicateConstraint(n: BoolExpr) extends Constraint {
    override def toString: String = n.toString
  }
  case class RangeConstraint(n: Nat, r: arithexpr.arithmetic.Range) extends Constraint {
    override def toString: String = s"($n) in $r"
  }

  // we only look at constraints on top-level nats
  def collectConstraints(e: Expr, parameters: Parameters): Set[Constraint] = {
    import arithexpr.arithmetic._
    import BoolExpr._

    val paramOrInput = (parameters ++ collectInputNats(e)).map(_.name)
    val cs = collection.mutable.Set[Constraint]()

    def addPredicate(p: ArithPredicate): Unit = {
      if (!p.evaluate.contains(true)) {
        cs += PredicateConstraint(p)
      }
    }

    traverse.traverse(e, new traverse.PureTraversal {
      override def expr: Expr => monads.Pure[Expr] = { e =>
        e match {
          case DepApp(NatKind, DepApp(NatKind, DepApp(NatKind, DepApp(NatKind, DepApp(NatKind, DepApp(NatKind,
            rise.openCL.primitives.oclRunPrimitive(),
            ls0: Nat), ls1: Nat), ls2: Nat),
            gs0: Nat), gs1: Nat), gs2: Nat)
          =>
            for (s <- Seq(ls0, ls1, ls2, gs0, gs1, gs2)) {
              addPredicate(ArithPredicate(s, 1, ArithPredicate.Operator.>=))
            }
            for ((ls, gs) <- Seq((ls0, gs0), (ls1, gs1), (ls2, gs2))) {
              cs += RangeConstraint(gs, RangeAdd(0, PosInf, ls))
            }
          case _ =>
        }
        super.expr(e)
      }

      override def datatype: DataType => monads.Pure[DataType] = { t =>
        t match {
          case ArrayType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            addPredicate(ArithPredicate(n, 1, ArithPredicate.Operator.>=))
          case VectorType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            cs += RangeConstraint(n, RangeMul(2, 16, 2))
          case _ =>
        }
        super.datatype(t)
      }

      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild { m =>
          if (m.varList.forall(v => paramOrInput(v.name))) { m match {
            case Prod(parts) =>
              var (num, denum) = parts.partition {
                case Pow(_, Cst(-1)) => false
                case _ => true
              }
              denum = denum.map { case Pow(b, Cst(-1)) => b }
              if (denum.nonEmpty) { // num /^ denum
                val aNum = num.fold(1: ArithExpr)(_ * _)
                val aDenum = denum.fold(1: ArithExpr)(_ * _)
                if (aNum % aDenum != Cst(0)) {
                  cs += RangeConstraint(aNum, RangeAdd(0, PosInf, aDenum))
                }
              }
            case Mod(x, _) =>
              addPredicate(ArithPredicate(x, 0, ArithPredicate.Operator.>=))
            case _ => ()
          }}
          m
        })
    })

    cs.toSet
  }


  def generateJSON(p: Parameters, c:Set[Constraint], tuner: Tuner): String = {
    //    def generateJSON(p: Parameters, tuner: Tuner): String = {


    // distinguish wether to generate constraints or not
    tuner.hierarchicalHM match {
      case Some(value) => // generate constraints
      case None =>  // generate without constraints
    }

    val parametersWDC = distributeConstraints(p, c)

    val doe = p.size * 10
    // create header for hypermapper configuration file
    // WARNING: configuration is partially hard coded
    val header =
    s"""{
       | "application_name" : "${tuner.name}",
       | "optimization_objectives" : ["runtime"],
       | "hypermapper_mode" : {
       |   "mode" : "client-server"
       | },
       | "feasible_output" : {
       |   "enable_feasible_predictor" : true,
       |   "name" : "Valid",
       |   "true_value" : "True",
       |   "false_value" : "False"
       | },
       | "design_of_experiment" : {
       |   "doe_type" : "random sampling",
       |   "number_of_samples" : ${doe}
       | },
       | "optimization_iterations" : ${tuner.iterations},
       | "input_parameters" : {
       |""".stripMargin

    // create entry foreach parameter
    var parameter = ""

    p.foreach(elem => {

      println("elem: " + elem)
      println("range: " + elem.range)

      val parameterRange = elem.range match {

        // case step = 1
        case RangeAdd(start, stop, step) =>  {
          //check if all elements are evaluable
          // Todo check start and stop

          // HACK
          // if step is not evaluable use 1 instead
          val stepWidth = step.isEvaluable match{
            case true => step.eval
            case false => 1
          }

          // use integer range then

//          val x = List.range(start.evalInt, stop.evalInt+1)
//          val values = x.filter(_ % stepWidth == 0)

          //          listToString(values)

          start.eval match {
            case 0 => valuesListToString(List(start.evalInt+1, stop.evalInt))
            case _ => valuesListToString(List(start.evalInt, stop.evalInt))
          }
        }

        // case mul = 1
        case RangeMul(start, stop, mul) => {
          // check if all elements are evaluable
          // Todo check start and stop

          mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt)/scala.math.log(mul.evalDouble)
              val powers:List[Int] = List.range(start.evalInt, maxVal.toInt+1)
              val values:List[Int] = powers.map(power => scala.math.pow(mul.evalInt, power).toInt)

              //              listToString(values)
              valuesListToString(List(start.evalInt, stop.evalInt))
            }
            case false =>
              //              listToString(List.range(start.evalInt, stop.evalInt+1))
              valuesListToString(List(start.evalInt, stop.evalInt))
          }
        }
        case _ => {
          println("Not yet implemented")
          ""
        }
      }

      //
      val parameterEntry = tuner.hierarchicalHM match {
        case Some(_) => {
          // use constraints


      // get dependencies and constraints from map
      val dependencies = parametersWDC(elem)._2.size match {
        case 0 => "[]"
        case _ =>  elementListToString(parametersWDC(elem)._2.toList)
      }

      val constraints = parametersWDC(elem)._1.size match {
        case 0 => "[]"
        case _ => {
          val constraintsList = new ListBuffer[String]
          parametersWDC(elem)._1.foreach(constraint => {
            // check type of constraint
            val constraintString = constraint match {
              case RangeConstraint(n, r) => {
                val (start, stop, step) = r match {
                  case RangeAdd(start, stop, step) => (start, stop, step)
                  case RangeMul(start, stop, step) => (start, stop, step)
                  case _ => (0, 0, 0) // todo catch other types of ranges
                }

                // if stop is PosInf, remove constraint (already catched by the range of parameter)
                stop.toString match {
                  case "PosInf" =>{
                    val startConstraint = n.toString + " >= " + start
                    val stepConstraint = n.toString + " % " + step + " == 0"

                    startConstraint + " and " + stepConstraint
                  }
                  case _ => {
                    val startConstraint = n.toString + " >= " + start
                    val stopConstraint = n.toString + " <= " + stop
                    val stepConstraint = n.toString + " % " + step + " == 0"

                    startConstraint + " and " + stopConstraint + " and " + stepConstraint
                  }
                }

              }
              case PredicateConstraint(n) => {
                n.toString.contains("/^") match {
                  case true => constraint.toString.replace("/^", "/")
                  case false => constraint.toString
                }
              }
            }
            constraintsList += constraintString
          })
          elementListToString(constraintsList.filter(elem => elem.size != 0).toList)
        }
      }

      val parameterEntry =
        s"""   "${elem.name}" : {
           |       "parameter_type" : "integer",
           |       "values" : ${parameterRange},
           |       "constraints" : ${constraints},
           |       "dependencies" : ${dependencies}
           |   },
           |""".stripMargin


          parameterEntry
        }
        case None => {
          // don't use constraints

          val parameterEntry =
            s"""   "${TuningParameterName(elem)}" : {
               |       "parameter_type" : "integer",
               |       "values" : ${parameterRange}
               |   },
               |""".stripMargin


          parameterEntry
        }
      }
      parameter += parameterEntry
    })

    // remove last comma
    val parameterSection = parameter.dropRight(2) + "\n"

    val foot =
      """ }
        |}
        |""".stripMargin

    header + parameterSection + foot
  }

  def applyBest(e: Expr, samples: Seq[Sample]): Expr = {
    val best = getBest(samples)
    best match {
      case Some(_) => rise.core.substitute.natsInExpr(best.get.parameters, e)
      case None => e // maybe throw exception?
    }
  }

  def applySample(e: Expr, sample: Sample): Expr = {
    rise.core.substitute.natsInExpr(sample.parameters, e)
  }

  def getBest(samples: Seq[Sample]): Option[Sample] = {
    val best = samples.reduceLeft(min)
    best.runtime match {
      case Some(_) => Some(best)
      case None => None
    }
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

  def valuesListToString(list: List[Any]):String = {
      var valuesString = ""
      list.foreach(value => {
        valuesString += value.toString +  ", "
      })
      "["  + valuesString.dropRight(2) + "]"
  }

  def elementListToString(list: List[Any]):String = {
    var valuesString = ""
    list.foreach(value => {
      valuesString += "\"" + value.toString + "\"" + ", "
    })
    "["  + valuesString.dropRight(2) + "]"
  }


  //  def TuningResult(drop)

  // check path and duplicates above

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

  // write tuning results meta information to file
  def saveMeta(path: String, tuningResult: TuningResult):Unit = {
    // currently no meta data available
  }


  def distributeConstraints(parameters: Parameters, constraints: Set[Constraint]): Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])] =  {

    // initialize output map and add parameters
    val parametersWDC = scala.collection.mutable.Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])]()
    parameters.foreach(param => {
      parametersWDC(param) = (Set.empty[Constraint], Set.empty[NatIdentifier])
    })

    // get parameters from constraint
    // check for given parameters in the given constraint
    constraints.foreach(constraint => {
      //      println("constraint: " + constraint)
      val parametersInConstraint = getParametersFromConstraint(parameters, constraint)
      //      println("parametersInConstraint: " + parametersInConstraint)
      //      println("\n")

      // check if constraint is type of range
      constraint match {
        case RangeConstraint(n, r) => {
          // order this constraint to n
          val candidates = parametersInConstraint.filter(param => param.name.equals(n.toString))

          candidates.size match {
            case 0 => {
              // we do not stop if candidate is found!
              parametersInConstraint.foreach(candidate => {

                // check if pointer occurs in other parameters' dependencies  (avoid cycles)
                parametersWDC.filter(paramWDC => !(paramWDC._1.name.equals(candidate.name))).exists(paramWDC => {
                  paramWDC._2._2.exists(dependency => candidate.name.equals(dependency.name))
                }) match {
                  case false => {
                    // use this candidate
                    // add candidate to output map
                    val elem = parametersWDC(candidate)
                    parametersWDC(candidate) = (elem._1 + constraint, elem._2 ++ parametersInConstraint.filter(param => !(param.name.equals(candidate.name))))
                  }
                  case true => // use next candidate
                }
              })
            }
            case _ => {
              val candidate = candidates.last

              val elem = parametersWDC(candidate)
              parametersWDC(candidate) = (elem._1 + constraint, elem._2 ++ parametersInConstraint.filter(param => !(param.name.equals(candidate.name))))
              // should go to n
            }
          }
        }
        case _ => {
          // iterate over candidates
          //  true: next candidate
          //  false: add constraint and other parameters to candidate parameter
          // we do not stop if candidate is found!
          parametersInConstraint.foreach(candidate => {

            // check if pointer occurs in other parameters' dependencies  (avoid cycles)
            parametersWDC.filter(paramWDC => !(paramWDC._1.name.equals(candidate.name))).exists(paramWDC => {
              paramWDC._2._2.exists(dependency => candidate.name.equals(dependency.name))
            }) match {
              case false => {
                // use this candidate
                // add candidate to output map
                val elem = parametersWDC(candidate)
                parametersWDC(candidate) = (elem._1 + constraint, elem._2 ++ parametersInConstraint.filter(param => !(param.name.equals(candidate.name))))
              }
              case true => // use next candidate
            }
          })
        }
      }
    })
    parametersWDC.toMap
  }


  def getParametersFromConstraint(parameters: Parameters, constraint: Constraint):Set[NatIdentifier] = {

    // consider kind of constraints here

    // min max should be part of
    // constraints with only one element?

    // collect parameters as vars in constraint
    val parametersInConstraint = constraint match {
      case RangeConstraint(n, r) => {
        val rangeVars = r match {
          case RangeAdd(start, stop, step) => {
            start.varList ++ stop.varList ++ step.varList
          }
          case RangeMul(start, stop, mul) => {
            start.varList ++ stop.varList ++ mul.varList
          }
          case _ => List.empty[Var]
        }
        (n.varList ++ rangeVars).toSet
      }
      case PredicateConstraint(n) => {
        n.freeVariables()
      }
    }
//    println("constraint: " + constraint)
//    println("parametersInConstraint: " + parametersInConstraint)

    // get occurring parameters form parameter list
    val output = parameters.filter(nat => {
      parametersInConstraint.exists(paramInConstraint => {
        paramInConstraint.name.equals(nat.name)
      })
    })

    output
  }

}
