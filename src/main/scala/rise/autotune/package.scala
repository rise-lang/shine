package rise

import arithexpr.arithmetic.{BoolExpr, RangeAdd, RangeMul, RangeUnknown}
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, KernelExecutor, LocalSize, ScalaFunction, `(`, `)=>`, `,`}
import util.{Time, TimeSpan}

import java.io.{File, FileOutputStream, PrintWriter}
import java.security.Policy.Parameters
import scala.annotation.tailrec

package object autotune {
  type Parameters = Set[NatIdentifier]
  case class Sample(p: Parameters, runtime: Option[TimeSpan[Time.ms]], timestamp: Int)

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, RangeUnknown, isExplicit = true, isTuningParam = true))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, r, isExplicit = true, isTuningParam = true))

  def search(e: Expr): Seq[Sample] = {
    // collect parameters
    val parameters = collectParameters(e)

    // collect constraints
    val constraints = collectConstraints(e, parameters)

    // generate json and write to tmp directory
    val config_file = generateJSON(parameters)

    println("parameters: \n" + parameters)
    println("constraints: \n" + constraints)
    println("json: \n" + config_file)

    // open file
    val file = new PrintWriter(
      new FileOutputStream(new File("autotuning/tmp.json"), false))

    // write to file and close
    file.write(config_file)
    file.close()

    // compute function value as result for hypermapper
    val computeF: (Array[String], Array[String]) => Option[Float] = (header, parametersValues) => {

      val parametersValuesMap = Map.empty[String, Int]
      for(i <- Range(0,header.length)){
        parametersValuesMap + (header(i)->parametersValues(i).toInt)
      }

      checkConstraints(constraints, parametersValuesMap) match{
        case true => execute(substitute(e, parametersValuesMap))
        case false => None
      }
    }

    val hypermapperBinary = os.Path.expandUser("~") / ".local" / "bin" / "hypermapper"

    val configFile = os.pwd / "autotuning" / "tmp.json"

    assert( os.isFile(hypermapperBinary) && os.isFile(configFile) )


    val hypermapper = os.proc(hypermapperBinary, configFile).spawn()

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
            // compute f value
            val f = computeF(header, parametersValues)
            // append response
            f match {
              case None => response += s"${parametersValues.mkString(",")},-1,False\n"
              case Some(value) => response += s"${parametersValues.mkString(",")},$value,True\n"
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


    // options to get results
    // collect elements in store in Seq[Sample]
    // collect best
    // get results from hypermapper .csv output file

    Seq.empty[Sample]
  }


  // check constraints for given values and returns boolean
  def checkConstraints(constraints: Set[Constraint], values:Map[String, Int]):Boolean = ???

  // substitute tp parameters using value Map
  def substitute(e: Expr, values:Map[String, Int]):Expr = ???

    // add information for execution as parameter?
    // execution failure of e returns None
    // execution success of e returns Some(runtimef)
  def execute(e: Expr): Option[Float]  = ???


  def collectParameters(e: Expr): Parameters = {
    var params = scala.collection.mutable.Set[NatIdentifier]()
    traverse(e, new traverse.PureTraversal {
      override def nat: Nat => traverse.Pure[Nat] = n =>
        return_(n.visitAndRebuild({
          case n: NatIdentifier if n.isTuningParam =>
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
        case DepLambda(x: NatIdentifier, e) => iter(e, inputs + x)
        case DepLambda(_, e) => iter(e, inputs)
        case Lambda(_, e) => iter(e, inputs)
        case _ => inputs
      }
    }
    iter(e, Set.empty)
  }

  sealed trait Constraint
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

    traverse(e, new traverse.PureTraversal {
      override def datatype: DataType => traverse.Pure[DataType] = { t =>
        t match {
          case ArrayType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            addPredicate(ArithPredicate(n, 0, ArithPredicate.Operator.>=))
          case VectorType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            cs += RangeConstraint(n, RangeMul(2, 16, 2))
          case _ =>
        }
        super.datatype(t)
      }

      override def nat: Nat => traverse.Pure[Nat] = n =>
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


  def generateJSON(p: Parameters): String = {

    // create header for hypermapper configuration file
    // WARNING: configuration is partially hard coded
    val header =
    """{
      | "application_name" : "rise",
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
      |   "number_of_samples" : 100
      | },
      | "optimization_iterations" : 100,
      | "input_parameters" : {
      |""".stripMargin

    // create entry foreach parameter
    var parameter = ""

    p.foreach(elem => {

      val parameterRange = elem.range match {
        case RangeAdd(start, stop, step) =>  {
          // check if all elements are evaluable
          // Todo check start and stop

          // HACK
          // if step is not evaluable use 1 instead
          val stepWidth = step.isEvaluable match{
            case true => step.eval
            case false => 1
          }

          val x = List.range(start.evalInt, stop.evalInt+1)
          val values = x.filter(_ % stepWidth == 0)

          listToString(values)
        }
        case RangeMul(start, stop, mul) => {
          // check if all elements are evaluable
          // Todo check start and stop

          mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt)/scala.math.log(mul.evalDouble)
              val powers:List[Int] = List.range(start.evalInt, maxVal.toInt+1)
              val values:List[Int] = powers.map(power => scala.math.pow(mul.evalInt, power).toInt)

              listToString(values)
            }
            case false =>
              listToString(List.range(start.evalInt, stop.evalInt+1))
          }
        }
        case _ => {
          println("Not yet implemented")
          ""
        }
      }

      val parameterEntry =
        s"""   "${elem.name}" : {
           |       "parameter_type" : "ordinal",
           |       "values" : ${parameterRange}
           |   },
           |""".stripMargin

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


  def applyBest(e: Expr, samples: Seq[Sample]): Expr = ???

  def listToString(list: List[Int]): String = {

    var valueString = "["
    list.foreach(value => {
      valueString += "" + value + ", "
    })

    var valueStringFinal= valueString.dropRight(2)
    valueStringFinal += "]"

    valueStringFinal
  }
}
