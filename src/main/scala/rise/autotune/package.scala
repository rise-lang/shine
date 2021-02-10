package rise

import arithexpr.arithmetic.{RangeAdd, RangeMul, RangeUnknown}
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import util.{Time, TimeSpan}

package object autotune {
  type Parameters = Set[NatIdentifier]
  case class Sample(p: Parameters, runtime: Option[TimeSpan[Time.ms]], timestamp: Int)

  // should we allow tuning params to be substituted during type inference?
  // this could allow to restrict the search space at compile time
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, RangeUnknown, isExplicit = true, isTuningParam = true))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, r, isExplicit = true, isTuningParam = true))

  def search(e: Expr): Seq[Sample] = ???

  def collectParameters(e: Expr): Parameters = {
    var params = scala.collection.mutable.Set[NatIdentifier]()
    class NatVisitor extends traversal.Visitor {
      override def visitNat(n: Nat): traversal.Result[Nat] = {
        n match {
          case n: NatIdentifier if n.isTuningParam =>
            params += n
            traversal.Stop(n)
          case _ =>
            traversal.Stop(n.visitAndRebuild({
              case n: NatIdentifier if n.isTuningParam =>
                params += n
                n
              case ae => ae
            }))
        }
      }
    }
    traversal.DepthFirstLocalResult(e, new NatVisitor {
      override def visitType[T <: Type](t: T): traversal.Result[T] =
        traversal.Stop(traversal.types.DepthFirstLocalResult(t, new NatVisitor))
    })
    params.toSet
  }

  def generateConstraints(p: Parameters): String = {
    // generate constraints as String to be evaluated in python

    var output = ""

    p.foreach(elem => {
      // check if evaluable
      val test = elem.range match{
        case RangeAdd(start, stop, step) => {
          val constraint = step.isEvaluable match {
            case true => ""
            case false => elem.name + " % " + step.toString + " == 0" + " && "
          }

          constraint
        }
        case RangeMul(start, stop, step) => {
          val constraint = step.isEvaluable match {
            case true => ""
            case false => "math.log(" + elem.name + ", "+ step.toString + ") % 1 == 0" + " && "
          }

          constraint
        }
        case _ => // no constraint
      }
      output += test.toString
    })

    output.dropRight(3)
  }

  def generateJSON(p: Parameters): String = {

    // create header for hypermapper configuration file
    // WARNING: configuration is partially hard coded
    val header =
    """{
      | "application_name" : "rise",
      | "optimization_objectives" : ["runtime"],
      | "feasible_output" : {
      |   "enable_feasible_predictor" : true
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
           |       "values" : "${parameterRange}"
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
