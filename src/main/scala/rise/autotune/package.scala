package rise

import arithexpr.arithmetic.{RangeAdd, RangeUnknown}
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

  def generateJSON(p: Parameters): String = {

    // create header for hypermapper configuration file
    val header =
      """{
        | "application_name" : "rise",
        | "optimization_objectives" : ["runtime"],
        | "feasible_output" : {
        |   "enable_feasible_predictor" : false
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

      // translate range to hypermapper
      // WARNING: currently, no interdependencies possible
      // rangeUnknow to range? PowersOfTwo?

      elem.range match {
        case RangeUnknown => {
          println("range unknow")
          // create what?
          // just an entry?
          // what are the possible values?
        }
        case RangeAdd(start, stop, step) =>  {
          println("range add")
          println("start: " + start)
          println("stop: " + stop)
          println("step: " + step)

          // create filtered list
          val x = List.range(start.evalInt, stop.evalInt+1)
          val list = x.filter(_ % step.evalInt == 0) // step not evaluable
          println("list: " + list)

          // translate list to hypermapper json array
          // rangeAdd to ordinal Array with valid entries
        }
        case _ => println("Not yet implemented")
      }

      val parameterEntry =
        s"""   "${elem.name}" : {
           |       "parameter_type" : "integer",
           |       "values" : "${elem.range}"
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
}
