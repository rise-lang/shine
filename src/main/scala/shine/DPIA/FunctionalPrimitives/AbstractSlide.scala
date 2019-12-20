package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{ArrayData, Store}
import shine.DPIA.Types._
import shine.DPIA._
import arithexpr.arithmetic.SimplifiedExpr

import scala.language.{postfixOps, reflectiveCalls}
import scala.xml.Elem

abstract class AbstractSlide(n: Nat,
                             sz: Nat,
                             sp: Nat,
                             dt: DataType,
                             input: Phrase[ExpType])
  extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  override val t: ExpType =
    (n: Nat) ->: (sz: Nat) ->: (sp: Nat) ->: (dt: DataType) ->:
      (input :: exp"[$inputSize.$dt, $read]") ->:
        exp"[$n.$sz.$dt, $read]"

  override def eval(s: Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, input) match {
      case ArrayData(arrayE) =>

        def slide[T](sz: Int, sp: Int, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          while (vec.nonEmpty) {
            builder += vec.take(sz)
            vec = vec.drop(sp)
          }
          builder.result()
        }

        ArrayData(slide(sz.eval, sp.eval, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }

  def primitiveName: String = {
    val name = this.getClass.getSimpleName
    Character.toLowerCase(name.charAt(0)) + name.substring(1)
  }

  override def prettyPrint: String =
    s"($primitiveName $sz $sp ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <slide n={ToString(n)} sz={ToString(sz)} sp={ToString(sp)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(input)}
    </slide>.copy(label = primitiveName)
}
