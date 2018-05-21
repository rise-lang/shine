package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{CodeGenerator, RewriteToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.SplitAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Split(n: Nat,
                       m: Nat,
                       dt: DataType,
                       array: Phrase[ExpType])
  extends ExpPrimitive {

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[${m * n}.$dt]") -> exp"[$m.$n.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Split(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(arrayE) =>

        def split[T](n: Nat, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          for (i <- 0 until vector.length / n.eval) {
            val (head, tail) = vec splitAt n.eval
            vec = tail
            builder += head
          }
          builder.result()
        }

        ArrayData(split(n, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(split $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <split n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </split>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    acc(array)(SplitAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(array)(λ(exp"[${m * n}.$dt]")(x => C(Split(n, m, dt, x)) ))
  }
}