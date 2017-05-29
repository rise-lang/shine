package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.VisitAndRebuild.Visitor
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.{postfixOps, reflectiveCalls}
import scala.xml.Elem

final case class Gather(n: Nat,
                        dt: DataType,
                        idxF: Phrase[`(nat)->`[ExpType -> ExpType]],
                        array: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = exp"[$n.$dt]"

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    idxF match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (dt: DataType) ->
          (idxF :: t"($l : nat) -> exp[idx($l)] -> exp[idx($l)]") ->
          (array :: exp"[$n.$dt]") ->
          `type`
      case _ => throw new Exception("This should not happen")
    }
  }

  override def eval(s: Store): Data = {
//    import idealised.DPIA.Semantics.OperationalSemantics._
//    val idxFE = OperationalSemantics.eval(s, idxF)
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(a) =>
//        val res = new Array[Data](a.length)
//        for (i <- a.indices) {
//          res(i) = a(OperationalSemantics.evalIndexExp(s, idxFE(i)).eval)
//        }
//        ArrayData(res.toVector)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def visitAndRebuild(fun: Visitor): Phrase[ExpType] =
    Gather(fun(n), fun(dt), VisitAndRebuild(idxF, fun), VisitAndRebuild(array, fun))

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(this)(λ(exp"[$n.$dt]")(x => acc(x)(A) ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(array)(λ(exp"[$n.$dt]")(x => C(Gather(n, dt, idxF, x)) ))
  }

  override def prettyPrint: String = s"(gather idxF ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <gather>
      <idxF>{Phrases.xmlPrinter(idxF)}</idxF>
      <input>{Phrases.xmlPrinter(array)}</input>
    </gather>
}
