package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.{IterateIAcc, IterateIExp}

import scala.xml.Elem

final case class Iterate(n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->`[ExpType -> ExpType]],
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type`: ExpType = exp"[${m /^ n.pow(k)}.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (f :: t"($l : nat) -> exp[$l.$dt] -> exp[${l /^ n}.$dt]") ->
          (array :: exp"[$m.$dt]") ->
          `type`
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Iterate(fun(n), fun(m), k, fun(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
//    val fE = OperationalSemantics.eval(s, f)
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(xs) =>
//        var a = array
//        for (i <- 0 until k.eval) {
//          a = fE(a)
//        }
//        OperationalSemantics.eval(s, a)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l -> (ExpType(ArrayType(l, dt)) -> ExpType(ArrayType(l /^ n, dt))))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.RewriteToImperative._

    con(array)(λ(exp"[$m.$dt]")(x =>
      IterateIAcc(n, m = m /^ n.pow(k), k, dt, A,
        _Λ_(l => λ(acc"[${l /^ n}.$dt]")(o => λ(exp"[$l.$dt]")(x => acc(f(l)(x))(o)))),
        x) ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.RewriteToImperative._

    con(array)(λ(exp"[$m.$dt]")(x =>
      IterateIExp(n, m = m /^ n.pow(k), k, dt, C,
        _Λ_(l => λ(acc"[${l /^ n}.$dt]")(o => λ(exp"[$l.$dt]")(x => acc(f(l)(x))(o)))),
        x) ))
  }
}
