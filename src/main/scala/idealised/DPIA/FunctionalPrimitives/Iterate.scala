package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.IterateIAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Iterate(n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->`[ExpType -> ExpType]],
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType = {
    val l = f.t.x
    (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
      (f :: t"($l : nat) -> exp[${l * n}.$dt, $read] -> exp[$l.$dt, $read]") ->
      (array :: exp"[${m * n.pow(k)}.$dt, $read]") -> exp"[$m.$dt, $read]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Iterate(fun(n), fun(m), fun(k), fun(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
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
    val l = f.t match {
      case DependentFunctionType(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l -> (ExpType(ArrayType(l, dt), read) -> ExpType(ArrayType(l /^ n, dt), read)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._

    con(array)(λ(exp"[${m * n.pow(k)}.$dt, $read]")(x =>
      IterateIAcc(n, m, k, dt, A,
        _Λ_[NatKind](l => λ(acc"[$l.$dt]")(o => λ(exp"[${l * n}.$dt, $read]")(x => acc(f(l)(x))(o)))),
        x) ))
  }

  // TODO
  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._

    ???
  }
}
