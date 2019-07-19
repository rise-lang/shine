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
                         f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType = {
    val l = f.t.x
    (n: Nat) ->: (m: Nat) ->: (k: Nat) ->: (dt: DataType) ->:
      (f :: t"($l : nat) -> exp[${l * n}.$dt] -> exp[$l.$dt]") ->:
        (array :: exp"[${m * n.pow(k)}.$dt]") ->:
          exp"[$m.$dt]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Iterate(fun.nat(n), fun.nat(m), fun.nat(k), fun.data(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
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
    val l = f.t.x
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l ->: ExpType(ArrayType(l, dt)) ->: ExpType(ArrayType(l /^ n, dt)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._

    con(array)(λ(exp"[${m * n.pow(k)}.$dt]")(x =>
      IterateIAcc(n, m, k, dt, A,
        _Λ_[NatKind](l => λ(acc"[$l.$dt]")(o => λ(exp"[${l * n}.$dt]")(x => acc(f(l)(x))(o)))),
        x) ))
  }

  // TODO
  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._

    ???
  }
}
