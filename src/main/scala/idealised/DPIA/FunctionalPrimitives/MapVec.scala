package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.MapVecI
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class MapVec(n: Nat,
                        dt1: ScalarType,
                        dt2: ScalarType,
                        f: Phrase[ExpType ->: ExpType],
                        array: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt1: ScalarType) ->: (dt2: ScalarType) ->:
      (f :: t"exp[$dt1, $read] -> exp[$dt2, $write]") ->:
        (array :: exp"[${VectorType(n, dt1)}, $read]") ->: exp"[${VectorType(n, dt2)}, $write]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapVec(fun.nat(n), fun.data(dt1), fun.data(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    import idealised.DPIA.Semantics.OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    mapAcceptorTranslation(fun(exp"[$dt1, $read]")(x => x), A)
  }

  override def mapAcceptorTranslation(g: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[${VectorType(n, dt1)}, $read]")(x =>
      MapVecI(n, dt1, dt2, λ(exp"[$dt1, $read]")(x => λ(acc"[$dt2]")(o => acc(g(f(x)))(o))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new`(dt"[${VectorType(n, dt2)}]",
      λ(exp"[${VectorType(n, dt2)}, $read]" x acc"[${VectorType(n, dt2)}]")(tmp =>
        acc(this)(tmp.wr) `;`
          C(tmp.rd) )
      )
  }

  override def prettyPrint: String =
    s"mapVec (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <mapVec n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1, read) ->: ExpType(dt2, read))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(VectorType(n, dt1), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </mapVec>
}
