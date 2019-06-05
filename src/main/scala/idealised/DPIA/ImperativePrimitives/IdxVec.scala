package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class IdxVec(n: Nat,
                        st: ScalarType,
                        index: Phrase[ExpType],
                        vector: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) -> (st: ScalarType) ->
      (index :: exp"[idx($n)]") ->
        (vector :: exp"[${VectorType(n, st)}]") ->
          exp"[$st]"

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, vector), OperationalSemantics.eval(s, index)) match {
      case (VectorData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    IdxVec(fun(n), fun(st), VisitAndRebuild(index, fun), VisitAndRebuild(vector, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(vector)}).${PrettyPhrasePrinter(index)}"

  override def xmlPrinter: Elem =
    <idxVec n={ToString(n)} st={ToString(st)}>
      <input type={ToString(ExpType(VectorType(n, st)))}>
        {Phrases.xmlPrinter(vector)}
      </input>
      <index type={ToString(ExpType(int))}>
        {Phrases.xmlPrinter(index)}
      </index>
    </idxVec>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(vector)(λ(exp"[${VectorType(n, st)}]")(x => A :=| st | IdxVec(n, st, index, x)))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(vector)(λ(exp"[${VectorType(n, st)}]")(e => C(IdxVec(n, st, index, e))))
  }
}

object IdxVec {
  def apply(index: Phrase[ExpType], vector: Phrase[ExpType]): IdxVec = {
    (index.t, vector.t) match {
      case (ExpType(IndexType(n1)), ExpType(VectorType(n2, st))) if n1 == n2 =>
        IdxVec(n1, st, index, vector)
      case x => error(x.toString, "(exp[idx(n)], exp[st<n>])")
    }
  }
}
