package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class IdxVec(n: Nat,
                        st: ScalarType,
                        index: Phrase[ExpType],
                        vector: Phrase[ExpType])
  extends ExpPrimitive {

  index :: expT(idx(n), read)
  vector :: expT(vec(n, st), read)
  override val t: ExpType = expT(st, read)

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, vector), OperationalSemantics.eval(s, index)) match {
      case (VectorData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    IdxVec(fun.nat(n), fun.data(st), VisitAndRebuild(index, fun), VisitAndRebuild(vector, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(vector)}).${PrettyPhrasePrinter(index)}"

  override def xmlPrinter: Elem =
    <idxVec n={ToString(n)} st={ToString(st)}>
      <input type={ToString(ExpType(VectorType(n, st), read))}>
        {Phrases.xmlPrinter(vector)}
      </input>
      <index type={ToString(ExpType(int, read))}>
        {Phrases.xmlPrinter(index)}
      </index>
    </idxVec>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(vector)(λ(expT(vec(n, st), read))(x => A :=| st | IdxVec(n, st, index, x)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(vector)(λ(expT(vec(n, st), read))(e => C(IdxVec(n, st, index, e))))
  }
}

object IdxVec {
  def apply(index: Phrase[ExpType], vector: Phrase[ExpType]): IdxVec = {
    (index.t, vector.t) match {
      case (ExpType(IndexType(n1), _: read.type), ExpType(VectorType(n2, st), _: read.type)) if n1 == n2 =>
        IdxVec(n1, st, index, vector)
      case x => error(x.toString, "(exp[idx(n)], exp[st<n>])")
    }
  }
}
