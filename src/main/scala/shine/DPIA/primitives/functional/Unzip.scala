package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.UnzipAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Unzip(n: Nat,
                       dt1: DataType,
                       dt2: DataType,
                       access: AccessType,
                       e: Phrase[ExpType]
                      ) extends ExpPrimitive with AccT with FedeT {
  e :: expT(n`.`(dt1 x dt2), access)
  override val t: ExpType = expT((n`.`dt1) x (n`.`dt2), access)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(e)(UnzipAcc(n, dt1, dt2, A))

  def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(e)(fun(accT(C.t.inT.dataType))(o =>
      UnzipAcc(n, dt1, dt2, C(o))))

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, e) match {
      case ArrayData(xs) =>
        val (lhs, rhs) = xs.foldLeft((Vector[Data](), Vector[Data]())) {
          case (vs: (Vector[Data], Vector[Data]), p: PairData) =>
            (vs._1 :+ p.fst, vs._2 :+ p.snd)
          case _ => throw new Exception("This should not happen")
        }
        PairData(ArrayData(lhs), ArrayData(rhs))
      case _ => throw new Exception("This should not happen")
    }
  }
}
