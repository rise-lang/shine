package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.JoinAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Join(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType]
                     ) extends ExpPrimitive with FedeT {
  array :: expT(n`.`(m`.`dt), w)
  override val t: ExpType = expT((n * m)`.`dt, w)

  def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(array)(λ(accT(C.t.inT.dataType))(o => JoinAcc(n, m, dt, C(o))))

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }
}
