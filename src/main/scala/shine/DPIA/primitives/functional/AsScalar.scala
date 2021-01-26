package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.AsScalarAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class AsScalar(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          access: AccessType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive with ConT with AccT with FedeT {
  array :: expT(n `.` vec(m, dt), access)
  override val t: ExpType = expT((n * m) `.` dt, access)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(array)(AsScalarAcc(n, m, dt, A))

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(array.t)(x =>
      C(AsScalar(n, m, dt, access, x))))

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
      AsScalarAcc(n, m, dt, C(o))))
}
