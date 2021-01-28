package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.Continuation
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Generate(n: Nat,
                          dt: DataType,
                          f: Phrase[ExpType ->: ExpType]
                         ) extends ExpPrimitive with ConT {
  f :: expT(idx(n), read) ->: expT(dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    val fc = fun(expT(idx(n), read))(x =>
      Continuation(dt, fun(expT(dt, read) ->: (comm: CommType))(Cf => con(f(x))(Cf))))
    C(Generate(n, dt, fc))
  }
}
