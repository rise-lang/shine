package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.primitives.intermediate.OpenCLRotateValuesI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class RotateValues(a: AddressSpace,
                              n: Nat,
                              sz: Nat,
                              dt: DataType,
                              write: Phrase[ExpType ->: ExpType],
                              input: Phrase[ExpType]
                             ) extends ExpPrimitive with StreamT {
  write :: expT(dt, read) ->: expT(dt, shine.DPIA.Types.write)
  input :: expT((n - 1 + sz)`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                       (implicit context: TranslationContext): Phrase[CommType] = {
    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      OpenCLRotateValuesI(a, n, sz, dt,
        fun(expT(dt, read))(x =>
          fun(accT(dt))(o => acc(write(x))(o))),
        nextIn, C)
    ))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.primitives.functional.Slide
    Slide(n, sz, 1, dt, input).eval(s)
  }
}