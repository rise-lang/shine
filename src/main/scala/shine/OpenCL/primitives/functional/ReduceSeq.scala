package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.primitives.intermediate.OpenCLReduceSeqI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReduceSeq(unroll: Boolean)
                          (n: Nat,
                           initAddrSpace: AddressSpace,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive with ConT {
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt1, read))(X =>
      OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt2, read))(x =>
          λ(expT(dt1, read))(y =>
            λ(accT(dt2))(o => acc( f(x)(y) )( o )))),
        init, X, C, unroll)(context)))
}
