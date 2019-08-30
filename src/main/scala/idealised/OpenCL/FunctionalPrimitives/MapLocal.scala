package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{`new`, λ, _}
import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.OpenCLNew
import idealised.OpenCL.IntermediatePrimitives.MapLocalI
import idealised.OpenCL.AddressSpace

final case class MapLocal(dim: Int, addressSpace: AddressSpace)(n: Nat,
                                    dt1: DataType,
                                    dt2: DataType,
                                    f: Phrase[ExpType ->: ExpType],
                                    array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapLocal(dim, addressSpace)

  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    MapLocalI(dim)(n, dt1, dt2, f, array, out)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    OpenCLNew(dt"[$n.$dt2]", addressSpace, λ(exp"[$n.$dt2]" x acc"[$n.$dt2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }
}
