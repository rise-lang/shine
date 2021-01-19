package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL.`new`
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ToMem(addrSpace: AddressSpace,
                       dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive with ConT {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, read)

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(input, dt, addrSpace)

    `new` (addrSpace) (adj.dt, tmp =>
      acc(input)(adj.accF(tmp.wr)) `;` C(adj.exprF(tmp.rd)))
  }

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)
}
