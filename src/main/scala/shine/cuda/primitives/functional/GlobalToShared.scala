package shine.cuda.primitives.functional

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.{Phrases, _}
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL.`new`
import shine.cuda.primitives.imperative.{GlobalToSharedAcc, SyncPipeline}
import shine.macros.Primitive.expPrimitive

import scala.xml.Elem

@expPrimitive
final case class GlobalToShared(dt: DataType,
                                inputGlobal: Phrase[ExpType]) extends ExpPrimitive with ConT {

  inputGlobal :: expT(dt, write)
  override val t: ExpType = expT(dt, read)

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    val adj = AdjustArraySizesForAllocations(inputGlobal, dt, AddressSpace.Local)

    `new` (AddressSpace.Private) (pipeline, pipeline =>
      `new` (AddressSpace.Local) (adj.dt, tmp => acc(inputGlobal)(GlobalToSharedAcc(dt, pipeline.rd, tmp.wr)) `;`
        SyncPipeline(pipeline.rd) `;`
        C(adj.exprF(tmp.rd))))
  }
}

