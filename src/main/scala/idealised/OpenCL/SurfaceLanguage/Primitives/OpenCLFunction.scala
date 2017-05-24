package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[DataExpr]) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    val args_ = args.map(TypeInference(_, subs))
    val dts = args_.map(_.t.dataType)
    idealised.OpenCL.FunctionalPrimitives.OpenCLFunction(name, dts, outT, args_)
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    OpenCLFunction(name, inTs.map(dt => f(dt)), f(outT), args.map(SurfaceLanguage.VisitAndRebuild(_, f)))
  }

}
