package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[DataExpr]) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    val args_ = args.map(ExpressionToPhrase(_, subs))
    val dts = args_.map(_.t.dataType)
    idealised.OpenCL.FunctionalPrimitives.OpenCLFunction(name, dts, outT, args_)
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    OpenCLFunction(name, inTs.map(dt => f(dt)), f(outT), args.map(VisitAndRebuild(_, f)))
  }

}
