package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, ToDPIA}
import idealised.SurfaceLanguage.Types._

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[DataExpr]) extends PrimitiveExpr {

  override val `type`: Option[DataType] = Some(outT)


  override def toDPIA: idealised.OpenCL.FunctionalPrimitives.OpenCLFunction = {
    import idealised.DPIA.Types.DataType
    idealised.OpenCL.FunctionalPrimitives.OpenCLFunction(
      name, inTs.map(DataType(_)), DataType(outT), args.map(ToDPIA(_)))
  }

  override def inferType(subs: TypeInference.SubstitutionMap): OpenCLFunction = {
    val args_ = args.map(TypeInference(_, subs))
    val dts = args_.flatMap(_.`type`)
    OpenCLFunction(name, dts, outT, args_)
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    OpenCLFunction(name, inTs.map(dt => f(dt)), f(outT), args.map(SurfaceLanguage.VisitAndRebuild(_, f)))
  }

  override def toString: String = s"$name(${args.map(_.toString).mkString(",")})"

}
