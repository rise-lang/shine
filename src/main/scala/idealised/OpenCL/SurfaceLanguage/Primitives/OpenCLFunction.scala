package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.SurfaceLanguage.Types._

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[DataExpr]) extends PrimitiveExpr {

  override val t: Option[DataType] = Some(outT)


  override def convertToPhrase: idealised.OpenCL.FunctionalPrimitives.OpenCLFunction = {
    import idealised.DPIA.Types.DataType
    idealised.OpenCL.FunctionalPrimitives.OpenCLFunction(
      name, inTs.map(DataType(_)), DataType(outT),
      args.map(_.toPhrase[idealised.DPIA.Types.ExpType]))
  }

  override def inferType(subs: TypeInference.SubstitutionMap): OpenCLFunction = {
    val args_ = args.map(TypeInference(_, subs))
    val dts = args_.flatMap(_.t)
    var i = 0
    (inTs zip dts) foreach {
      case (expectedT, foundT) =>
        if (expectedT != foundT) {
          TypeInference.error(expr = s"$name(${args.mkString(",")})",
            found = s"`$foundT'", expected = s"`$expectedT' for argument $i")
        }
        i = i+1
      case _ =>
    }
    OpenCLFunction(name, dts, outT, args_)
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    OpenCLFunction(name, inTs.map(dt => f(dt)), f(outT), args.map(SurfaceLanguage.VisitAndRebuild(_, f)))
  }

  override def toString: String = s"$name(${args.map(_.toString).mkString(",")})"

}
