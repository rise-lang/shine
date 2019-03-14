package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.{DPIA, OpenCL}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import idealised.SurfaceLanguage.Types._

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Expr]) extends PrimitiveExpr {

  override val t: Option[DataType] = Some(outT)


  override def convertToPhrase: OpenCL.FunctionalPrimitives.OpenCLFunction = {
    import DPIA.Types.DataType
    OpenCL.FunctionalPrimitives.OpenCLFunction(
      name, inTs.map(DataType(_)), DataType(outT),
      args.map(_.toPhrase[DPIA.Types.ExpType]))
  }

  override def inferType(subs: TypeInference.SubstitutionMap): OpenCLFunction = {
    args.map(TypeInference(_, subs)) |> (args => {
      args.flatMap(_.t) match {
        case dts: Seq[DataType]@unchecked =>
          var i = 0
          (inTs zip dts) foreach {
            case (expectedT, foundT) =>
              if (expectedT != foundT) {
                TypeInference.error(expr = s"$name(${args.mkString(",")})",
                  found = s"`$foundT'", expected = s"`$expectedT' for argument $i")
              }
              i = i+1
          }
          OpenCLFunction(name, dts, outT, args)
      }
    })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    OpenCLFunction(name, inTs.map(f(_)), f(outT), args.map(SurfaceLanguage.VisitAndRebuild(_, f)))
  }

  override def toString: String = s"$name(${args.map(_.toString).mkString(",")})"

}
