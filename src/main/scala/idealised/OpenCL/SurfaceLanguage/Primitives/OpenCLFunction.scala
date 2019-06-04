package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Expr]) extends PrimitiveExpr {

  override val t: Option[DataType] = Some(outT)

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

  override def children: Seq[Any] = Seq(inTs, outT, args)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(inTs: Seq[DataType]@unchecked, outT: DataType, args: Seq[Expr]@unchecked) =>
      OpenCLFunction(name, inTs, outT, args)
  }

  override def toString: String = s"$name(${args.map(_.toString).mkString(",")})"

}
