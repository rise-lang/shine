package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types.{DataType, TypeInference}
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, VisitAndRebuild}

object ForeignFunction {
  final case class Declaration(name: String, argNames: Seq[String], body: String)
}

final case class ForeignFunction(funDecl: ForeignFunction.Declaration,
                                 inTs: Seq[DataType],
                                 outT: DataType,
                                 args: Seq[Expr]) extends PrimitiveExpr
{
  override def inferType(subs: SubstitutionMap): ForeignFunction = {
    args.map(TypeInference(_, subs)) |> (args => {
      args.flatMap(_.t) match {
        case dts: Seq[DataType]@unchecked =>
          var i = 0
          (inTs zip dts) foreach {
            case (expectedT, foundT) =>
              if (expectedT != foundT) {
                TypeInference.error(expr = s"${funDecl.name}(${args.mkString(",")})",
                  found = s"`$foundT'", expected = s"`$expectedT' for argument $i")
              }
              i = i+1
          }
          ForeignFunction(funDecl, dts, outT, args)
      }
    })
  }

  override def children: Seq[Any] = Seq(inTs, outT, args)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(inTs: Seq[DataType]@unchecked, outT: DataType, args: Seq[Expr]@unchecked) =>
      ForeignFunction(funDecl, inTs, outT, args)
  }

  override def t: Option[DataType] = Some(outT)
}
