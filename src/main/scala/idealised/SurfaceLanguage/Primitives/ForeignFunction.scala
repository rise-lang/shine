package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types.{DataType, TypeInference}
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}

object ForeignFunction {
  final case class Declaration(name: String, argNames: Seq[String], body: String)
}

final case class ForeignFunction(funDecl: ForeignFunction.Declaration,
                                 inTs: Seq[DataType],
                                 outT: DataType,
                                 args: Seq[DataExpr]) extends PrimitiveExpr
{
  override def inferType(subs: SubstitutionMap): ForeignFunction = {
    args.map(TypeInference(_, subs)) |> (args => {
      val dts = args.flatMap(_.t)
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
    })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    ForeignFunction(funDecl, inTs.map(f(_)), f(outT), args.map(VisitAndRebuild(_, f)))
  }

  override def convertToPhrase: DPIA.FunctionalPrimitives.ForeignFunction = {
    import DPIA.Types.DataType
    DPIA.FunctionalPrimitives.ForeignFunction(
      DPIA.FunctionalPrimitives.ForeignFunction.Declaration(funDecl.name, funDecl.argNames, funDecl.body),
      inTs.map(DataType(_)), DataType(outT),
      args.map(_.toPhrase[DPIA.Types.ExpType])
    )
  }


  override def t: Option[DataType] = Some(outT)
}
