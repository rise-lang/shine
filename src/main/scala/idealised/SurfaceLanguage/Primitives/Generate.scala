package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Generate(f: Expr, override val t: Option[DataType] = None)
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Generate = {
    import TypeInference._
    TypeInference(f, subs) |> (f =>
      f.t match {
        case Some(FunctionType(IndexType(n), dt: DataType)) => Generate(f, Some(ArrayType(n, dt)))
        case x => error(expr = s"Generator($f)", found = s"`${x.toString}'", expected = "idx[n] -> dt")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Expr = {
    Generate(VisitAndRebuild(f, fun), t.map(fun(_)))
  }
}
