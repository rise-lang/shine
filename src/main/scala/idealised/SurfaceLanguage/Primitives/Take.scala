package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Take(n: Nat, array: Expr,
                      override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Take = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(_, dt)) => Take(n, array, Some(ArrayType(n, dt)))
        //TODO: Check that n < m
        case x => error(expr = s"Take($n, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    Take(f(n), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
