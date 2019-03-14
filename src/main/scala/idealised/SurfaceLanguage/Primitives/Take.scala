package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}

final case class Take(n: Nat, array: Expr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(m, dt)) =>
        DPIA.FunctionalPrimitives.Take(n, m, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

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
