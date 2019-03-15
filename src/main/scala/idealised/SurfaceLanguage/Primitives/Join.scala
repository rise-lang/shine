package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.NatNatTypeFunction
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import idealised.SurfaceLanguage.Types._
import idealised.{DPIA, SurfaceLanguage}
import lift.arithmetic.BigSum

final case class Join(array: Expr, override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        DPIA.FunctionalPrimitives.Join(n, m, dt, array.toPhrase[DPIA.Types.ExpType])
      case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) =>
        ???
      case Some(DepArrayType(n, NatDependentFunctionType(d_i, ArrayType(d_n, dt)))) =>
        DPIA.FunctionalPrimitives.DepJoin(n, NatNatTypeFunction(n, d_i, d_n), dt, array.toPhrase[DPIA.Types.ExpType])
      case Some(DepArrayType(n, NatDependentFunctionType(i, DepArrayType(m, NatDependentFunctionType(j, dt))))) =>
        ???
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Join = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) =>
          Join(array, Some(ArrayType(n * m, dt)))
        case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) => ???
        case Some(DepArrayType(n, NatDependentFunctionType(i, ArrayType(d_n, dt)))) =>
          Join(array, Some(ArrayType(BigSum(from=0, upTo = n-1, `for`=i, d_n), dt)))
        case Some(DepArrayType(n, NatDependentFunctionType(i, DepArrayType(m, NatDependentFunctionType(j, dt))))) => ???
        case x => error(expr = s"Join($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    Join(SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
