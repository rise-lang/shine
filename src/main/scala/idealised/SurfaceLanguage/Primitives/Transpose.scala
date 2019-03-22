package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference, _}
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, VisitAndRebuild}

final case class Transpose(array: Expr,
                           override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Transpose = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) => Transpose(array, Some(ArrayType(m, ArrayType(n, dt))))
        case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) =>
          val outputType = DepArrayType(m, k => ArrayType(n, Type.substitute(k, `for` = i, `in` = dt)))
          Transpose(array, Some(outputType))
        case x => error(expr = s"Transpose($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr =
    Transpose(VisitAndRebuild(array, f), t)
}
