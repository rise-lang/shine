package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

abstract class AbstractMap(val f: Expr,
                           val array: Expr,
                           override val t: Option[DataType])
  extends PrimitiveExpr
{

  def makeMap: (Expr, Expr, Option[DataType]) => AbstractMap

  override def inferType(subs: SubstitutionMap): Expr = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, dt1)) =>
          setParamAndInferType(f, dt1, subs) |> (f =>
            f.t match {
              case Some(FunctionType(dt1_, dt2: DataType)) =>
                if (dt1 == dt1_) {
                  makeMap(f, array, Some(ArrayType(n, dt2)))
                } else {
                  error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                    found = s"`$dt1_'", expected = s"`$dt1'")
                }
              case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                found = s"`${x.toString}'", expected = "dt1 -> dt2")
            })
        case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def children: Seq[Any] = Seq(f, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(f: Expr, array: Expr, t: Option[DataType]@unchecked) => makeMap(f, array, t)
  }
}
