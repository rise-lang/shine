package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}

abstract class AbstractSlide(val sz: Nat, val sp: Nat, val input: Expr,
                             override val t: Option[DataType])
  extends PrimitiveExpr
{

  def make(sz: Nat, sp: Nat, input: Expr, t: Option[DataType]): AbstractSlide

  override def inferType(subs: TypeInference.SubstitutionMap): AbstractSlide = {
    import TypeInference._

    TypeInference(input, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) =>
          val n = (m - sz + sp) /^ sp
          make(sz, sp, array, Some(ArrayType(n, ArrayType(sz, dt))))
        case x => error(
          expr = s"${this.getClass.getSimpleName}($sz, $sp, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def children: Seq[Any] = Seq(sz, sp, input, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(sz: Nat, sp: Nat, input: Expr, t: Option[DataType]@unchecked) =>
      make(sz, sp, input, t)
  }
}
