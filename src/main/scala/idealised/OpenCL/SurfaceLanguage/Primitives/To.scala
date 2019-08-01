package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.AddressSpace
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, _}

abstract class To(val addressSpace: AddressSpace,
                  val input: Expr,
                  private val makeTo: (Expr, Option[DataType]) => To
                 )
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): To = {
    import TypeInference._
    val input_ = TypeInference(input, subs)
    input_.t match {
      case Some(dt: DataType) => makeTo(input_, Some(dt))
      case x => error(this.toString, s"`${x.toString}'", "dt")
    }
  }

  override def children: Seq[Any] = Seq(input, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(input: Expr, t: Option[DataType]@unchecked) => makeTo(input, t)
  }
}
