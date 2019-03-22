package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.AddressSpace
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, _}

abstract class To(val f: Expr,
                  val input: Expr,
                  val addressSpace: AddressSpace,
                  private val makeTo: (Expr, Expr, Option[DataType]) => To
                 )
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): To = {
    import TypeInference._
    val input_ = TypeInference(input, subs)
    input_.t match {
      case Some(dt1: DataType) =>
        val f_ = TypeInference.setParamAndInferType(f, dt1, subs)
        f_.t match {
          case Some(FunctionType(t1, dt2: DataType)) =>
            if (dt1 == t1) {
              makeTo(f_, input_, Some(dt2))
            } else {
              error(this.toString,
                s"`${dt1.toString}' and `${t1.toString}'", expected = "them to match")
            }
          case x => error(this.toString, s"`${x.toString}'", " -> dt2")
        }
      case x => error(this.toString, s"`${x.toString}'", "dt")
    }
  }

  override def children: Seq[Any] = Seq(f, input, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(f: Expr, input: Expr, t: Option[DataType]) => makeTo(f, input, t)
  }
}
