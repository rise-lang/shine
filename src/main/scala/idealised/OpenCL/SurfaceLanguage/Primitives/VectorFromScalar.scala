package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.{DPIA, OpenCL}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Nat, PrimitiveExpr}

final case class VectorFromScalar(n: Nat, arg: DataExpr,
                                  override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    arg.`type` match {
      case Some(dt: ScalarType) =>
        OpenCL.FunctionalPrimitives.VectorFromScalar(n, dt, arg.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): VectorFromScalar = {
    import TypeInference._
    val arg_ = TypeInference(arg, subs)
    arg_.`type` match {
      case Some(dt: ScalarType) =>
        VectorFromScalar(n, arg_, Some(VectorType(n, dt)))
      case x => error(this.toString, s"`${x.toString}'", "st")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    VectorFromScalar(f(n), VisitAndRebuild(arg, f), `type`.map(f(_)))
  }

}
