package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.OpenCL.AddressSpace
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

abstract class To(f: Expr[DataType -> DataType],
                  input: DataExpr,
                  addressSpace: AddressSpace,
                  private val makeTo: (Expr[DataType -> DataType], DataExpr, Option[DataType]) => To,
                  private val makeToDPIA: (DPIA.Types.DataType, DPIA.Types.DataType,
                    DPIA.Phrases.Phrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
                    DPIA.Phrases.Phrase[DPIA.Types.ExpType]) => idealised.OpenCL.FunctionalPrimitives.To
                 )
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (f.t, input.t) match {
      case (Some(FunctionType(dt1, dt2)), Some(t1)) if dt1 == t1 =>
        makeToDPIA(dt1, dt2,
          f.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
          input.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): To = {
    import TypeInference._
    val input_ = TypeInference(input, subs)
    input_.t match {
      case Some(dt1) =>
        val f_ = TypeInference.setParamAndInferType(f, dt1, subs)
        f_.t match {
          case Some(FunctionType(t1, dt2)) =>
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

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeTo(VisitAndRebuild(f, fun), VisitAndRebuild(input, fun), t.map(fun(_)))
  }

}
