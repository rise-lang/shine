package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.OpenCL.AddressSpace
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.OpenCL

final case class OpenCLReduceSeq(f: Expr[DataType -> (DataType -> DataType)],
                                 init: DataExpr,
                                 initAddrSpace: AddressSpace,
                                 array: DataExpr,
                                 override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (f.t, init.t, array.t) match {
      case (Some(FunctionType(t1, FunctionType(t2, t3))), Some(dt2), Some(ArrayType(n, dt1)))
        if dt1 == t1 && dt2 == t2 && dt2 == t3 =>
         OpenCL.FunctionalPrimitives.OpenCLReduceSeq(n, dt1, dt2,
          f.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
          init.toPhrase[DPIA.Types.ExpType],
           initAddrSpace,
          array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: SubstitutionMap): OpenCLReduceSeq = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      TypeInference(init , subs) |> (init =>
        (init.t, array.t) match {
          case (Some(dt2), Some(ArrayType(_, dt1))) =>
            setParamsAndInferTypes(f, dt1, dt2, subs) |> (f =>
              f.t match {
                case Some(FunctionType(t1, FunctionType(t2, t3))) =>
                  if (dt1 == t1 && dt2 == t2 && dt2 == t3) {
                    OpenCLReduceSeq(f, init, initAddrSpace, array, Some(dt2))
                  } else {
                    error(this.toString,
                      dt1.toString + ", " + t1.toString + " as well as " +
                        dt2.toString + ", " + t2.toString + " and " + t3.toString,
                      expected = "them to match")
                  }
                case x => error(expr = s"${this.getClass.getSimpleName}($f, $init, $array)",
                  found = s"`${x.toString}'", expected = "dt1 -> (dt2 -> dt3)")
              })
          case x => error(expr = s"${this.getClass.getSimpleName}($f, $init, $array)",
            found = s"`${x.toString}'", expected = "(dt1, n.dt2)")
        }))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    OpenCLReduceSeq(VisitAndRebuild(f, fun),
                    VisitAndRebuild(init, fun),
                    initAddrSpace,
                    VisitAndRebuild(array, fun),
                    t.map(fun(_)))
  }
}
