package idealised.SurfaceLanguage.Primitives

import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import TypeInference._

final case class MapSeqSlide(size: Nat,
                             step: Nat,
                             f: Expr[DataType -> DataType],
                             input: DataExpr,
                             override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.MapSeqSlide = {
    val (_, _, n) = inputInfo(input.t)
    val (_, dt1, dt2) = fInfo(f.t)
    DPIA.FunctionalPrimitives.MapSeqSlide(
      n, size, step, dt1, dt2,
      f.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
      input.toPhrase[DPIA.Types.ExpType])
  }

  override def inferType(subs: SubstitutionMap): DataExpr = {
    // TODO? reuse Map and Slide type inference here
    TypeInference(input, subs) |> (input => {
      val (inputSize, dt1, n) = inputInfo(input.t)
      setParamAndInferType(f, ArrayType(size, dt1), subs) |> (f => {
        val (size_, dt1_, dt2) = fInfo(f.t)
        if (dt1 == dt1_) {
          MapSeqSlide(size, step, f, input, Some(ArrayType(n, dt2)))
        } else {
          error(
            expr = s"MapSeqSlide($size, $step, $f, $input)",
            found = s"`$dt1_'", expected = s"`$dt1'"
          )
        }})
    })
  }

  def fInfo(t: Option[Type]) = {
    t match {
      case Some(FunctionType(ArrayType(size_, dt1), dt2 : DataType)) => (size_, dt1, dt2)
      case x => error(
        expr = s"MapSeqSlide($size, $step, $f, $input)",
        found = s"`$x'", expected = "size.dt1 -> dt2"
      )
    }
  }

  def inputInfo(t: Option[DataType]) = {
    t match {
      case Some(ArrayType(in, dt)) => (in, dt, (in - size + step) /^ step)
      case x => error(
        expr = s"MapSeqSlide($size, $step, $f, $input)",
        found = s"`$x'",
        expected = "n.dt")
    }
  }

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): DataExpr =
    MapSeqSlide(v(size), v(step),
      SurfaceLanguage.VisitAndRebuild(f, v),
      SurfaceLanguage.VisitAndRebuild(input, v),
      t.map(v(_)))
}
