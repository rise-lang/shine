package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference}

final case class Transpose(array: DataExpr,
                           override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.`type` match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        import idealised.DPIA.FunctionalPrimitives._
        import idealised.DPIA.DSL._
        import idealised.DPIA.Types._

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            val j = i asNatIdentifier(withUpperBound = n * m)
//            val col = (j % m) * n
//            val row = j / m
//
//            row + col
            j asPhrase(withType = IndexType(n * m))
          })

        Split(n, m, dt,
          Gather(n*m, dt, transposeFunction,
            Join(n, m, dt, array.toPhrase[ExpType])))

      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Transpose = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.`type` match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        Transpose(array_, Some(ArrayType(m, ArrayType(n, dt))))
      case x => error(expr = s"Transpose($array_)",
        found = s"`${x.toString}'", expected = "n.m.dt")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr =
    Transpose(VisitAndRebuild(array, f))
}
