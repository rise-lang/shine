package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference}

final case class TransposeOnWrite(array: DataExpr,
                                  override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        import idealised.DPIA.FunctionalPrimitives._
        import idealised.DPIA.DSL._
        import idealised.DPIA.Types._

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            val j = i asNatIdentifier(withUpperBound = n * m)
            val col = (j % m) * n
            val row = j / m

            row + col asPhrase(withType = IndexType(n * m))
          })

        Split(n, m, dt,
          Scatter(n*m, dt, transposeFunction,
            Join(n, m, dt, array.toPhrase[ExpType])))

      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): TransposeOnWrite = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) => TransposeOnWrite(array, Some(ArrayType(m, ArrayType(n, dt))))
        case x => error(expr = s"TransposeOnWrite($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr =
    TransposeOnWrite(VisitAndRebuild(array, f))
}
