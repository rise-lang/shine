package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.DPIA.FunctionalPrimitives.TransposeArrayDep
import idealised.DPIA.Types.ExpType
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.SurfaceLanguage.Types._

final case class Transpose(array: DataExpr,
                           override val t: Option[DataType])
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
            val col = (j % n) * m
            val row = j / n
            (row + col) asPhrase(withType = IndexType(n * m))
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            val j = i asNatIdentifier(withUpperBound = n * m)
            val col = (j % m) * n
            val row = j / m

            row + col asPhrase(withType = IndexType(n * m))
          })

        Split(n, m, dt,
          Reorder(n*m, dt, transposeFunction, transposeInverseFunction,
            Join(n, m, dt, array.toPhrase[ExpType])))

      case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) =>
        TransposeArrayDep(n, m, i, dt, array.toPhrase[ExpType])

      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Transpose = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) => Transpose(array, Some(ArrayType(m, ArrayType(n, dt))))
        case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) =>
          val outputType = DepArrayType(m, k => ArrayType(n, Type.substitute(k, `for`=i, `in` = dt)))
          Transpose(array,  Some(outputType))
        case x => error(expr = s"Transpose($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr =
    Transpose(VisitAndRebuild(array, f), t)
}
