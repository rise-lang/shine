package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

import scala.language.{postfixOps, reflectiveCalls}

final case class Reorder(idxF: Expr[DataType -> DataType],
                         idxFinv: Expr[DataType -> DataType],
                         array: DataExpr,
                         override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.Reorder(n, dt,
          idxF.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
          idxFinv.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
          array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): DataExpr = {
    import TypeInference._

    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, _)) => {
          def idxFcheck(f: Expr[DataType -> DataType]) =
            f.t match {
              case Some(FunctionType(IndexType(m: NatIdentifier), _)) =>
                Type.substitute(n, `for` = m, in = f)
              case Some(FunctionType(IndexType(m1), IndexType(m2))) =>
                if (n == m1 && n == m2) {
                  f
                } else {
                  error(expr = s"Reorder($idxF, $idxFinv, $array)",
                    found = s"`$n', `$m1' and `$m2'", expected = "them to match")
                }
              case x => error(expr = s"Reorder($idxF, $idxFinv, $array)",
                found = s"`${x.toString}'", expected = "idx(_) -> idx(_)")
            }

          TypeInference(idxF, subs) |> (idxF =>
            TypeInference(idxFinv, subs) |> (idxFinv =>
              Reorder(idxFcheck(idxF), idxFcheck(idxFinv), array, array.t)))
        }

        case x => error(expr = s"Reorder($idxF, $idxFinv, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Reorder(
      VisitAndRebuild(idxF, f),
      VisitAndRebuild(idxFinv, f),
      VisitAndRebuild(array, f),
      t)
  }
}
