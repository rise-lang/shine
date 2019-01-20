package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

import scala.language.{postfixOps, reflectiveCalls}

final case class Scatter(idxF: Expr[DataType ->DataType],
                         array: DataExpr,
                         override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.Scatter(n, dt,
          idxF.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
          array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Scatter = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, _)) =>
          TypeInference(idxF, subs) |> (idxF =>
            idxF.t match {
              case Some(FunctionType(IndexType(m: NatIdentifier), _)) =>
                val idxF_ = Type.substitute(n, `for` = m, in = idxF)
                Scatter(idxF_, array, array.t)
              case Some(FunctionType(IndexType(m1), IndexType(m2))) =>
                if (n == m1 && n == m2) {
                  Scatter(idxF, array, array.t)
                } else {
                  error(expr = s"Gather($idxF, $array)",
                    found = s"`$n', `$m1' and `$m2'", expected = "them to match")
                }
              case x => error(expr = s"Gather($idxF, $array)",
                found = s"`${x.toString}'", expected = "idx(_) -> idx(_)")
            })
        case x => error(expr = s"Gather($idxF, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Scatter = {
    Scatter(VisitAndRebuild(idxF, f), VisitAndRebuild(array, f), t)
  }
}
