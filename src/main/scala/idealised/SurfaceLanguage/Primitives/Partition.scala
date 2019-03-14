package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.{Nat, NatNatTypeFunction}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import lift.arithmetic.{ArithExpr, BigSum}

final case class Partition(m:Nat, lenF:NatNatTypeFunction, array: DataExpr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.Partition(n, m, lenF, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Partition = {
    import TypeInference._
    val typed = TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, dt)) =>
          (BigSum(from=0, upTo = m-1, `for`=lenF.x, lenF.body) == n,
            s"Total output size of partition not provably equal to input size!" +
              s"${BigSum(from=0, upTo = m-1, `for`=lenF.x, lenF.body)} != $n."
          )
          Partition(m, lenF, array,
            Some(DepArrayType(m, i => ArrayType(lenF(i), dt))))
        case x => error(expr = s"Partition($m, $lenF $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
    typed
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Partition(f(m), f(lenF), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
