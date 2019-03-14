package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Nat
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import lift.arithmetic.ArithExpr

final case class Partition(m:Nat, lenID:NatIdentifier, lenBody:Nat, array: Expr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{

  val lenF:Nat => Nat = (x:Nat) => ArithExpr.substitute(lenBody, scala.collection.Map((lenID, x)))

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.Partition(n, m, lenID, lenBody, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Partition = {
    import TypeInference._
    val typed = TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(_, dt)) =>
          Partition(m, lenID, lenBody, array, Some(DepArrayType(m, i => ArrayType(lenF(i), dt))))
        case x => error(expr = s"Partition($m, $lenID => $lenBody $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
    typed
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    Partition(f(m), f(lenID).asInstanceOf[NatIdentifier], f(lenBody), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
