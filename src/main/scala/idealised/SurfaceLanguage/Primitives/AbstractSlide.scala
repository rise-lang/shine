package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

abstract class AbstractSlide(sz: Nat, sp: Nat, input: DataExpr,
                             override val t: Option[DataType])
  extends PrimitiveExpr
{
  def makeDPIA(n: Nat,
               sz: Nat,
               sp: Nat,
               dt: DataType,
               input: DPIA.Phrases.Phrase[DPIA.Types.ExpType]
              ): DPIA.Phrases.Phrase[DPIA.Types.ExpType]

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    input.t match {
      case Some(ArrayType(m, dt)) =>
        val n = (m - sz + sp) /^ sp
        makeDPIA(n, sz, sp, dt, input.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  def make(sz: Nat, sp: Nat, input: DataExpr, t: Option[DataType]): AbstractSlide

  override def inferType(subs: TypeInference.SubstitutionMap): AbstractSlide = {
    import TypeInference._

    TypeInference(input, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) =>
          val n = (m - sz + sp) /^ sp
          make(sz, sp, array, Some(ArrayType(n, ArrayType(sz, dt))))
        case x => error(
          expr = s"${this.getClass.getSimpleName}($sz, $sp, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    make(f(sz), f(sp), SurfaceLanguage.VisitAndRebuild(input, f), t.map(f(_)))
  }
}
