package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.DPIA
import idealised.SurfaceLanguage.Types._

final case class PrintType(input: DataExpr,
                           msg: String,
                           override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = input.toPhrase[DPIA.Types.ExpType]

  override def inferType(subs: TypeInference.SubstitutionMap): PrintType = {
    TypeInference(input, subs) |> (input => {
      println(s"Type $msg: ${input.t match {
        case None => "NoType"
        case Some(dt) => dt.toString
      }}")
      PrintType(input, msg, input.t)
    })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    PrintType(VisitAndRebuild(input, f), msg, t.map(f(_)))
  }

  override def toString: String = ""
}
