package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.{DPIA, OpenCL, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class PrintType(input: DataExpr,
                           msg: String,
                           override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = input.toPhrase[DPIA.Types.ExpType]

  override def inferType(subs: TypeInference.SubstitutionMap): PrintType = {
    val input_ = TypeInference(input, subs)
    println(s"Type $msg: ${input_.`type` match {
      case None => "NoType"
      case Some(dt) => dt.toString
    }}")
    PrintType(input_, msg, input_.`type`)
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    PrintType(VisitAndRebuild(input, f), msg, `type`.map(f(_)))
  }

  override def toString: String = ""
}
