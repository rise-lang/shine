package idealised.DPIA.FunctionalPrimitives


import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
/*
case class Native(text:String, dt:DataType) extends ExpPrimitive {
  override def `type` = ExpType(dt)

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext) = {
    A :=|dt|this
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])(implicit context: TranslationContext) = {
    C(this)
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor) = this
}
*/