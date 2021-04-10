package shine.DPIA.Compilation

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.language.reflectiveCalls

object TranslationToImperative {
  def apply(p: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def acc(E: Phrase[ExpType])
         (A: Phrase[AccType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    AcceptorTranslation.acc(E)(A)
  }

  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    ContinuationTranslation.con(E)(C)
  }

  def fedAcc(env: Map[Identifier[ExpType], Identifier[AccType]])
            (E: Phrase[ExpType])
            (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    FedeTranslation.fedAcc(env)(E)(C)
  }

  def str(E: Phrase[ExpType])
         (C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    StreamTranslation.str(E)(C)
  }
}
