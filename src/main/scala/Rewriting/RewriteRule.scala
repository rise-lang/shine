package Rewriting

import Core._
import DSL._
import Patterns._

case class RewriteRule[T <: PhraseType](desc: String, rewrite: PartialFunction[Phrase[T], Phrase[T]]) {
  override def toString: String = desc
}

object RewriteRules {
  val mapToFor = RewriteRule[CommandType]("map to for", {
    case Assign(out, PatternPhrase(MapPhrase(f, array))) =>
      `for`(length(out), { i =>
        out `@` i := Apply(f, array `@` i)
      })
  })

  val betaReduction = RewriteRule[ExpType]("beta reduction", {
    case Apply(Lambda(param, body), arg) =>
      OperationalSemantics.substitute(arg, param, in = body)
  })

  val zipIndex = RewriteRule[ExpType]("zip index", {
    case ArrayExpAccessPhrase(PatternPhrase(ZipPhrase(lhs, rhs)), i) =>
      Record(lhs `@` i, rhs `@` i)
  })

  val recordFieldAccess = RewriteRule[ExpType]("record field access", {
    case FieldAccess(i, Record(fields @ _*)) => fields(i)
  })
}