package Rewriting

import Core._
import DSL._
import ExpPatterns._

case class RewriteRule(desc: String, rewrite: PartialFunction[Phrase[_ <: PhraseType], Phrase[_ <: PhraseType]]) {
  override def toString: String = desc
  def apply[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    rewrite(p).asInstanceOf[Phrase[T]]
  }
}

object RewriteRules {
  val betaReduction = RewriteRule("beta reduction", {
    case Apply(Lambda(param, body), arg) =>
      OperationalSemantics.substitute(arg, param, body)
  })

  val mapToFor = RewriteRule("map to for", {
    case Assign(out, ExpPatternPhrase(MapPhrase(f, array))) =>
      `for`(length(out), { i =>
        out `@` i := Apply(f, array `@` i)
      })
  })

  val joinToFor = RewriteRule("join to for", {
    case Assign(out, ExpPatternPhrase(JoinPhrase(array))) =>
      `for`(length(array), { i =>
        `for`(length(array `@` i), { j =>
          out `@` (i*length(array `@` i)+j) := (array `@` j) `@` j
        })
      })
  })

  val mapIndex = RewriteRule("map index", {
    case ArrayExpAccessPhrase(ExpPatternPhrase(MapPhrase(f, array)), i) =>
      Apply(f, array `@` i)
  })

  val splitIndex = RewriteRule("split index", {
    case ArrayExpAccessPhrase(ArrayExpAccessPhrase(ExpPatternPhrase(SplitPhrase(n, array)), i), j) =>
      array `@` (i * n + j)
  })

  val zipIndex = RewriteRule("zip index", {
    case ArrayExpAccessPhrase(ExpPatternPhrase(ZipPhrase(lhs, rhs)), i) =>
      Record(lhs `@` i, rhs `@` i)
  })

  val recordFieldAccess = RewriteRule("record field access", {
    case FieldAccess(i, Record(fields@_*)) => fields(i)
  })

  val rules: Vector[RewriteRule] = Vector(betaReduction, mapToFor, joinToFor, mapIndex, splitIndex, zipIndex, recordFieldAccess)
}