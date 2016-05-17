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
    case ApplyPhrase(LambdaPhrase(param, body), arg) =>
      OperationalSemantics.substitute(arg, param, body)
  })

  val mapToFor = RewriteRule("map to for", {
    case AssignPhrase(out, ExpPatternPhrase(MapPattern(f, array))) =>
      `for`(length(out), { i =>
        out `@` i := ApplyPhrase(f, array `@` i)
      })
  })

  val joinToFor = RewriteRule("join to for", {
    case AssignPhrase(out, ExpPatternPhrase(JoinPattern(array))) =>
      `for`(length(array), { i =>
        `for`(length(array `@` i), { j =>
          out `@` (i*length(array `@` i)+j) := (array `@` j) `@` j
        })
      })
  })

  val mapIndex = RewriteRule("map index", {
    case ArrayExpAccessPhrase(ExpPatternPhrase(MapPattern(f, array)), i) =>
      ApplyPhrase(f, array `@` i)
  })

  val splitIndex = RewriteRule("split index", {
    case ArrayExpAccessPhrase(ArrayExpAccessPhrase(ExpPatternPhrase(SplitPattern(n, array)), i), j) =>
      array `@` (i * n + j)
  })

  val zipIndex = RewriteRule("zip index", {
    case ArrayExpAccessPhrase(ExpPatternPhrase(ZipPattern(lhs, rhs)), i) =>
      RecordExpPhase(lhs `@` i, rhs `@` i)
  })

  val fstAccess = RewriteRule("record field access", {
    case FstExprPhrase(RecordExpPhase(fst, _)) => fst
  })

  val sndAccess = RewriteRule("record field access", {
    case SndExprPhrase(RecordExpPhase(_, snd)) => snd
  })

  val rules: Vector[RewriteRule] = Vector(betaReduction, mapToFor, joinToFor, mapIndex, splitIndex, zipIndex, fstAccess, sndAccess)
}