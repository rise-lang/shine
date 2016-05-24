package Rewriting

import Core._
import DSL._

import ExpPatterns._
import CommandPatterns._

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
    case Assign(out, Map(f, array)) =>
      `for`(length(out), { i =>
        out `@` i := ApplyPhrase(f, array `@` i)
      })
  })

  val joinToFor = RewriteRule("join to for", {
    case Assign(out, Join(array)) =>
      `for`(length(array), { i =>
        `for`(length(array `@` i), { j =>
          out `@` (i*length(array `@` i)+j) := (array `@` j) `@` j
        })
      })
  })

  val mapIndex = RewriteRule("map index", {
    case Idx(Map(f, array), i) =>
      ApplyPhrase(f, array `@` i)
  })

  val splitIndex = RewriteRule("split index", {
    case Idx(Idx(Split(n, array), i), j) =>
      array `@` (i * n + j)
  })

  val zipIndex = RewriteRule("zip index", {
    case Idx(Zip(lhs, rhs), i) =>
      Record(lhs `@` i, rhs `@` i)
  })

  val fstAccess = RewriteRule("record field access", {
    case Fst(Record(fst, _)) => fst
  })

  val sndAccess = RewriteRule("record field access", {
    case Snd(Record(_, snd)) => snd
  })

  val rules: Vector[RewriteRule] = Vector(betaReduction, mapToFor, joinToFor, mapIndex, splitIndex, zipIndex, fstAccess, sndAccess)
}