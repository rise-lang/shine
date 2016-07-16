package OpenCL.Core

import Core._
import LowLevelCombinators._

import scala.language.postfixOps
import scala.language.reflectiveCalls

object AdjustMemoryAllocation {

  def apply(originalP: Phrase[CommandType]): Phrase[CommandType] = {

    var allocations = List[(AddressSpace, IdentPhrase[VarType])]()

    case class fun(env: List[(IdentPhrase[ExpType], Nat)]) extends VisitAndRebuild.fun {
      override def pre[U <: PhraseType](p: Phrase[U]): Result[Phrase[U]] = {
        p match {
          // remember param and length for each `par for`
          case pf: AbstractParFor =>
            pf.body match {
              case LambdaPhrase(param, _) =>
                Continue(pf, fun((param, pf.n) :: env))
              case _ => throw new Exception("This should not happen")
            }
          case _ => Continue(p, this)
        }
      }

      override def post[U <: PhraseType](p: Phrase[U]): Phrase[U] = {
        import DSL.typed._
        p match {
          // for every new ...
          case New(dt, addressSpace, f) if addressSpace != PrivateMemory =>
            // .. investigate the nested lambda, ...
            f match {
              case LambdaPhrase(param, body) =>
                // ... by looking through the information from the `par for`s, ...
                val (finalParam, finalBody) = env.foldLeft((param, body)) {
                  // ... to rewrite the new's body given the oldParam, oldBody,
                  // as well as the index `i` and length `n` of a `par for` ...
                  case ((oldParam, oldBody), (i, n)) =>
                    // ... create a newParam with a new type ...
                    val newDt = ArrayType(n, oldParam.t.t1.dataType)
                    val newParam = IdentPhrase(oldParam.name, VarType(newDt))
                    // ... and substitute all occurrences of the oldParam with
                    // the newParam indexed by the `par for` index, ...
                    val substitutionMap: Map[Phrase[_], Phrase[_]] = Map(
                      π1(oldParam) -> (π1(newParam) `@` i),
                      π2(oldParam) -> (π2(newParam) `@` i)
                    )
                    val newBody = Phrase.substitute(substitutionMap, oldBody)
                    // ... finally, pass the newParam and newBody along.
                    (newParam, newBody)
                }

                // ... remember the finalParam to regenerate the new at the
                // outermost scope and return the rewritten finalBody which
                // replaces the New node
                allocations = (addressSpace, finalParam) :: allocations
                finalBody.asInstanceOf[Phrase[U]]
              case _ => throw new Exception("This should not happen")
            }
          case _ => p
        }
      }
    }

    val rewrittenPhrase = VisitAndRebuild(originalP, fun(List()))
    // Create a fresh allocation for every replaced New node using initially the
    // rewrittenPhrase and then the previous New node as its nested body
    allocations.foldLeft(rewrittenPhrase)((prev, alloc) =>
      New(alloc._2.t.t1.dataType, alloc._1, LambdaPhrase(alloc._2, prev))
    )
  }

}
