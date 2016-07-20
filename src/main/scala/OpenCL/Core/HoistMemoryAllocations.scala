package OpenCL.Core

import Core._
import LowLevelCombinators._

object HoistMemoryAllocations {

  type AllocationInfo = (OpenCLAddressSpace, IdentPhrase[VarType])

  def apply(originalP: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {

    var replacedAllocations = List[AllocationInfo]()

    case class Visitor(parForInfo: List[(IdentPhrase[ExpType], Nat)])
      extends VisitAndRebuild.Visitor {

      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          // remember param and length for each `par for`
          case pf: AbstractParFor =>
            pf.body match {
              case LambdaPhrase(param, _) =>
                Continue(pf, Visitor((param, pf.n) :: parForInfo))
              case _ => throw new Exception("This should not happen")
            }
          // for every new ...
          case New(dt, addressSpace, f) if addressSpace != PrivateMemory =>
            // .. investigate the nested lambda, ...
            f match {
              case LambdaPhrase(param, body) =>
                println(s"Visiting new with $param and $body")
                // ... by looking through the information from the `par for`s, ...
                val (finalParam, finalBody) = parForInfo.foldLeft((param, body)) {
                  // ... to rewrite the new's body given the oldParam, oldBody,
                  // as well as the index `i` and length `n` of a `par for` ...
                  case ((oldParam, oldBody), (i, n)) =>
                    import DSL.typed._
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
                replacedAllocations = (addressSpace.asInstanceOf[OpenCLAddressSpace], finalParam) :: replacedAllocations
                Stop(VisitAndRebuild(finalBody.asInstanceOf[Phrase[T]], this))
              case _ => throw new Exception("This should not happen")
            }
          case _ => Continue(p, this)
        }
      }

    }

    val rewrittenPhrase = VisitAndRebuild(originalP, Visitor(List()))

    (rewrittenPhrase, replacedAllocations)
//    // Create a fresh allocation for every replaced New node using initially the
//    // rewrittenPhrase and then the previous New node as its nested body
//    replacedAllocations.foldLeft(rewrittenPhrase)((prev, alloc) => {
//      val (addressSpace, identifier) = alloc
//      New(identifier.t.t1.dataType, addressSpace, LambdaPhrase(identifier, prev))
//    })
  }

}
