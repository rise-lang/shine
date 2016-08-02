package idealised.OpenCL.Core

import idealised._
import idealised.Core._
import idealised.LowLevelCombinators._
import idealised.OpenCL.LowLevelCombinators.OpenCLParFor

object HoistMemoryAllocations {

  type AllocationInfo = (idealised.OpenCL.AddressSpace, IdentPhrase[VarType])

  def apply(originalP: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {

    var replacedAllocations = List[AllocationInfo]()

    case class Visitor(parForInfo: List[(idealised.OpenCL.ParallelismLevel, IdentPhrase[ExpType], Nat)])
      extends VisitAndRebuild.Visitor {

      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          // remember param and length for each `par for`
          case pf: OpenCLParFor =>
            pf.body match {
              case LambdaPhrase(param, _) =>
                Continue(pf, Visitor((pf.parallelismLevel, param, pf.n) :: parForInfo))
              case _ => throw new Exception("This should not happen")
            }
          case New(dt, addressSpace, f) if addressSpace != OpenCL.PrivateMemory =>
            f match {
              case LambdaPhrase(param, body) =>
                replaceNew(addressSpace.asInstanceOf[idealised.OpenCL.AddressSpace],
                  param, body).asInstanceOf[Result[Phrase[T]]]
              case _ => throw new Exception("This should not happen")
            }
          case _ => Continue(p, this)
        }
      }

      private def replaceNew(addressSpace: idealised.OpenCL.AddressSpace,
                             param: IdentPhrase[VarType],
                             body: Phrase[CommandType]): Result[Phrase[CommandType]] = {
        // Replace `new` node by looking through the information from the `par for`s, ...
        val (finalParam, finalBody) = parForInfo.foldLeft((param, body)) {
          // ... to rewrite the new's body given the oldParam, oldBody,
          // as well as the index `i` and length `n` of a `par for` ...
          case ((oldParam, oldBody), (parallelismLevel, i, n)) =>
            addressSpace match {
              case OpenCL.GlobalMemory =>
                performRewrite(oldParam, oldBody, i, n)
              case OpenCL.LocalMemory =>
                parallelismLevel match {
                  case OpenCL.Local | OpenCL.Sequential =>
                    performRewrite(oldParam, oldBody, i, n)
                  case OpenCL.WorkGroup => // do not perform the substitution
                    (oldParam, oldBody)
                  case OpenCL.Global =>
                    throw new Exception("This should not happen")
                }
              case OpenCL.PrivateMemory =>
                throw new Exception("This can't happen")
            }
        }

        // ... remember the finalParam to regenerate the new at the
        // outermost scope and return the rewritten finalBody which
        // replaces the `new` node
        replacedAllocations = (addressSpace, finalParam) :: replacedAllocations
        Stop(VisitAndRebuild(finalBody, this))
      }

      private def performRewrite(oldParam: IdentPhrase[VarType],
                                 oldBody: Phrase[CommandType],
                                 i: IdentPhrase[ExpType],
                                 n: Nat): (IdentPhrase[VarType], Phrase[CommandType]) = {
        import idealised.DSL.typed._
        // Create a newParam with a new type ...
        val newDt = ArrayType(n, oldParam.t.t1.dataType)
        val newParam = IdentPhrase(oldParam.name, VarType(newDt))
        // ... and substitute all occurrences of the oldParam with
        // the newParam indexed by the `par for` index, ...
        val substitutionMap: Map[Phrase[_], Phrase[_]] = Map(
          π1(oldParam) -> (π1(newParam) `@` i),
          π2(oldParam) -> (π2(newParam) `@` i)
        )
        val newBody = Phrase.substitute(substitutionMap, oldBody)
        // ... finally, return the newParam and newBody along.
        (newParam, newBody)
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
