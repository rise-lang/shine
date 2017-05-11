package idealised.OpenCL.Core

import idealised._
import idealised.Core._
import idealised.ImperativePrimitives._
import idealised.OpenCL.LowLevelPrimitives.OpenCLParFor

object HoistMemoryAllocations {

  case class AllocationInfo(addressSpace: idealised.OpenCL.AddressSpace,
                            identifier: Identifier[VarType])

  def apply(originalPhrase: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {
    val visitor = new VisitorScope(List[AllocationInfo]()).Visitor(List())

    val rewrittenPhrase = VisitAndRebuild(originalPhrase, visitor)

    (rewrittenPhrase, visitor.getReplacedAllocations)
    //    // Create a fresh allocation for every replaced New node using initially the
    //    // rewrittenPhrase and then the previous New node as its nested body
    //    replacedAllocations.foldLeft(rewrittenPhrase)((prev, alloc) => {
    //      val (addressSpace, identifier) = alloc
    //      New(identifier.t.t1.dataType, addressSpace, LambdaPhrase(identifier, prev))
    //    })
  }

  private class VisitorScope(var replacedAllocations: List[AllocationInfo]) {

    case class ParForInfo(parallelismLevel: idealised.OpenCL.ParallelismLevel,
                          param: Identifier[ExpType],
                          length: Nat)

    case class Visitor(parForInfos: List[ParForInfo]) extends VisitAndRebuild.Visitor {

      def getReplacedAllocations: List[AllocationInfo] = replacedAllocations

      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          // remember param and length for each `par for`
          case pf: OpenCLParFor =>
            pf.body match {
              case Lambda(param, _) =>
                Continue(pf,
                  Visitor(ParForInfo(pf.parallelismLevel, param, pf.n) :: parForInfos))
              case _ => throw new Exception("This should not happen")
            }
          case New(_, addressSpace, f) if addressSpace != OpenCL.PrivateMemory =>
            f match {
              case Lambda(param, body) =>
                Stop(
                  replaceNew(addressSpace.asInstanceOf[idealised.OpenCL.AddressSpace],
                    param, body)).asInstanceOf[Result[Phrase[T]]]
              case _ => throw new Exception("This should not happen")
            }
          case _ => Continue(p, this)
        }
      }

      private def replaceNew(addressSpace: idealised.OpenCL.AddressSpace,
                             param: Identifier[VarType],
                             body: Phrase[CommandType]): Phrase[CommandType] = {
        // Replace `new` node by looking through the information from the `par for`s, ...
        val (finalParam, finalBody) = parForInfos.foldLeft((param, body)) {
          // ... to rewrite the new's body given the oldParam, oldBody,
          // as well as the index `i` and length `n` of a `par for` ...
          case ((oldParam, oldBody), ParForInfo(parallelismLevel, i, n)) =>
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

        // ... remember `finalParam' to regenerate the `new' at the
        // outermost scope and return the rewritten finalBody which
        // replaces the old `new` node
        replacedAllocations = AllocationInfo(addressSpace, finalParam) :: replacedAllocations
        VisitAndRebuild(finalBody, this)
      }

      private def performRewrite(oldParam: Identifier[VarType],
                                 oldBody: Phrase[CommandType],
                                 i: Identifier[ExpType],
                                 n: Nat): (Identifier[VarType], Phrase[CommandType]) = {
        import idealised.DSL.typed._
        // Create `newParam' with a new type ...
        val newParam = Identifier(oldParam.name, VarType(dt=ArrayType(n, oldParam.t.t1.dataType)))
        // ... and substitute all occurrences of the oldParam with
        // the newParam indexed by the `par for` index, ...
        val newBody = Phrase.substitute(
          substitutionMap = Map(
            π1(oldParam) -> (π1(newParam) `@` i),
            π2(oldParam) -> (π2(newParam) `@` i)
          ),
          in = oldBody
        )
        // ... finally, return `newParam' and `newBody'.
        (newParam, newBody)
      }

    }

  }

}
