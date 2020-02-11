package shine.OpenCL.CodeGeneration

import arithexpr.arithmetic.ArithExpr.Math.Min
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases.{VisitAndRebuild, _}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.ImperativePrimitives.{OpenCLNew, OpenCLParFor, OpenCLParForNat}
import shine._

object HoistMemoryAllocations {

  case class AllocationInfo(addressSpace: AddressSpace,
                            identifier: Identifier[VarType])

  def apply(originalPhrase: Phrase[CommType]): (Phrase[CommType], List[AllocationInfo]) = {
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

    case class ParForInfo(parallelismLevel: shine.OpenCL.ParallelismLevel,
                          allocations: Nat,
                          allocation: Either[Identifier[ExpType],Nat])

    case class Visitor(parForInfos: List[ParForInfo]) extends VisitAndRebuild.Visitor {

      def getReplacedAllocations: List[AllocationInfo] = replacedAllocations

      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          // 1 thread only needs 1 allocation
          // `t` threads need `t` individual allocations
          // we also do not need more allocations that loop iterations
          case f: For => Continue(f,
            Visitor(ParForInfo(OpenCL.Sequential, Min(1, f.n), Right(0)) :: parForInfos))
          case f: ForNat => Continue(f,
            Visitor(ParForInfo(OpenCL.Sequential, Min(1, f.n), Right(0)) :: parForInfos))
          case pf: OpenCLParFor =>
            val t = pf.step
            Continue(pf,
              Visitor(ParForInfo(pf.parallelismLevel, Min(t, pf.n), Right(pf.init)) :: parForInfos))
          case pfn: OpenCLParForNat => ???

          case OpenCLNew(addressSpace, _, Lambda(variable, body))  if addressSpace != AddressSpace.Private =>
            Stop( // TODO? there might be fors and news in the body
              replaceNew(addressSpace.asInstanceOf[AddressSpace],
                variable, body)).asInstanceOf[Result[Phrase[T]]]

          case _ => Continue(p, this)
        }
      }

      private def replaceNew(addressSpace: AddressSpace,
                             variable: Identifier[VarType],
                             body: Phrase[CommType]): Phrase[CommType] = {
        // Replace `new` node by looking through the information from the `par for`s, ...
        val (finalVariable, finalBody) = parForInfos.foldLeft((variable, body)) {
          // ... to rewrite the new's body given the oldParam, oldBody,
          // as well as the index `i` and length `n` of a `par for` ...
          case ((oldVariable, oldBody), ParForInfo(parallelismLevel, n, i)) =>
            addressSpace match {
              case AddressSpace.Global =>
                  performRewrite(oldVariable, oldBody, i, n)
              case AddressSpace.Local =>
                parallelismLevel match {
                  case OpenCL.Local | OpenCL.Sequential =>
                    performRewrite(oldVariable, oldBody, i, n)
                  case OpenCL.WorkGroup => // do not perform the substitution
                    (oldVariable, oldBody)
                  case OpenCL.Global =>
                    throw new Exception("This should not happen")
                }
              case AddressSpace.Private | AddressSpace.Constant | AddressSpaceIdentifier(_) =>
                throw new Exception("This can't happen")
            }
        }

        // ... remember `finalVariable' to regenerate the `new' at the
        // outermost scope and return the rewritten finalBody which
        // replaces the old `new` node
        replacedAllocations = AllocationInfo(addressSpace, finalVariable) :: replacedAllocations
        VisitAndRebuild(finalBody, this)
      }

      private def performRewrite(oldVariable: Identifier[VarType],
                                 oldBody: Phrase[CommType],
                                 i: Either[Identifier[ExpType],Nat],
                                 n: Nat): (Identifier[VarType], Phrase[CommType]) = {
        // Create `newVariable' with `n` times more memory ...
        val newVariable = Identifier(oldVariable.name, varT(n`.`oldVariable.t.t1.dataType))
        // ... and substitute all occurrences of `oldVariable` with
        // `newVariable` indexed by the index `i`, ...
        val newBody = i match {
          case Left(identExpr) =>
            Phrase.substitute(
              substitutionMap = Map(
                π1(oldVariable) -> (π1(newVariable) `@` identExpr),
                π2(oldVariable) -> (π2(newVariable) `@` identExpr)
              ),
              in = oldBody
            )
          case Right(identNat) =>
            Phrase.substitute(
              substitutionMap = Map(
                π1(oldVariable) -> (π1(newVariable) `@` identNat),
                π2(oldVariable) -> (π2(newVariable) `@` identNat)
              ),
              in = oldBody
            )
        }
        // ... finally, return `newParam' and `newBody'.
        (newVariable, newBody)
      }
    }
  }

}
