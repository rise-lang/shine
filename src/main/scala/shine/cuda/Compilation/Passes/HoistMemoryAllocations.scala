package shine.cuda.Compilation.Passes

import arithexpr.arithmetic.ArithExpr.Math.Min
import shine.DPIA.DSL._
import shine.DPIA.Phrases.{Identifier, Lambda, Phrase, VisitAndRebuild}
import rise.core.types._
import rise.core.DSL.Type._
import shine.DPIA.Types.{CommType, ExpType, PhraseType}
import shine.DPIA.primitives.imperative.{For, ForNat}
import shine.DPIA.{Nat, VarType, varT}
import shine.OpenCL
import shine.OpenCL.Compilation.Passes.HoistMemoryAllocations.AllocationInfo
import shine.OpenCL.primitives.imperative.New

//TODO similar to OpenCL HoistMemoryAllocations
object HoistMemoryAllocations {

  def hoist: Phrase[CommType] => (scala.Seq[AllocationInfo], Phrase[CommType]) = originalPhrase => {
    val visitor = new VisitorScope(List[AllocationInfo]()).Visitor(List())

    val rewrittenPhrase = VisitAndRebuild(originalPhrase, visitor)

    (visitor.getReplacedAllocations, rewrittenPhrase)
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
                          allocation: Either[Identifier[ExpType], Nat])

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
          case pf: shine.OpenCL.primitives.imperative.ParFor =>
            val t = pf.step
            Continue(pf,
              Visitor(ParForInfo(pf.level, Min(t, pf.n), Right(pf.init)) :: parForInfos))
          case pf: shine.cuda.primitives.imperative.ParFor =>
            val t = pf.step
            Continue(pf,
              Visitor(ParForInfo(pf.level, Min(t, pf.n), Right(pf.init)) :: parForInfos))
          case pf: shine.OpenCL.primitives.imperative.ParForNat =>
            Continue(pf,
              Visitor(ParForInfo(pf.level, Min(pf.step, pf.n), Right(pf.init)) :: parForInfos))

          case New(addressSpace, _, Lambda(variable, body)) if addressSpace != AddressSpace.Private
            && addressSpace != AddressSpace.Local =>
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
              case AddressSpace.Local | AddressSpace.Private | AddressSpace.Constant | AddressSpaceIdentifier(_) =>
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
                                 i: Either[Identifier[ExpType], Nat],
                                 n: Nat): (Identifier[VarType], Phrase[CommType]) = {
        // Create `newVariable' with `n` times more memory ...
        val newVariable = Identifier(oldVariable.name, varT(n `.` oldVariable.t.t1.dataType))
        // ... and substitute all occurrences of `oldVariable` with
        // `newVariable` indexed by the index `i`, ...
        val newBody = i match {
          case Left(identExpr) =>
            Phrase.substitute(
              substitutionMap = Map(
                oldVariable.rd -> (newVariable.rd `@` identExpr),
                oldVariable.wr -> (newVariable.wr `@` identExpr)
              ),
              in = oldBody
            )
          case Right(identNat) =>
            Phrase.substitute(
              substitutionMap = Map(
                oldVariable.rd -> (newVariable.rd `@` identNat),
                oldVariable.wr -> (newVariable.wr `@` identNat)
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
