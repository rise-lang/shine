package idealised.OpenCL.CodeGeneration

import lift.arithmetic.NamedVar
import opencl.generator.OpenCLAST.Block

object AdaptKernelBody {

  def apply(body: Block): Block = {
    unrollPrivateArrays(body)
  }

  // Arrays in private memory are unrolled
  private def unrollPrivateArrays(body: Block): Block = {
    val (bodyWithUnrolledAllocations, loopsToUnroll) =
      unrollPrivateArrayAllocationsAndIdentifyLoopsToUnroll(body)
    unrollLoops(bodyWithUnrolledAllocations, loopsToUnroll)
  }

  // Unroll private array allocations and identify all loops which needs to be unrolled
  //
  // Returns the body of the kernel with private array allocations unrolled as well as a set of
  // loop variables indicating which loops to unroll
  private def unrollPrivateArrayAllocationsAndIdentifyLoopsToUnroll(body: Block): (Block, Set[NamedVar]) = {
    ???
  }

  // Unrolls every loop whoes loop variable is a member of the given set
  //
  // Returns the body of the kernel with the indicated loops unrolled
  private def unrollLoops(body: Block, loopVars: Set[NamedVar]): Block = {
    ???
  }
}
