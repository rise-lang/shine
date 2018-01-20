package idealised.OpenMP.CodeGeneration

import idealised.DPIA.ImperativePrimitives.ParFor

import idealised.OpenMP.ImperativePrimitives.OpenMPParFor

import idealised.C.CodeGeneration._
import idealised.C.AST._

class PrimitivesToOpenMP extends PrimitivesToC {

  override def codeGen(pf: ParFor, block: Block, gen: CodeGenerator): Block = {
    OpenMPParFor(pf.n, pf.dt, pf.out, pf.body).codeGen(block, gen)
  }

}
