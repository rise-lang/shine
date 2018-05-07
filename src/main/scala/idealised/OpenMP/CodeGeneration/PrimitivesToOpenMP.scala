package idealised.OpenMP.CodeGeneration

import idealised.DPIA.{ImperativePrimitives => DPIA}
import idealised.OpenMP.{ImperativePrimitives => OpenMP}

import idealised.C.CodeGeneration._
import idealised.C.AST._

class PrimitivesToOpenMP extends PrimitivesToC {

  override def codeGen(pf: DPIA.ParFor, block: Block, gen: CodeGenerator): Block = {
    OpenMP.ParFor(pf.n, pf.dt, pf.out, pf.body).codeGen(block, gen)
  }

}
