//package idealised.OpenMP.CodeGeneration
//
//import idealised.DPIA.{ImperativePrimitives => DPIA}
//import idealised.OpenMP.{ImperativePrimitives => OpenMP}
//
//import idealised.C.CodeGeneration._
//import idealised.C.AST._
//
//class PrimitivesToOpenMP extends PrimitivesToC {
//
//  override val name: String = "OpenMP"
//
//  override def codeGen(pf: DPIA.ParFor, block: Block, gen: CodeGenerator): Block = {
//    OpenMP.ParFor(pf.n, pf.dt, pf.out, pf.body).codeGen(block, gen)
//  }
//
//  override def codeGen(pf: DPIA.ParForVec, block: Block, gen: CodeGenerator): Block = {
//    OpenMP.ParForVec(pf.n, pf.st, pf.out, pf.body).codeGen(block, gen)
//  }
//
//}
