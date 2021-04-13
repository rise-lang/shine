package shine.cuda.AST

import shine.C.AST.Nodes.{VisitAndGenerateStmt, VisitAndRebuild}
import shine.C.AST._
import shine.{C, cuda}

case class VarDecl(override val name: String,
                   override val t: Type,
                   addressSpace: cuda.AddressSpace,
                   override val init: Option[Expr] = None)
  extends C.AST.VarDecl(name, t, init)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): VarDecl =
    cuda.AST.VarDecl(name, v(t), addressSpace, init.map(VisitAndRebuild(_, v)))
}

case class SynchronizeThreads() extends Stmt {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): SynchronizeThreads = this

  override def visitAndGenerateStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
}

case class SynchronizeWarp() extends Stmt {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): SynchronizeWarp = this

  override def visitAndGenerateStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
}
