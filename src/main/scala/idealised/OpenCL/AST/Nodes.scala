package idealised.OpenCL.AST

import idealised.C
import idealised.C.AST.Nodes.{VisitAndGenerateStmt, VisitAndRebuild}
import idealised.C.AST._
import idealised.OpenCL

case class RequiredWorkGroupSize(localSize: OpenCL.NDRange)

case class KernelDecl(override val name: String,
                      override val params: Seq[ParamDecl],
                      override val body: Stmt,
                      attribute: Option[RequiredWorkGroupSize])
  extends FunDecl(name, C.AST.Type.void, params, body)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): KernelDecl =
    KernelDecl(name, params.map(VisitAndRebuild(_, v)),
      VisitAndRebuild(body, v), attribute)
}

case class VarDecl(override val name: String,
                   override val t: Type,
                   addressSpace: OpenCL.AddressSpace,
                   override val init: Option[Expr] = None)
  extends C.AST.VarDecl(name, t, init)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): VarDecl =
    OpenCL.AST.VarDecl(name, v(t), addressSpace, init.map(VisitAndRebuild(_, v)))
}

//case class ParamDecl(override val name: String,
//                     override val t: Type,
//                     addressSpace: OpenCL.AddressSpace)
//  extends C.AST.ParamDecl(name, t)
//{
//  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ParamDecl =
//    OpenCL.AST.ParamDecl(name, v(t), addressSpace)
//}

case class Barrier(local: Boolean, global: Boolean) extends Stmt {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Barrier = this

  override def visitAndGenerateStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
}
