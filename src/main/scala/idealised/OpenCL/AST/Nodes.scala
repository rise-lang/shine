package idealised.OpenCL.AST

import idealised.C
import idealised.C.AST.Nodes.VisitAndRebuild
import idealised.C.AST._
import idealised.OpenCL

case class RequiredWorkGroupSize(localSize: OpenCL.NDRange) extends Attribute {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Node = this
}

case class KernelDecl(override val name: String,
                      override val returnType: Type,
                      override val params: Seq[ParamDecl],
                      override val body: Stmt,
                      attribute: Option[Attribute])
  extends FunDecl(name, returnType, params, body)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): KernelDecl =
    KernelDecl(name, v(returnType), params.map(VisitAndRebuild(_, v)),
      VisitAndRebuild(body, v), attribute.map(VisitAndRebuild(_, v)))
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

case class ParamDecl(override val name: String,
                     override val t: Type,
                     addressSpace: OpenCL.AddressSpace)
  extends C.AST.ParamDecl(name, t)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ParamDecl =
    OpenCL.AST.ParamDecl(name, v(t), addressSpace)
}

case class Barrier(local: Boolean, global: Boolean) extends Stmt {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Barrier = this
}
