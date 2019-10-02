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

case class VectorLiteral(t: VectorType, values: Seq[Expr]) extends Expr {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): VectorLiteral =
    VectorLiteral(v(t).asInstanceOf[VectorType], values.map(VisitAndRebuild(_, v)))

  override def visitAndGenerateStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = {
    def rec(todo: Seq[Expr], done: Seq[Expr]): Stmt = {
      todo match {
        case Nil => cont(VectorLiteral(t, done))
        case e +: rest => VisitAndGenerateStmt(e, v, e2 => rec(rest, done :+ e2))
      }
    }

    rec(values, Seq())
  }
}

case class VectorSubscript(vector: Expr, index: Expr) extends Expr {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Node =
    VectorSubscript(VisitAndRebuild(vector, v), VisitAndRebuild(index, v))

  override def visitAndGenerateStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
    VisitAndGenerateStmt(vector, v, vector2 =>
      VisitAndGenerateStmt(index, v, index2 =>
        cont(VectorSubscript(vector2, index2))))
}