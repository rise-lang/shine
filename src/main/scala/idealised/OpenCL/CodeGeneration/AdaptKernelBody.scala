package idealised.OpenCL.CodeGeneration

import idealised.C.AST._
import opencl.generator.OclFunction
import opencl.generator.OpenCLAST.VarRef
//import ir.{ArrayType, Type}
import lift.arithmetic.{IfThenElse => _, _}
//import opencl.generator.OclFunction
//import opencl.generator.OpenCLAST._
import idealised.{C, OpenCL}
//import opencl.ir.PrivateMemory

import scala.collection.mutable

object AdaptKernelBody {

  def apply(body: C.AST.Block): C.AST.Block = {
    unrollPrivateArrays(body)
  }

  // In OpenCL we unroll arrays in private memory
  private def unrollPrivateArrays(body: C.AST.Block): C.AST.Block = {
    unroll(body, identifyLoopsToUnroll(body))
  }

  // Identify all loops which needs to be unrolled
  //
  // Returns a set of loop variables indicating which loops to unroll
  private def identifyLoopsToUnroll(body: C.AST.Block): Set[String] = {

    case class Visitor(privateArrayVars: mutable.Set[String],
                       loopVars: mutable.Set[String]) extends C.AST.Nodes.VisitAndRebuild.Visitor
    {
      override def pre(n: Node): Result = {
        n match {
          case OpenCL.AST.VarDecl(name, _: ArrayType, OpenCL.PrivateMemory, _) =>  privateArrayVars.add(name)

          case ArraySubscript(DeclRef(name), index) if privateArrayVars.contains(name) =>
            index match {
              case ArithmeticExpr(ae) => ArithExpr.visit(ae, { case v: Var => loopVars.add(v.name); case _ => })
              case _ =>
            }

          case _ =>
        }
        Continue(n, this)
      }
    }

    val v = Visitor(mutable.Set(), mutable.Set())
    C.AST.Nodes.VisitAndRebuild(body, v)
    v.loopVars.toSet
  }

  // Unroll private arrays and every loop where the loop variable is a member of the given set
  //
  // Returns the body of the kernel with the private array declarations and indicated loops unrolled
  private def unroll(block: C.AST.Block, loopVars: Set[String]): C.AST.Block = {

    case class Visitor(privateArrayVars: mutable.Set[String], map: Map[ArithExpr, ArithExpr])
      extends C.AST.Nodes.VisitAndRebuild.Visitor
    {
      override def pre(n: Node): Result = {
        n match {

          case ArraySubscript(DeclRef(name), index) =>
            index match {
              case ArithmeticExpr(ae) =>
                ArithExpr.substitute(ae, map) match {
                  // append the index as a suffix when the arithmetic expression is reduced to a
                  // constant (i.e. all variables have been substituted)
                  case Cst(i) if privateArrayVars.contains(name) => Stop(DeclRef(s"${name}_i"))
                  case newAe => Stop(ArraySubscript(DeclRef(name), ArithmeticExpr(newAe)))
                }
              case _ => Continue(n, this)
            }

          //        case v: VLoad =>
          //           ExpressionVisitor.Stop(
          //             VLoad(v.v, v.t, ArithExpression(ArithExpr.substitute(v.offset.content, map))))

          // unroll var declarations
          case OpenCL.AST.VarDecl(name, at: ArrayType, OpenCL.PrivateMemory, _) =>
            privateArrayVars.add(name)
            val size = Type.getLengths(at).map(_.evalInt).product
            val seq = (0 until size).foldLeft(Seq[OpenCL.AST.VarDecl]())( (seq, i) =>
              seq :+ OpenCL.AST.VarDecl(s"${name}_$i", Type.getBaseType(at), OpenCL.PrivateMemory))
            seq.foldLeft(Comment(s"unrolled $name"): Stmt)( (stmt, v) => Stmts(stmt, DeclStmt(v)))
            Continue(n, this)

          // unroll previously identified loops
          case loop: ForLoop if loopVars.contains(loop.init.decl.name) =>
            val seq = (0 until inferLoopTripCount(loop)).foldLeft(Seq[Stmt]())( (seq, i) => {
              val nestedBlock = C.AST.Nodes.VisitAndRebuild(loop.body, Visitor(privateArrayVars, map.updated(NamedVar(loop.init.decl.name), i)))
              nestedBlock.body.foldLeft(seq)(_ :+ _)
            })
            seq.foldLeft(Comment(s"unrolled loop"): Stmt)( (stmt, v) => Stmts(stmt, v))
            Continue(n, this)

          case _ => Continue(n, this)
        }
      }
    }

    C.AST.Nodes.VisitAndRebuild(block, Visitor(mutable.Set(), Map()))
  }

  // compute the number of iterations to be performed by the given loop
  private def inferLoopTripCount(loop: ForLoop): Int = {
    val start = loop.init.decl match {
      case VarDecl(_, _, Some(ArithmeticExpr(ae))) => ae match {
        case ArithExprFunction(_, range) => range.min.evalInt
        case _ => ae.evalInt
      }
      case _ => ???
    }

    val stop = loop.cond match {
      case C.AST.BinaryExpr(_, C.AST.BinaryOperator.<, ArithmeticExpr(ae)) => ae match {
        case ArithExprFunction(_, range) => range.max.evalInt
        case _ => ae.evalInt
      }
      case _ => ???
    }

    val step = loop.increment match {
      // assuming steps of the form: i = i + 1
      case Assignment(DeclRef(name), ArithmeticExpr(rhs)) =>
        val ae = ArithExpr.substitute(rhs, Map(NamedVar(name) -> Cst(0)))
        ae match {
          // TODO: generallise this
          case ArithExprFunction(_, range) => range match {
            case RangeAdd(min, max, s) if (min.evalInt + 1) == max.evalInt && s.evalInt == 1 =>
              min.evalInt
            case _ => ???
          }
          case _ => ae.evalInt
        }
      case _ => ???
    }

    (stop - start) / step
  }
}

//object VisitBlockAndRebuild {
//
//  class Visitor {
//    def visitBlockMember(blockMember: OclAstNode with BlockMember): BlockMemberVisitor.Result =
//      BlockMemberVisitor.Continue(blockMember, this)
//
//    object BlockMemberVisitor {
//      sealed trait Result
//      case class AppendToBlock(blockMembers: Seq[OclAstNode with BlockMember]) extends Result
//      case class Continue(blockMember: OclAstNode with BlockMember, visitor: Visitor) extends Result
//    }
//
//    def visitExpression(expr: Expression): ExpressionVisitor.Result =
//      ExpressionVisitor.Continue(expr, this)
//
//    object ExpressionVisitor {
//      sealed trait Result
//      case class Stop(expr: Expression) extends Result
//      case class Continue(expr: Expression, visitor: Visitor) extends Result
//    }
//  }
//
//  def visitBlock(block: Block, visitor: Visitor): Block = {
//    val newBlock = Block(global = block.global)
//    block.content.foreach(visitBlockMember(_, newBlock, visitor))
//    newBlock
//  }
//
//  private def visitExpression(expression: Expression, visitor: Visitor): Expression = {
//    visitor.visitExpression(expression) match {
//      case r: visitor.ExpressionVisitor.Stop => r.expr
//      case c: visitor.ExpressionVisitor.Continue =>
//        val visitor = c.visitor
//        c.expr match {
//          case _: Literal | _: VarRef | _: OpenCLExpression | _: ArithExpression => expression
//          case null => null
//          case u: UnaryExpression => UnaryExpression(u.op, visitExpression(u.e, visitor))
//          case vl: VLoad => VLoad(visitVarRef(vl.v, visitor), vl.t, vl.offset)
//          case f: FunctionCall => FunctionCall(f.name, f.args.map(visitExpression(_, visitor)))
//          case vs: VStore => VStore(visitVarRef(vs.v, visitor), vs.t,
//            visitExpression(vs.value, visitor), vs.offset)
//          case c: Cast => Cast(visitVarRef(c.v, visitor), c.t)
//          case pc: PointerCast => PointerCast(visitVarRef(pc.v, visitor), pc.t, pc.addressSpace)
//          case s: Store => Store(
//            visitVarRef(s.v, visitor), s.t, visitExpression(s.value, visitor), s.offset, s.openCLAddressSpace)
//          case b: BinaryExpression => BinaryExpression(b.op,
//            visitExpression(b.lhs, visitor), visitExpression(b.rhs, visitor))
//          case a: AssignmentExpression => visitAssignmentExpression(a, visitor)
//          case l: Load => Load(visitVarRef(l.v, visitor), l.t, l.offset, l.openCLAddressSpace)
//          case vl: VectorLiteral => VectorLiteral(vl.t, vl.vs.map(visitExpression(_, visitor)):_*)
//          case s: StructConstructor => StructConstructor(s.t, s.args.map(visitExpression(_, visitor)))
//          case c: CondExpression => visitCondExpression(c, visitor)
//        }
//    }
//  }
//
//  private def visitAssignmentExpression(a: AssignmentExpression, visitor: Visitor): AssignmentExpression =
//    AssignmentExpression(visitExpression(a.to, visitor), visitExpression(a.value, visitor))
//
//  private def visitCondExpression(c: CondExpression, visitor: Visitor): CondExpression =
//    CondExpression(visitExpression(c.lhs, visitor), visitExpression(c.rhs, visitor), c.cond)
//
//  private def visitVarRef(varRef: VarRef, visitor: Visitor): VarRef = varRef
//
//  private def visitDeclaration(declaration: Declaration, visitor: Visitor): Declaration = declaration  match {
//    case _: Label => declaration
//    case f: Function => Function(f.name, f.ret,
//      f.params.map(visitParamDecl(_, visitor)), visitBlock(f.body, visitor),
//      f.kernel, f.attribute.map(visitAttribute(_, visitor)))
//    case vd: VarDecl => visitVarDecl(vd, visitor)
//    case pd: ParamDecl => visitParamDecl(pd, visitor)
//  }
//
//  private def visitVarDecl(vd: VarDecl, visitor: Visitor): VarDecl =
//    VarDecl(vd.name, vd.t, visitExpression(vd.init, visitor), vd.addressSpace, vd.length)
//
//  private def visitAttribute(attribute: Attribute, visitor: Visitor): Attribute = attribute
//
//  private def visitParamDecl(paramDecl: ParamDecl, visitor: Visitor): ParamDecl =
//    ParamDecl(paramDecl.name, paramDecl.t, paramDecl.addressSpace, paramDecl.const)
//
//  private def visitBlockMember(node: OclAstNode with BlockMember,
//                               block: Block,
//                               visitor: Visitor): Unit = {
//    visitor.visitBlockMember(node) match {
//      case r: visitor.BlockMemberVisitor.AppendToBlock => r.blockMembers.foreach((block: Block) += _)
//      case c: visitor.BlockMemberVisitor.Continue =>
//        val visitor = c.visitor
//        (block: Block) += (c.blockMember match {
//          case s: Statement => s match {
//            case _: GOTO | _: Break | _: TypeDef | _: TupleAlias | _: Barrier => s
//            case i: IfThenElse => IfThenElse(
//              visitExpression(i.cond, visitor), visitBlock(i.trueBody, visitor), visitBlock(i.falseBody, visitor))
//            case b: Block => visitBlock(b, visitor)
//            case es: ExpressionStatement =>
//              ExpressionStatement(visitExpression(es.e, visitor))
//            case f: ForLoop => ForLoop(
//              visitVarDecl(f.init, visitor),
//              visitCondExpression(f.cond, visitor),
//              visitAssignmentExpression(f.increment, visitor),
//              visitBlock(f.body, visitor))
//            case w: WhileLoop => WhileLoop(w.loopPredicate, visitBlock(w.body, visitor))
//          }
//          case d: Declaration => visitDeclaration(d, visitor)
//          case _: OpenCLExtension | _: OpenCLCode | _: Skip | _: Comment => c.blockMember
//        })
//    }
//  }
//
//}
