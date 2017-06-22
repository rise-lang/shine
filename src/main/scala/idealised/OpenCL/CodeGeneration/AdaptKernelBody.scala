package idealised.OpenCL.CodeGeneration

import ir.{ArrayType, Type}
import lift.arithmetic.{ArithExpr, Cst, NamedVar, Var}
import opencl.generator.OpenCLAST._
import opencl.ir.PrivateMemory

import scala.collection.mutable

object AdaptKernelBody {

  def apply(body: Block): Block = {
    unrollPrivateArrays(body)
  }

  // In OpenCL we unroll arrays in private memory
  private def unrollPrivateArrays(body: Block): Block = {
    unroll(body, identifyLoopsToUnroll(body))
  }

  // Identify all loops which needs to be unrolled
  //
  // Returns a set of loop variables indicating which loops to unroll
  private def identifyLoopsToUnroll(body: Block): Set[String] = {

    case class Visitor(privateArrayVars: mutable.Set[String],
                       loopVars: mutable.Set[String]) extends VisitBlockAndRebuild.Visitor
    {
      override def visitBlockMember(node: OclAstNode with BlockMember): BlockMemberVisitor.Result = node match {
        case VarDecl(name, _: ArrayType, _, PrivateMemory, _) => privateArrayVars.add(name)
          BlockMemberVisitor.Continue(node, this)
        case _ => BlockMemberVisitor.Continue(node, this)
      }

      override def visitExpression(expr: Expression): ExpressionVisitor.Result = expr match {
        // remember the loopVars used for accessing private arrays
        case v: VarRef if privateArrayVars.contains(v.name) =>
          v.arrayIndex match {
            case ArithExpression(ae) => ArithExpr.visit(ae, { case v: Var => loopVars.add(v.name); case _ => })
            case _ =>
          }
          ExpressionVisitor.Continue(v, this)
        case _ => ExpressionVisitor.Continue(expr, this)
      }
    }

    val v = Visitor(mutable.Set(), mutable.Set())
    VisitBlockAndRebuild.visitBlock(body, v)
    v.loopVars.toSet
  }

  // Unroll private arrays and every loop where the loop variable is a member of the given set
  //
  // Returns the body of the kernel with the private array declarations and indicated loops unrolled
  private def unroll(block: Block, loopVars: Set[String]): Block = {

    case class Visitor(privateArrayVars: mutable.Set[String], map: Map[ArithExpr, ArithExpr]) extends VisitBlockAndRebuild.Visitor
    {
      override def visitBlockMember(node: OclAstNode with BlockMember): BlockMemberVisitor.Result = node match {

        // unroll var declarations
        case VarDecl(name, at: ArrayType, _, PrivateMemory, _) => privateArrayVars.add(name)
          val size = Type.getLengths(at).map(_.evalInt).product
          val seq = (0 until size).foldLeft(Seq[VarDecl]())( (seq, i) =>
            seq :+ VarDecl(s"${name}_$i", Type.getBaseType(at), addressSpace = PrivateMemory))
          BlockMemberVisitor.AppendToBlock(seq)

        // unroll previously identified loops
        case loop: ForLoop if loopVars.contains(loop.init.name) =>
          val seq = (0 until inferLoopTripCount(loop)).foldLeft(Seq[OclAstNode with BlockMember]())( (seq, i) => {
            val nestedBlock = VisitBlockAndRebuild.visitBlock(loop.body,
              Visitor(privateArrayVars, map.updated(NamedVar(loop.init.name), i)))
            // remove nested block by appending nested nodes to this block
            nestedBlock.content.filterNot(_.isInstanceOf[Skip]).foldLeft(seq)(_ :+ _)
          })
          BlockMemberVisitor.AppendToBlock(seq)

        // do nothing for the rest
        case _ => BlockMemberVisitor.Continue(node, this)
      }

      override def visitExpression(expr: Expression): ExpressionVisitor.Result = expr match {
        // perform the substitutions in the arithmetic expressions
        case v: VarRef =>
          v.arrayIndex match {
            case ArithExpression(ae) =>
              ArithExpr.substitute(ae, map) match {
                // append the index as a suffix when the arithmetic expression is reduced to a
                // constant (i.e. all variables have been substituted)
                case Cst(i) if privateArrayVars.contains(v.name) =>
                  ExpressionVisitor.Stop(VarRef(v.name, s"_$i", null))
                case newAe =>
                  ExpressionVisitor.Stop(VarRef(v.name, v.suffix, ArithExpression(newAe)))
              }
            case _ =>
              ExpressionVisitor.Continue(expr, this)
           }
        case v: VLoad =>
           ExpressionVisitor.Stop(
             VLoad(v.v, v.t, ArithExpression(ArithExpr.substitute(v.offset.content, map))))

        case _ => ExpressionVisitor.Continue(expr, this)
      }
    }

    VisitBlockAndRebuild.visitBlock(block, Visitor(mutable.Set(), Map()))
  }

  // compute the number of iterations to be performed by the given loop
  private def inferLoopTripCount(loop: ForLoop): Int = {
    val start = loop.init.init match {
      case ArithExpression(ae) => ae.evalInt
      case _ => throw new Exception("")
    }
    val stop = loop.cond match {
      case CondExpression(_, ArithExpression(ae), CondExpression.Operator.<) =>
        ae.evalInt
      case _ => throw new Exception("")
    }
    val step = loop.increment match {
      // assuming steps of the form: i = i + 1
      case AssignmentExpression(ArithExpression(lhs: Var), ArithExpression(rhs)) =>
        ArithExpr.substitute(rhs, Map(lhs -> Cst(0))).evalInt
      case _ => throw new Exception("")
    }
    (stop - start) / step
  }
}

object VisitBlockAndRebuild {

  class Visitor {
    def visitBlockMember(blockMember: OclAstNode with BlockMember): BlockMemberVisitor.Result =
      BlockMemberVisitor.Continue(blockMember, this)

    object BlockMemberVisitor {
      sealed trait Result
      case class AppendToBlock(blockMembers: Seq[OclAstNode with BlockMember]) extends Result
      case class Continue(blockMember: OclAstNode with BlockMember, visitor: Visitor) extends Result
    }

    def visitExpression(expr: Expression): ExpressionVisitor.Result =
      ExpressionVisitor.Continue(expr, this)

    object ExpressionVisitor {
      sealed trait Result
      case class Stop(expr: Expression) extends Result
      case class Continue(expr: Expression, visitor: Visitor) extends Result
    }
  }

  def visitBlock(block: Block, visitor: Visitor): Block = {
    val newBlock = Block(global = block.global)
    block.content.foreach(visitBlockMember(_, newBlock, visitor))
    newBlock
  }

  private def visitExpression(expression: Expression, visitor: Visitor): Expression = {
    visitor.visitExpression(expression) match {
      case r: visitor.ExpressionVisitor.Stop => r.expr
      case c: visitor.ExpressionVisitor.Continue =>
        val visitor = c.visitor
        c.expr match {
          case _: Literal | _: VarRef | _: OpenCLExpression | _: ArithExpression => expression
          case null => null
          case u: UnaryExpression => UnaryExpression(u.op, visitExpression(u.e, visitor))
          case vl: VLoad => VLoad(visitVarRef(vl.v, visitor), vl.t, vl.offset)
          case f: FunctionCall => FunctionCall(f.name, f.args.map(visitExpression(_, visitor)))
          case vs: VStore => VStore(visitVarRef(vs.v, visitor), vs.t,
            visitExpression(vs.value, visitor), vs.offset)
          case c: Cast => Cast(visitVarRef(c.v, visitor), c.t)
          case pc: PointerCast => PointerCast(visitVarRef(pc.v, visitor), pc.t, pc.addressSpace)
          case s: Store => Store(
            visitVarRef(s.v, visitor), s.t, visitExpression(s.value, visitor), s.offset, s.openCLAddressSpace)
          case b: BinaryExpression => BinaryExpression(b.op,
            visitExpression(b.lhs, visitor), visitExpression(b.rhs, visitor))
          case a: AssignmentExpression => visitAssignmentExpression(a, visitor)
          case l: Load => Load(visitVarRef(l.v, visitor), l.t, l.offset, l.openCLAddressSpace)
          case vl: VectorLiteral => VectorLiteral(vl.t, vl.vs.map(visitExpression(_, visitor)):_*)
          case s: StructConstructor => StructConstructor(s.t, s.args.map(visitExpression(_, visitor)))
          case c: CondExpression => visitCondExpression(c, visitor)
        }
    }
  }

  private def visitAssignmentExpression(a: AssignmentExpression, visitor: Visitor): AssignmentExpression =
    AssignmentExpression(visitExpression(a.to, visitor), visitExpression(a.value, visitor))

  private def visitCondExpression(c: CondExpression, visitor: Visitor): CondExpression =
    CondExpression(visitExpression(c.lhs, visitor), visitExpression(c.rhs, visitor), c.cond)

  private def visitVarRef(varRef: VarRef, visitor: Visitor): VarRef = varRef

  private def visitDeclaration(declaration: Declaration, visitor: Visitor): Declaration = declaration  match {
    case _: Label => declaration
    case f: Function => Function(f.name, f.ret,
      f.params.map(visitParamDecl(_, visitor)), visitBlock(f.body, visitor),
      f.kernel, f.attribute.map(visitAttribute(_, visitor)))
    case vd: VarDecl => visitVarDecl(vd, visitor)
    case pd: ParamDecl => visitParamDecl(pd, visitor)
  }

  private def visitVarDecl(vd: VarDecl, visitor: Visitor): VarDecl =
    VarDecl(vd.name, vd.t, visitExpression(vd.init, visitor), vd.addressSpace, vd.length)

  private def visitAttribute(attribute: Attribute, visitor: Visitor): Attribute = attribute

  private def visitParamDecl(paramDecl: ParamDecl, visitor: Visitor): ParamDecl =
    ParamDecl(paramDecl.name, paramDecl.t, paramDecl.addressSpace, paramDecl.const)

  private def visitBlockMember(node: OclAstNode with BlockMember,
                               block: Block,
                               visitor: Visitor): Unit = {
    visitor.visitBlockMember(node) match {
      case r: visitor.BlockMemberVisitor.AppendToBlock => r.blockMembers.foreach((block: Block) += _)
      case c: visitor.BlockMemberVisitor.Continue =>
        val visitor = c.visitor
        (block: Block) += (c.blockMember match {
          case s: Statement => s match {
            case _: GOTO | _: Break | _: TypeDef | _: TupleAlias | _: Barrier => s
            case i: IfThenElse => IfThenElse(
              visitExpression(i.cond, visitor), visitBlock(i.trueBody, visitor), visitBlock(i.falseBody, visitor))
            case b: Block => visitBlock(b, visitor)
            case es: ExpressionStatement =>
              ExpressionStatement(visitExpression(es.e, visitor))
            case f: ForLoop => ForLoop(
              visitVarDecl(f.init, visitor),
              visitCondExpression(f.cond, visitor),
              visitAssignmentExpression(f.increment, visitor),
              visitBlock(f.body, visitor))
            case w: WhileLoop => WhileLoop(w.loopPredicate, visitBlock(w.body, visitor))
          }
          case d: Declaration => visitDeclaration(d, visitor)
          case _: OpenCLExtension | _: OpenCLCode | _: Skip | _: Comment => c.blockMember
        })
    }
  }

}
