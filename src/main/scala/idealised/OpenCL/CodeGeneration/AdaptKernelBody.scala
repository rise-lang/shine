package idealised.OpenCL.CodeGeneration

import idealised.C.AST._
import idealised.OpenCL.AddressSpace
import idealised.{C, OpenCL}
import lift.arithmetic.{IfThenElse => _, _}

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
          case OpenCL.AST.VarDecl(name, _: ArrayType, AddressSpace.Private, _) =>  privateArrayVars.add(name)

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
                  case Cst(_) if privateArrayVars.contains(name) => Stop(DeclRef(s"${name}_i"))
                  case newAe => Stop(ArraySubscript(DeclRef(name), ArithmeticExpr(newAe)))
                }
              case _ => Continue(n, this)
            }

          //        case v: VLoad =>
          //           ExpressionVisitor.Stop(
          //             VLoad(v.v, v.t, ArithExpression(ArithExpr.substitute(v.offset.content, map))))

          // unroll var declarations
          case OpenCL.AST.VarDecl(name, at: ArrayType, AddressSpace.Private, _) =>
            privateArrayVars.add(name)
            val size = Type.getLengths(at).map(_.evalInt).product
            val seq = (0 until size).foldLeft(Seq[OpenCL.AST.VarDecl]())( (seq, i) =>
              seq :+ OpenCL.AST.VarDecl(s"${name}_$i", Type.getBaseType(at), AddressSpace.Private))
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
          // TODO: generalise this
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
