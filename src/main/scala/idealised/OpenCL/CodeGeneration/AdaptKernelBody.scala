package idealised.OpenCL.CodeGeneration

import idealised.C.AST.{Nodes, _}
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
          case OpenCL.AST.VarDecl(name, _: ArrayType, OpenCL.PrivateMemory, _) =>
            privateArrayVars.add(name)

          case ArraySubscript(DeclRef(name), index) if privateArrayVars.contains(name) =>
            collectVars(index, loopVars)

          // literal arrays too
          case ArraySubscript(Literal(_), index) =>
            collectVars(index, loopVars)

          case _ =>
        }
        Continue(n, this)
      }
    }

    val v = Visitor(mutable.Set(), mutable.Set())
    C.AST.Nodes.VisitAndRebuild(body, v)
    v.loopVars.toSet
  }

  private def collectVars(e: Expr, set: mutable.Set[String]): Unit = {
    Nodes.VisitAndRebuild(e, new Nodes.VisitAndRebuild.Visitor {
      override def pre(n: Node): Result = {
        n match {
          case DeclRef(i) => set.add(i)
          case _ =>
        }
        Continue(n, this)
      }
    })
  }

  // Unroll every loop where the loop variable is a member of the given set
  //
  // Returns the body of the kernel with the indicated loops unrolled
  private def unroll(block: C.AST.Block, loopVars: Set[String]): C.AST.Block = {

    case class Visitor(privateArrayVars: mutable.Set[String], map: Map[String, Expr])
      extends C.AST.Nodes.VisitAndRebuild.Visitor
    {
      override def pre(n: Node): Result = {
        n match {
          case DeclRef(name) if map.contains(name) => Stop(map(name))

          // unroll previously identified loops
          case loop: ForLoop if loopVars.contains(loop.init.decl.name) =>
            val seq = (0 until inferLoopTripCount(loop)).foldLeft(Seq[Stmt]())( (seq, i) => {
              val nestedBlock = C.AST.Nodes.VisitAndRebuild(loop.body, Visitor(privateArrayVars, map.updated(loop.init.decl.name, Literal(i.toString))))
              nestedBlock.body.foldLeft(seq)(_ :+ _)
            })
            val result = seq.foldLeft(Comment(s"unrolled loop"): Stmt)( (stmt, v) => Stmts(stmt, v))
            Continue(result, this)

          case _ => Continue(n, this)
        }
      }
    }

    C.AST.Nodes.VisitAndRebuild(block, Visitor(mutable.Set(), Map()))
  }

  // compute the number of iterations to be performed by the given loop
  private def inferLoopTripCount(loop: ForLoop): Int = {
    // expected loop: for (int i = start; i < stop; i = step + i)

    val i = loop.init.decl.name

    val start = loop.init.decl match {
      case VarDecl(_, _, Some(Literal(l))) => l.toInt
      case _ => ???
    }

    val stop = loop.cond match {
      case BinaryExpr(DeclRef(i2), BinaryOperator.<, Literal(l))
        if i == i2 => l.toInt
      case _ => ???
    }

    val step = loop.increment match {
      case Assignment(DeclRef(i2), BinaryExpr(Literal(l), BinaryOperator.+, DeclRef(i3)))
        if i == i2 && i == i3 => l.toInt
      case _ => ???
    }

    (stop - start + step - 1) / step
  }
}
