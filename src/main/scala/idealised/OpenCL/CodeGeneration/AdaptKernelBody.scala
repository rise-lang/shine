package idealised.OpenCL.CodeGeneration

import idealised.C.AST.{Nodes, _}
import idealised.DPIA.Types.AddressSpace
import idealised.{C, OpenCL}
import lift.arithmetic.{IfThenElse => _, _}

import scala.collection.mutable

//
// This performs transformations to ensure:
//   - arrays in private memory are enrolled
//   - "variables in the local address space can only be declared in the outermost scope of a kernel function"
//
object AdaptKernelBody {

  def apply(body: C.AST.Block): C.AST.Block = {
    body |>
      (unrollPrivateArrays(_)) |>
      (moveLocalMemoryVariableDeclarations(_))
  }

  // In OpenCL we unroll arrays in private memory
  private object unrollPrivateArrays {
    def apply(body: C.AST.Block): C.AST.Block = {
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
            case OpenCL.AST.VarDecl(name, _: ArrayType, AddressSpace.Private, _) =>
              privateArrayVars.add(name)

            case ArraySubscript(DeclRef(name), index) if privateArrayVars.contains(name) =>
              collectVars(index, loopVars)

            // literal arrays too
            case ArraySubscript(ArrayLiteral(_, _), index) =>
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
        override def pre(n: Node): Result = n match {
          case DeclRef(name) if map.contains(name) => Stop(map(name))

          // unroll previously identified loops
          case loop: ForLoop if loopVars.contains(loop.init.decl.name) =>
            val block = (0 until inferLoopTripCount(loop)).foldLeft(Block())( (block, i) => {
              C.AST.Nodes.VisitAndRebuild(loop.body, Visitor(privateArrayVars, map.updated(loop.init.decl.name, Literal(i.toString)))) match {
                case Block(stmts) =>
                  // keep block nesting if there are names declared inside
                  if ( stmts.exists(exists(_, _.isInstanceOf[DeclStmt])) ) {
                    block + Block(stmts)
                  } else {
                    stmts.foldLeft(block)(_ + _)
                  }
              }
            })

            val result = block.body.foldLeft(Comment(s"unrolled loop"): Stmt)( (stmt, v) => Stmts(stmt, v))

            Continue(result, this)

          case _ => Continue(n, this)
        }
      }

      C.AST.Nodes.VisitAndRebuild(block, Visitor(mutable.Set(), Map()))
    }

    private def exists(n: Node, pred: Node => Boolean): Boolean = {
      var seen = false
      case object Vistor extends C.AST.Nodes.VisitAndRebuild.Visitor {
        override def pre(n: Node): Vistor.Result = {
          if (pred(n)) {
            seen = true
            Stop(n)
          } else {
            Continue(n, this)
          }
        }
      }

      C.AST.Nodes.VisitAndRebuild(n, Vistor)
      seen
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

  // "variables in the local address space can only be declared in the outermost scope of a kernel function"
  private object moveLocalMemoryVariableDeclarations {
    def apply(body: C.AST.Block): C.AST.Block = {
      val localVars = mutable.Set[OpenCL.AST.VarDecl]()
      object Visitor extends C.AST.Nodes.VisitAndRebuild.Visitor
      {
        override def pre(n: Node): Result = n match {
          case DeclStmt(v@OpenCL.AST.VarDecl(_, _, AddressSpace.Local, _)) =>
            localVars += v
            Continue(C.AST.Comment(s"${v.name} moved"), this)
          case _ => Continue(n, this)
        }
      }

      val block = C.AST.Nodes.VisitAndRebuild(body, Visitor)
      val localVarDecls = localVars.foldLeft[C.AST.Stmt](C.AST.Comment("Start of moved local vars")){ (stmts, v) =>
        Stmts(stmts, DeclStmt(v))
      }
      Block(localVarDecls +: C.AST.Comment("End of moved local vars") +: block.body)
    }


  }
}
