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
//      assert(identifyLoopsToUnroll(body).isEmpty)
      body
    }

    // Identify all loops which needs to be unrolled
    //
    // Returns a set of loop variables indicating which loops to unroll
    private def identifyLoopsToUnroll(body: C.AST.Block): Set[String] = {

      case class Visitor(privateArrayVars: mutable.Set[String],
                         loopVars: mutable.Set[String]) extends C.AST.Nodes.VisitAndRebuild.Visitor {
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
