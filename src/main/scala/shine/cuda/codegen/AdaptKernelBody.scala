package shine.cuda.codegen

import arithexpr.arithmetic.{IfThenElse => _}
import shine.C.AST.{Nodes, _}
import shine.DPIA.Types.AddressSpace
import shine.cuda.ast.ExternArrayType
import shine.{C, OpenCL}

import scala.collection.mutable

//TODO smiliar to OpenCl-AdaptKernelBody
object AdaptKernelBody {

  def apply(body: C.AST.Block): (C.AST.Block, Long) = {
    body |>
      (unrollPrivateArrays(_)) |>
      (moveLocalMemoryVariableDeclarations(_))
  }

  // In OpenCL we unroll arrays in private memory
  private object unrollPrivateArrays {
    def apply(body: C.AST.Block): C.AST.Block = {
      if (identifyLoopsToUnroll(body).nonEmpty) {
        println("WARNING: loops that should have been unrolled remain in the OpenCL code")
      }
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

  private object moveLocalMemoryVariableDeclarations {
    def apply(body: C.AST.Block): (C.AST.Block, Long) = {
      val dynamicShared = OpenCL.AST.VarDecl(
        shine.DPIA.freshName("dynamicSharedMemory"),
        ExternArrayType(C.AST.Type.char),
        AddressSpace.Local)

      var offset = 0L
      var sharedMemSize = 0L
      val localVars = mutable.Set[(Node, Long)]()

      object Visitor extends C.AST.Nodes.VisitAndRebuild.Visitor
      {
        override def pre(n: Node): Result = n match {
          case DeclStmt(OpenCL.AST.VarDecl(name, vType, AddressSpace.Local, init)) =>
            val sizeInBytes = shine.cuda.ast.Type.sizeInBytes(vType)

            val shmemPtr =
              C.AST.ArraySubscript(
                C.AST.DeclRef(dynamicShared.name),
                C.AST.ArithmeticExpr(offset))

            val decl =
              vType match {
                case a@C.AST.ArrayType(_, _, _) =>
                  val pointerType = PointerType(a.getBaseType)
                  C.AST.DeclStmt(
                    C.AST.VarDecl(name, pointerType,
                    Some(
                      C.AST.Cast(
                        pointerType,
                        C.AST.UnaryExpr(C.AST.UnaryOperator.&, shmemPtr)))))
                //TODO is this correct?
                case _ =>
                  C.AST.DeclStmt(
                    C.AST.VarDecl(name, vType, Some(
                      C.AST.ArraySubscript(
                        C.AST.Cast(
                          PointerType(vType),
                          shmemPtr),
                        C.AST.ArithmeticExpr(0)))))
              }

            offset += sizeInBytes
            sharedMemSize = Math.max(offset, sharedMemSize)

            val cNode =
              init match {
                case Some(initStmt) =>
                  C.AST.Stmts(decl,
                    C.AST.ExprStmt(C.AST.Assignment(C.AST.DeclRef(name), initStmt)))
                case _ =>
                  decl
              }

            localVars.add((cNode, sizeInBytes))

            Continue(cNode, this)
          case _ => Continue(n, this)
        }

        override def post(n: Node): Node = {
          n match {
            case block: Block =>
              block.body.foreach(stmt => {
                val localVar = localVars.filter(l => l._1.equals(stmt)).toArray
                if (localVar.nonEmpty) {
                  if (localVar.length == 1)
                    offset -= localVar(0)._2
                  else
                    throw new Exception("this shold not happen")
                }
              })
            case _ =>
          }
          
          n
        }
      }

      val block = C.AST.Nodes.VisitAndRebuild(body, Visitor)

      (if (sharedMemSize != 0)
         Block(C.AST.DeclStmt(dynamicShared) +: block.body)
       else
         Block(block.body),
        sharedMemSize)
    }
  }
}
