package shine.GAP8.Compilation

import shine.C.AST.ParamDecl
import shine.C.Compilation.CodeGenerator
import shine.C.Compilation.CodeGenerator.{Declarations, Ranges}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.GAP8.primitives.imperative.KernelCallCmd
import shine._

import scala.collection.mutable
import scala.language.postfixOps

case class HostCodeGenerator(
                              override val decls: Declarations,
                              override val ranges: Ranges,
                              acceleratorFunctions: Seq[shine.C.Module]
                            ) extends CodeGenerator(decls, ranges){
  override def name: String = "GAP8 Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case k@KernelCallCmd(kernelName, numCores) =>
      //output of type Phrase[AccType]
      //Needed to be piped through shine.C.Compilation.acc
      //k.output |> acc(env, Nil, outputCont => expSeq())

      //output |> acc(env, Nil, outputC => expSeq())

      val calledKernel = acceleratorFunctions
        .filter(module => module.functions.map(_.name).contains(kernelName))
        .head




      // Generate deviceBufferSync for every param
      // One function per kernel?
      val bufferSyncStatements: Seq[(Stmt, ParamDecl)] = calledKernel.functions.head.params.zipWithIndex.map{
        case(paramDecl, index) =>
          (???, paramDecl)
      }

      // Pack arguments
      val packedKernelArgs = C.AST.DeclStmt(
        C.AST.VarDecl("args",
          C.AST.PointerType(C.AST.Type.void),
          Some(
            C.AST.ArrayLiteral(
              C.AST.ArrayType(
                C.AST.PointerType(C.AST.Type.void),
                Some(bufferSyncStatements.length)
              ),
              bufferSyncStatements.map(_._2).map(pdecl => C.AST.Literal(pdecl.name))
            )
          )
        )
      )

      // Call launchKernel
      // void launchKernel(Context ctx, Kernel k, int num_threads, void* args)
      val launchKernel = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("launchKernel"), Seq(
        C.AST.DeclRef("ctx"),
        C.AST.StructMemberAccess(
          C.AST.UnaryExpr(C.AST.UnaryOperator.*, C.AST.DeclRef("self")),
          // Dubious hardcoded k0
          C.AST.DeclRef("k0")
        ),
        C.AST.Literal(s"$numCores"),
        C.AST.DeclRef("args")
      )))

      C.AST.Block(bufferSyncStatements.map(_._1) ++ Seq(packedKernelArgs) ++ Seq(launchKernel))

    case phrase => phrase |> super.cmd(env)
  }

  private def expSeq(ps: collection.Seq[Phrase[ExpType]],
                     env: Environment,
                     k: collection.Seq[Expr] => Stmt): Stmt = {
    def iter(ps: collection.Seq[Phrase[ExpType]], res: mutable.ArrayBuffer[Expr]): Stmt =
      ps match {
        case p +: ps => p |> exp(env, Nil, e => iter(ps, res += e))
        case _ => k(res)
      }

    iter(ps, new mutable.ArrayBuffer[Expr]())
  }

  override def typ(dt: DataType): Type = super.typ(dt)

}
