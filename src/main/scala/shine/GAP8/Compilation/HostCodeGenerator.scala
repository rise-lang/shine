package shine.GAP8.Compilation

import shine.C.AST.ParamDecl
import shine.C.Compilation.CodeGenerator
import shine.C.Compilation.CodeGenerator.{Declarations, Ranges}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{CommType, DataType}
import shine.GAP8.primitives.imperative.KernelCallCmd
import shine._

import scala.language.postfixOps

case class HostCodeGenerator(
                              override val decls: Declarations,
                              override val ranges: Ranges,
                              acceleratorFunctions: Seq[shine.C.Module]
                            ) extends CodeGenerator(decls, ranges){
  override def name: String = "GAP8 Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case KernelCallCmd(kernelName, numCores) =>
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

  override def typ(dt: DataType): Type = super.typ(dt)

}
