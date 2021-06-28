package shine.GAP8.Compilation

import shine.C.AST.{ArrayLiteral, BinaryOperator}
import shine.C.Compilation.CodeGenerator
import shine.C.Compilation.CodeGenerator.{Declarations, Ranges}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{CommType, DataType, DataTypeIdentifier, DepArrayType, DepPairType, ExpType, IndexType, ManagedBufferType, NatToDataApply, OpaqueType, PairType, ScalarType, VectorType}
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
    case k@KernelCallCmd(kernelName, numCores, numArgs) =>
      //output of type Phrase[AccType]
      //Needs to be piped through shine.C.Compilation.acc

      val calledKernel = acceleratorFunctions
        .filter(module => module.functions.map(_.name).contains(kernelName))
        .head

      //private def expSeq(
      // ps: collection.Seq[Phrase[ExpType]],
      // env: Environment,
      // k: collection.Seq[Expr] => Stmt): Stmt

      //k.output: Phrase[AccType]
      //k.args: Seq[Phrase[ExpType]
      //outputC: C.AST.Expr
      //argsC: Seq[C.AST.Expr]
      //expSeq: Stmt
      //acc(env: Environment, path: Path, cont: Expr => Stmt): Phrase[AccType] => Stmt
      k.output |> acc(env, Nil, (outputC: C.AST.Expr) => expSeq(k.args, env, (argsC: collection.Seq[C.AST.Expr]) => {

        //DeviceBuffer b0 = deviceBufferSync(ctx, moutput, (8 * sizeof(int32_t)), DEVICE_WRITE);
        //DeviceBuffer b1 = deviceBufferSync(ctx, me607, (8 * sizeof(int32_t)), DEVICE_READ);
        val outputSync = deviceBufferSync("b0", outputC, k.output.t.dataType)
        val argSyncs = (k.args zip argsC).zipWithIndex.flatMap{
          case ((arg, argC), i) =>
            arg.t.dataType match {
              case _: ManagedBufferType =>
                Some(deviceBufferSync(s"b${i + 1}", argC, arg.t.dataType))
              case _ => None
            }
        }

        //void* args = pmsis_l2_malloc(num_args * sizeof(struct BufferImpl));
        //void args[num_args] = {outputSync, argSyncs...}
        //val paramsStruct = C.AST.VarDecl("cl_params", C.AST.StructType("cluster_params", Seq()))

        val arrayPackedArgs = C.AST.DeclStmt(C.AST.VarDecl("args", C.AST.ArrayType(C.AST.Type.void, Some(argSyncs.length + 1)), Some(
            ArrayLiteral(C.AST.ArrayType(C.AST.Type.void, Some(argSyncs.length + 1)),
              Seq(outputC) ++ argsC
            )
          ))
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

        C.AST.Block(Seq(outputSync) ++ argSyncs ++ Seq(arrayPackedArgs) ++ Seq(launchKernel))
      }))


      // Generate deviceBufferSync for every param
      // One function per kernel?
      /*val bufferSyncStatements: Seq[(Stmt, ParamDecl)] = calledKernel.functions.head.params.zipWithIndex.map{
        case(paramDecl, index) =>
          (???, paramDecl)
      }*/

      // Pack arguments
      /*val packedKernelArgs = C.AST.DeclStmt(
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
      )*/

    case phrase => phrase |> super.cmd(env)
  }


  //def exp(env: Environment, path: Path, cont: Expr => Stmt): Phrase[ExpType] => Stmt
  private def expSeq(ps: collection.Seq[Phrase[ExpType]],
                     env: Environment,
                     k: collection.Seq[Expr] => Stmt): Stmt = {
    def iter(ps: collection.Seq[Phrase[ExpType]], res: mutable.ArrayBuffer[Expr]): Stmt =
      ps match {
        case p +: ps => (p: Phrase[ExpType]) |> exp(env, Nil, (e: C.AST.Expr) => iter(ps, res += e))
        case _ => k(res)
      }

    iter(ps, new mutable.ArrayBuffer[Expr]())
  }

  private def bufferSize(dt: DataType): Expr =
    dt match {
      case ManagedBufferType(dt) => bufferSize(dt)
      case _: ScalarType | _: IndexType | _: VectorType | _: PairType =>
        C.AST.Literal(s"sizeof(${typ(dt)})")
      case a: shine.DPIA.Types.ArrayType =>
        C.AST.BinaryExpr(C.AST.ArithmeticExpr(a.size), BinaryOperator.*, bufferSize(a.elemType))
      case a: DepArrayType => ??? // TODO
      case _: DepPairType | _: NatToDataApply | _: DataTypeIdentifier | _: OpaqueType |
           _: shine.DPIA.Types.FragmentType =>
        throw new Exception(s"did not expect ${dt}")
    }

  private def deviceBufferSync(varName: String, buffer: Expr, dt: DataType): Stmt = {
    C.AST.DeclStmt(C.AST.VarDecl(varName, C.AST.OpaqueType("DeviceBuffer"), Some(
      C.AST.FunCall(C.AST.DeclRef("deviceBufferSync"), Seq(
        C.AST.DeclRef("ctx"),
        buffer,
        bufferSize(dt),
        C.AST.Literal("0")
      ))
    )))
  }

  override def typ(dt: DataType): Type = super.typ(dt)

}
