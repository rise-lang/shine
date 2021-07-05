package shine.GAP8.Compilation

import arithexpr.arithmetic
import shine.C.AST.{BinaryOperator, Decl}
import shine.C.Compilation.CodeGenerator
import shine.C.Compilation.CodeGenerator.{Declarations, Ranges}
import shine.DPIA.Phrases._
import shine.DPIA.Types.{BasePhraseType, CommType, DataType, DataTypeIdentifier, DepArrayType, DepPairType, ExpType, IndexType, ManagedBufferType, NatToDataApply, OpaqueType, PairType, PhrasePairType, PhraseType, ScalarType, VectorType}
import shine.DPIA.VarType
import shine.GAP8.primitives.imperative.KernelCallCmd
import shine.OpenCL.AccessFlags
import shine.OpenCL.Compilation.HostManagedBuffers
import shine.OpenCL.primitives.imperative.{HostExecution, NewManagedBuffer}
import shine._

import scala.collection.{immutable, mutable}
import scala.language.postfixOps

object HostCodeGenerator {
  def apply(acceleratorFunctions: Seq[shine.C.Module]): HostCodeGenerator =
    new HostCodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range](), acceleratorFunctions)
}

case class HostCodeGenerator(
                              override val decls: Declarations,
                              override val ranges: Ranges,
                              acceleratorFunctions: Seq[shine.C.Module]
                            ) extends CodeGenerator(decls, ranges){
  override def name: String = "GAP8 Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case k@KernelCallCmd(kernelName, numCores, numArgs) =>
      val calledKernel = acceleratorFunctions
        .filter(module => module.functions.map(_.name).contains(kernelName))
        .head

      k.output |> acc(env, Nil, (outputC: C.AST.Expr) => expSeq(k.args, env, (argsC: collection.Seq[C.AST.Expr]) => {

        //DeviceBuffer b0 = deviceBufferSync(ctx, moutput, (8 * sizeof(int32_t)), DEVICE_WRITE);
        val outputSync = (deviceBufferSync("b0", outputC, k.output.t.dataType), "b0")
        val argSyncs = (k.args zip argsC).zipWithIndex.flatMap{
          case ((arg, argC), i) =>
            arg.t.dataType match {
              case _: ManagedBufferType =>
                val varName = s"b${i + 1}"
                Some((deviceBufferSync(varName, argC, arg.t.dataType)), varName)
              case _ => None
            }
        }

        //struct cluster_params* cl_params = (struct cluster_params*) pmsis_l2_malloc(sizeof(struct cluster_params));
        val structDecl = C.AST.DeclStmt(C.AST.VarDecl(
          "cl_params",
          C.AST.PointerType(C.AST.StructType("cluster_params", Seq())),
          Some(
            C.AST.Cast(
              C.AST.PointerType(C.AST.StructType("cluster_params", Seq())),
              C.AST.FunCall(
                C.AST.DeclRef("pmsis_l2_malloc"),
                Seq(
                  C.AST.FunCall(C.AST.DeclRef("sizeof"), Seq(C.AST.Literal("struct cluster_params")))
                )
              )
            )
          )
        ))

        //Assume one accelerator function per module (definition). Get params
        //  cl_params->output = b0;
        val assignments = (outputSync._2 +: argSyncs.map(_._2)).zip(calledKernel.functions.head.params).map {
          case (expr, pdecl) =>
            C.AST.ExprStmt(C.AST.Assignment(
              C.AST.StructMemberAccess(C.AST.Literal("(*cl_params)"), C.AST.DeclRef(pdecl.name)),
              C.AST.Literal(expr)
            ))
        }

        // void launchKernel(Context ctx, Kernel k, int num_threads, void* args)
        val launchKernel = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("launchKernel"), Seq(
          C.AST.DeclRef("ctx"),
          C.AST.StructMemberAccess(
            C.AST.UnaryExpr(C.AST.UnaryOperator.*, C.AST.DeclRef("self")),
            // Dubious hardcoded cluster_core_task
            C.AST.DeclRef("cluster_core_task")
          ),
          C.AST.Literal(s"${numCores}"),
          C.AST.DeclRef("cl_params")
        )))

        C.AST.Block(Seq(outputSync._1) ++ argSyncs.map(_._1) ++ Seq(structDecl) ++ assignments ++ Seq(launchKernel))
      }))

    case n@NewManagedBuffer(access) =>
      val (dt, Lambda(v, p)) = n.unwrap
      newManagedBuffer(dt, access, v, p, env)
    case h@HostExecution(params) =>
      hostExecution(params, h.body, env)

    case phrase => phrase |> super.cmd(env)
  }

  private def newManagedBuffer(dt: DataType,
                               access: AccessFlags,
                               managedV: Identifier[VarType],
                               body: Phrase[CommType],
                               env: Environment): Stmt = {
    // FIXME: the constructed substitutions and environment are convoluted
    val managedE = Identifier(s"${managedV.name}_e", managedV.t.t1)
    val managedA = Identifier(s"${managedV.name}_a", managedV.t.t2)
    val managedC = C.AST.DeclRef(managedV.name)
    val unmanagedV = Identifier(managedV.name.drop(1), VarType(dt))
    val unmanagedE = Identifier(s"${unmanagedV.name}_e", unmanagedV.t.t1)
    val unmanagedA = Identifier(s"${unmanagedV.name}_a", unmanagedV.t.t2)
    val unmanagedC = C.AST.DeclRef(unmanagedV.name)
    val createBuffer = C.AST.DeclStmt(C.AST.VarDecl(managedC.name, typ(ManagedBufferType(dt)), Some(
      C.AST.FunCall(C.AST.DeclRef("createBuffer"), Seq(
        C.AST.DeclRef("ctx"),
        bufferSize(dt),
        C.AST.Literal("0")
      ))
    )))
    val bodyC =
    // projections of the DPIA variable are the same C variable
      Phrase.substitute(Map(
        Proj1(managedV) -> managedE,
        Proj2(managedV) -> managedA,
        Proj1(unmanagedV) -> unmanagedE,
        Proj2(unmanagedV) -> unmanagedA,
      ), `in` = body) |>
        cmd(env updatedIdentEnv (managedE -> managedC) updatedIdentEnv (managedA -> managedC)
          updatedIdentEnv (unmanagedE -> unmanagedC) updatedIdentEnv (unmanagedA -> unmanagedC))
    val destroyBuffer = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("destroyBuffer"), Seq(
      C.AST.DeclRef("ctx"),
      managedC
    )))
    C.AST.Block(immutable.Seq(createBuffer, bodyC, destroyBuffer))
  }

  private def hostExecution(params: Map[Identifier[_ <: PhraseType], AccessFlags],
                            body: Phrase[CommType],
                            env: Environment): Stmt = {
    var modifiedEnv = env
    val paramSyncs = params.foldRight(Seq[C.AST.Stmt]()){ case ((ident, access), stmts) =>
      HostManagedBuffers.optionallyManaged(ident) match {
        case Some((mident, dt)) =>
          modifiedEnv = ident match {
            case Identifier(_, _: PhrasePairType[_, _]) =>
              // no need to modify the environment, this has been done by `newManagedBuffer`
              modifiedEnv
            case Identifier(_, _: BasePhraseType) => modifiedEnv updatedIdentEnv
              (ident.asInstanceOf[Identifier[_ <: BasePhraseType]] -> C.AST.DeclRef(ident.name))
            case _ => throw new Exception("this should not happen")
          }

          stmts :+ C.AST.DeclStmt(C.AST.VarDecl(ident.name, managedTyp(dt), Some(
            C.AST.Cast(managedTyp(dt), C.AST.FunCall(C.AST.DeclRef("hostBufferSync"), Seq(
              C.AST.DeclRef("ctx"),
              C.AST.DeclRef(mident.name),
              bufferSize(dt),
              C.AST.Literal("0")
            )))
          )))
        case None => stmts
      }
    }
    C.AST.Block(paramSyncs :+ cmd(modifiedEnv)(body))
  }

  private def managedTyp(dt: DataType): C.AST.Type = dt match {
    case shine.DPIA.Types.ArrayType(_, elemType) => C.AST.PointerType(typ(elemType))
    case _ => throw new Exception(s"did not expect $dt")
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

  //override def typ(dt: DataType): Type = super.typ(dt)
  override def typ(dt: DataType): Type = dt match {
    case ManagedBufferType(_) => C.AST.OpaqueType("Buffer")
    case OpaqueType(name) => C.AST.OpaqueType(name)
    case _ => super.typ(dt)
  }

}
