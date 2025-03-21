package shine.OpenCL.Compilation

import arithexpr.arithmetic
import rise.core.types.DataType
import rise.core.types.DataType._
import shine.C
import shine.C.AST.{OpaqueType => _, _}
import shine.C.Compilation.CodeGenerator
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.VarType
import shine.OpenCL.primitives.imperative.{HostExecution, KernelCallCmd, NewManagedBuffer}
import shine.OpenCL._

import scala.collection.{immutable, mutable}

object HostCodeGenerator {
  def apply(kernelModules: Seq[KernelModule]): HostCodeGenerator = new HostCodeGenerator(
    mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range](), kernelModules)
}

case class HostCodeGenerator(override val decls: C.Compilation.CodeGenerator.Declarations,
                             override val ranges: C.Compilation.CodeGenerator.Ranges,
                             kernelModules: Seq[KernelModule])
  extends CodeGenerator(decls, ranges)
{
  override def name: String = "OpenCL Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case k@KernelCallCmd(name, LocalSize(ls), GlobalSize(gs), n) =>
      kernelCallCmd(name, ls, gs, k.output, k.args, env)
    case n@NewManagedBuffer(access) =>
      val (dt, Lambda(v, p)) = n.unwrap
      newManagedBuffer(dt, access, v, p, env)
    case h@HostExecution(params) =>
      hostExecution(params, h.body, env)
    case phrase => phrase |> super.cmd(env)
  }

  private def kernelCallCmd(name: String,
                            localSize: NDRange,
                            globalSize: NDRange,
                            output: Phrase[AccType],
                            args: Seq[Phrase[ExpType]],
                            env: Environment): Stmt = {
    import rise.core.types.AddressSpace

    val calledKernel = kernelModules.flatMap(km => km.kernels.filter(_.name == name)) match {
      case Seq(k) => k
      case Seq() => throw new Exception(s"could not find kernel named $name")
      case _ => throw new Exception(s"found multiple kernels named $name")
    }
    val temporaries = calledKernel.paramKinds.zip(calledKernel.code.params).flatMap { case (pk, p) =>
      if (pk.kind == ParamKind.Kind.temporary) {
        Some((pk.typ, p.t.asInstanceOf[shine.OpenCL.AST.PointerType].a, "m" + p.name))
      } else {
        None
      }
    }
    val arg_count = 1 + args.size + temporaries.size
    output |> acc(env, Nil, outputC => expSeq(args, env, argsC => {
      val outputSync = deviceBufferSync("b0", outputC, output.t.dataType, DEVICE_WRITE)
      val argSyncs = (args zip argsC).zipWithIndex.flatMap { case ((arg, argC), i) =>
        arg.t.dataType match {
          case _: ManagedBufferType =>
            Some(deviceBufferSync(s"b${i + 1}", argC, arg.t.dataType, DEVICE_READ))
          case _ => None
        }
      }
      // TODO: could optimize temporary buffer creation/deletion
      val createTmp = temporaries.zipWithIndex.flatMap {
        case ((dt, AddressSpace.Global, name), i) => Seq(
          C.AST.DeclStmt(C.AST.VarDecl(name, typ(ManagedBufferType(dt)), Some(
            C.AST.FunCall(C.AST.DeclRef("createBuffer"), Seq(
              C.AST.DeclRef("ctx"),
              bufferSize(dt),
              C.AST.Literal(accessToString(DEVICE_READ | DEVICE_WRITE))
            ))
          ))),
          deviceBufferSync(s"tb${i}", C.AST.DeclRef(name), dt, DEVICE_READ | DEVICE_WRITE)
        )
        case _ => Seq()
      }
      val destroyTmp = temporaries.flatMap {
        case (dt, AddressSpace.Global, name) => Seq(
          C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("destroyBuffer"), Seq(
            C.AST.DeclRef("ctx"),
            C.AST.DeclRef(name)
          )))
        )
        case _ => Seq()
      }
      val ndRangeTy = C.AST.ArrayType(C.AST.Type.usize, Some(3), true)
      val declGlobalSize = C.AST.DeclStmt(C.AST.VarDecl("global_size", ndRangeTy, Some(
        ArrayLiteral(ndRangeTy, NDRangeToAST(globalSize))
      )))
      val declLocalSize = C.AST.DeclStmt(C.AST.VarDecl("local_size", ndRangeTy, Some(
        ArrayLiteral(ndRangeTy, NDRangeToAST(localSize))
      )))
      val argsTy = C.AST.ArrayType(C.AST.OpaqueType("KernelArg"), Some(arg_count), true)
      val declArgs = C.AST.DeclStmt(C.AST.VarDecl("args", argsTy, Some(
        ArrayLiteral(argsTy,
          kernelArg(0, output.t.dataType, outputC) +:
          ((args zip argsC).zipWithIndex.map { case ((arg, argC), i) =>
            kernelArg(i + 1, arg.t.dataType, argC)
          } ++ temporaries.zipWithIndex.map {
            case ((_, AddressSpace.Private, _), _) =>
              throw new Exception("temporary kernel argument cannot live in private memory")
            case ((dt, AddressSpace.Local, _), i) =>
              kernelLocalArg(i + 1 + args.size, dt)
            case ((dt, AddressSpace.Global, name), i) =>
              kernelArg(i + 1 + args.size, dt, C.AST.DeclRef(s"tb${i}"))
            case ((_, addr, _), _) =>
              throw new Exception(s"did not expect $addr address space")
          })
        )
      )))
      val launchKernel = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("launchKernel"), Seq(
        C.AST.DeclRef("ctx"),
        C.AST.StructMemberAccess(
          C.AST.UnaryExpr(C.AST.UnaryOperator.*, C.AST.DeclRef("self")),
          C.AST.DeclRef(name)
        ),
        C.AST.DeclRef("global_size"),
        C.AST.DeclRef("local_size"),
        C.AST.Literal(s"$arg_count"),
        C.AST.DeclRef("args")
      )))
      C.AST.Block(
        Seq(outputSync) ++ argSyncs ++ createTmp ++
        Seq(declGlobalSize, declLocalSize, declArgs, launchKernel) ++ destroyTmp
      )
    }))
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
        C.AST.Literal(accessToString(access))
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
              C.AST.Literal(accessToString(access))
            )))
          )))
        case None => stmts
      }
    }
    C.AST.Block(paramSyncs :+ cmd(modifiedEnv)(body))
  }

  override def typ(dt: DataType): Type = dt match {
    case ManagedBufferType(_) => C.AST.OpaqueType("Buffer")
    case OpaqueType(name) => C.AST.OpaqueType(name)
    case _ => super.typ(dt)
  }

  private def managedTyp(dt: DataType): C.AST.Type = dt match {
    case DataType.ArrayType(_, elemType) => C.AST.PointerType(typ(elemType))
    case _ => throw new Exception(s"did not expect $dt")
  }

  private def accessToString(a: AccessFlags): String = {
    var res = ""
    if ((a & HOST_WRITE) != 0) { res += "HOST_WRITE | " }
    if ((a & HOST_READ) != 0) { res += "HOST_READ | " }
    if ((a & DEVICE_WRITE) != 0) { res += "DEVICE_WRITE | " }
    if ((a & DEVICE_READ) != 0) { res += "DEVICE_READ | " }
    if (res == "") {
      "0"
    } else {
      res.dropRight(3)
    }
  }

  // TODO: use arith expr simplification here
  private def bufferSize(dt: DataType): Expr =
    dt match {
      case ManagedBufferType(dt) => bufferSize(dt)
      case _: ScalarType | _: IndexType | NatType =>
        C.AST.Literal(s"sizeof(${typ(dt)})")
      case v: VectorType =>
        C.AST.BinaryExpr(C.AST.ArithmeticExpr(v.size), BinaryOperator.*, bufferSize(v.elemType))
      case PairType(fst, snd) =>
        C.AST.BinaryExpr(bufferSize(fst), BinaryOperator.+, bufferSize(snd))
      case a: DataType.ArrayType =>
        C.AST.BinaryExpr(C.AST.ArithmeticExpr(a.size), BinaryOperator.*, bufferSize(a.elemType))
      case a: DepArrayType => ??? // TODO
      case _: DepPairType[_, _] | _: NatToDataApply | _: DataTypeIdentifier | _: OpaqueType |
           _: DataType.FragmentType =>
        throw new Exception(s"did not expect ${dt}")
    }

  private def NDRangeToAST(r: NDRange): Seq[Expr] =
    Seq(C.AST.ArithmeticExpr(r.x), C.AST.ArithmeticExpr(r.y), C.AST.ArithmeticExpr(r.z))

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

  private def deviceBufferSync(varName: String, buffer: Expr, dt: DataType, access: AccessFlags): Stmt = {
    C.AST.DeclStmt(C.AST.VarDecl(varName, C.AST.OpaqueType("DeviceBuffer"), Some(
      C.AST.FunCall(C.AST.DeclRef("deviceBufferSync"), Seq(
        C.AST.DeclRef("ctx"),
        buffer,
        bufferSize(dt),
        C.AST.Literal(accessToString(access))
      ))
    )))
  }

  private def kernelArg(i: Int, dt: DataType, e: Expr): Expr =
    C.AST.FunCall(C.AST.DeclRef("KARG"), Seq(dt match {
      case _: ManagedBufferType => C.AST.DeclRef(s"b$i")
      case _ => e
    }))

  private def kernelLocalArg(i: Int, dt: DataType): Expr =
    C.AST.FunCall(C.AST.DeclRef("LARG"), Seq(bufferSize(dt)))
}