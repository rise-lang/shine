package shine.OpenCL

import arithexpr.arithmetic
import shine.C
import shine.C.AST._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.VarType
import shine.OpenCL.compilation.HostManagedBuffers
import shine.OpenCL.primitives.imperative.{KernelCallCmd, NewManagedBuffer}

import scala.collection.{immutable, mutable}

object HostCodeGenerator {
  def apply(): HostCodeGenerator =
    new HostCodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

case class HostCodeGenerator(override val decls: C.CodeGenerator.Declarations,
                             override val ranges: C.CodeGenerator.Ranges)
  extends C.CodeGenerator(decls, ranges)
{
  override def name: String = "OpenCL Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case KernelCallCmd(name, LocalSize(ls), GlobalSize(gs), output, args) =>
      output |> acc(env, Nil, outputC =>
        expSeq(args, env, argsC =>
          C.AST.Block(
            Seq(
              C.AST.DeclStmt(C.AST.VarDecl(name, C.AST.OpaqueType("Kernel"), Some(
                C.AST.FunCall(C.AST.DeclRef("loadKernel"), Seq(
                  C.AST.DeclRef("ctx"),
                  C.AST.Literal(s"${'"'}${name}${'"'}"),
                  C.AST.Literal(s"${'"'}${name}.cl${'"'}") // TODO: kernel path
                ))
              ))),
              targetBufferSync("b0", outputC, output.t.dataType, TARGET_WRITE)
            ) ++ (args zip argsC).zipWithIndex.flatMap { case ((arg, argC), i) =>
              arg.t.dataType match {
                case _: ManagedBufferType =>
                  Some(targetBufferSync(s"b${i+1}", argC, arg.t.dataType, TARGET_READ))
                case _ => None
              }
            } ++ Seq(
              clSetKernelArg(name, 0, output.t.dataType, outputC)
            ) ++ (args zip argsC).zipWithIndex.map { case ((arg, argC), i) =>
              clSetKernelArg(name, i+1, arg.t.dataType, argC)
            } ++ Seq(
              C.AST.DeclStmt(C.AST.VarDecl("global_size", C.AST.ArrayType(C.AST.Type.usize, Some(3), true), Some(
                ArrayLiteral(C.AST.ArrayType(C.AST.Type.usize, Some(3), true), NDRangeToAST(gs))
              ))),
              C.AST.DeclStmt(C.AST.VarDecl("local_size", C.AST.ArrayType(C.AST.Type.usize, Some(3), true), Some(
                ArrayLiteral(C.AST.ArrayType(C.AST.Type.usize, Some(3), true), NDRangeToAST(ls))
              ))),
              C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("launchKernel"), Seq(
                C.AST.DeclRef("ctx"),
                C.AST.DeclRef(name),
                C.AST.DeclRef("global_size"),
                C.AST.DeclRef("local_size")
              ))),
              C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("destroyKernel"), Seq(
                C.AST.DeclRef("ctx"),
                C.AST.DeclRef(name)
              )))
            )
          )
        ))
    case NewManagedBuffer(dt, access, Lambda(v, p)) =>
      // FIXME: the constructed substitutions and environment are convoluted
      val ve = Identifier(s"${v.name}_e", v.t.t1)
      val va = Identifier(s"${v.name}_a", v.t.t2)
      val vC = C.AST.DeclRef(v.name)
      val dt2 = v.`type`.t1.dataType.asInstanceOf[ManagedBufferType].dt
      val v2 = Identifier(v.name.drop(1), VarType(dt2))
      val ve2 = Identifier(s"${v2.name}_e", v2.t.t1)
      val va2 = Identifier(s"${v2.name}_a", v2.t.t2)
      val vC2 = C.AST.DeclRef(v2.name)
      C.AST.Block(immutable.Seq(
        C.AST.DeclStmt(C.AST.VarDecl(vC.name, typ(ManagedBufferType(dt)), Some(
          C.AST.FunCall(C.AST.DeclRef("createBuffer"), Seq(
            C.AST.DeclRef("ctx"),
            C.AST.Literal(KernelExecutor.sizeInByte(dt).value.toString),
            C.AST.Literal(accessToString(access))
          ))
        ))),
        Phrase.substitute(Map(
          Proj1(v) -> ve,
          Proj2(v) -> va,
          Proj1(v2) -> ve2,
          Proj2(v2) -> va2,
          ), `in` = p) |>
          cmd(env updatedIdentEnv (ve -> vC) updatedIdentEnv (va -> vC)
            updatedVarEnv (v -> vC)
            updatedIdentEnv (ve2 -> vC2) updatedIdentEnv (va2 -> vC2)
            updatedVarEnv (v2 -> vC2)),
        C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("destroyBuffer"), Seq(
          C.AST.DeclRef("ctx"),
          vC
        )))
      ))
    case HostManagedBuffers.HostExecution(params, execute) =>
      var env2 = env
      val stmts = params.foldRight(Seq[C.AST.Stmt]()){ case ((ident, access), stmts) =>
        HostManagedBuffers.optionallyManaged(ident) match {
          case Some((mident, dt)) =>
            env2 = ident match {
              case Identifier(_, _: PhrasePairType[_, _]) => env2 updatedVarEnv (ident.asInstanceOf[Identifier[VarType]] -> C.AST.DeclRef(ident.name))
              case Identifier(_, _: BasePhraseType) => env2 updatedIdentEnv (ident.asInstanceOf[Identifier[_ <: BasePhraseType]] -> C.AST.DeclRef(ident.name))
              case _ => throw new Exception("this should not happen")
            }
            stmts :+ C.AST.DeclStmt(C.AST.VarDecl(ident.name, managedTyp(dt), Some(
              C.AST.Cast(managedTyp(dt), C.AST.FunCall(C.AST.DeclRef("hostBufferSync"), Seq(
                C.AST.DeclRef("ctx"),
                C.AST.DeclRef(mident.name),
                C.AST.Literal(KernelExecutor.sizeInByte(dt).value.toString),
                C.AST.Literal(accessToString(access))
              )))
            )))
          case None => stmts
        }
      }
      C.AST.Block(stmts :+ cmd(env2)(execute))
    case phrase => phrase |> super.cmd(env)
  }

  private def managedTyp(dt: DataType): C.AST.Type = dt match {
    case shine.DPIA.Types.ArrayType(_, elemType) => C.AST.PointerType(typ(elemType))
    case _ => throw new Exception(s"did not expect $dt")
  }

  private def accessToString(a: AccessFlags): String = {
    var res = ""
    if ((a & HOST_WRITE) != 0) { res += "HOST_WRITE | " }
    if ((a & HOST_READ) != 0) { res += "HOST_READ | " }
    if ((a & TARGET_WRITE) != 0) { res += "TARGET_WRITE | " }
    if ((a & TARGET_READ) != 0) { res += "TARGET_READ | " }
    if (res == "") {
      "0"
    } else {
      res.dropRight(3)
    }
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

  private def targetBufferSync(varName: String, buffer: Expr, dt: DataType, access: AccessFlags): Stmt = {
    C.AST.DeclStmt(C.AST.VarDecl(varName, C.AST.OpaqueType("cl_mem"), Some(
      C.AST.FunCall(C.AST.DeclRef("targetBufferSync"), Seq(
        C.AST.DeclRef("ctx"),
        buffer,
        C.AST.Literal(KernelExecutor.sizeInByte(dt).value.toString),
        C.AST.Literal(accessToString(access))
      ))
    )))
  }

  private def clSetKernelArg(kName: String, i: Int, dt: DataType, e: Expr): Stmt = {
    val (argSize, argValue) = dt match {
      case _: ManagedBufferType =>
        (C.AST.Literal("sizeof(cl_mem)"),
          C.AST.UnaryExpr(UnaryOperator.&, C.AST.DeclRef(s"b$i")))
      case _ =>
        (C.AST.Literal(KernelExecutor.sizeInByte(dt).value.toString),
          C.AST.UnaryExpr(UnaryOperator.&, e))
    }
    C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("clSetKernelArg"), Seq(
      C.AST.StructMemberAccess(
        C.AST.UnaryExpr(UnaryOperator.*, C.AST.DeclRef(kName)),
        C.AST.DeclRef("inner")),
      C.AST.Literal(s"$i"),
      argSize,
      argValue
    )))
  }
}