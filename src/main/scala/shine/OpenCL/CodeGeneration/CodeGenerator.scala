package shine.OpenCL.CodeGeneration

import shine.C.AST.{ArraySubscript, BasicType, Decl, DeclStmt, PointerType}
import shine.C.CodeGeneration.CodeGenerator.{CIntExpr, FstMember, SndMember}
import shine.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.DSL._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.VectorData
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.FunctionalPrimitives.OpenCLFunction
import shine.OpenCL.ImperativePrimitives._
import shine.OpenCL.{BuiltInFunctionCall, GlobalSize, LocalSize}
import shine._
import arithexpr.arithmetic
import arithexpr.arithmetic._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends CCodeGenerator(decls, ranges) {
  override def name: String = "OpenCL"

  override def updatedRanges(key: String, value: arithexpr.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))

  override def cmd(phrase: Phrase[CommType], env: Environment): Stmt = {
    phrase match {
      case f@OpenCLParFor(n, dt, a, Lambda(i, Lambda(o, p)), _, _, _) =>
        OpenCLCodeGen.codeGenOpenCLParFor(f, n, dt, a, i, o, p, env)

      case f@OpenCLParForNat(n, _, a, DepLambda(i: NatIdentifier, Lambda(o, p)), _, _, _) =>
        OpenCLCodeGen.codeGenOpenCLParForNat(f, n, a, i, o, p, env)

      case Assign(dt, a, e) => dt match {
        case VectorType(_, _) =>
          //noinspection VariablePatternShadow
          exp(e, env, Nil, e =>
            acc(a, env, Nil, {
              case C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(idx, v))
                if name.startsWith("vstore") =>
                C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(e, idx, v)))
              case a => C.AST.ExprStmt(C.AST.Assignment(a, e))
            }))

        case _ => super.cmd(phrase, env)
      }

      case OpenCLNew(a, dt, Lambda(v, p)) => OpenCLCodeGen.codeGenOpenCLNew(a, dt, v, p, env)
      case _: New => throw new Exception("New without address space found in OpenCL program.")

      case OpenCLNewDoubleBuffer(a, _, _, dt, n, in, out, Lambda(ps, p)) =>
        OpenCLCodeGen.codeGenOpenCLNewDoubleBuffer(a, ArrayType(n, dt), in, out, ps, p, env)

      case Barrier(localMemFence, globalMemFence) => OpenCL.AST.Barrier(localMemFence, globalMemFence)

      case _: NewDoubleBuffer => throw new Exception("NewDoubleBuffer without address space found in OpenCL program.")

      case _ => super.cmd(phrase, env)
    }
  }

  override def acc(phrase: Phrase[AccType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case AsVectorAcc(n, _, _, a) => path match {
        case (i : CIntExpr) :: ps =>     acc(a, env, CIntExpr(i / n) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalarAcc(_, m, dt, a) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
          acc(a, env, CIntExpr((i * m) + j) :: ps, cont)

        case (i : CIntExpr) :: Nil =>
          // TODO: check alignment and use pointer with correct address space
          acc(a, env, CIntExpr(i * m) :: Nil, array => {
            // the continuation has to add the value ...
            val ptr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, array)
            cont(C.AST.FunCall(C.AST.DeclRef(s"vstore$m"), immutable.Seq(C.AST.Literal("0"), ptr)))
          })
        case _ =>           error(s"Expected path to be not empty")
      }
      case IdxVecAcc(_, _, i, a) => CCodeGen.codeGenIdxAcc(i, a, env, path, cont)

      case IdxDistributeAcc(_, _, stride, _, _, a) => path match {
        // TODO: ensure that i % stride == init ?
        case (i : CIntExpr) :: ps => acc(a, env, CIntExpr(i / stride) :: ps, cont)
        case _ => error(s"Expected a C-Integer-Expression on the path.")
      }

      case RecordAcc(_, _, fst, snd) => path match {
        case FstMember :: ps => acc(fst, env, ps, cont)
        case SndMember :: ps => acc(snd, env, ps, cont)
        case Nil =>
          // FIXME: hacky
          acc(fst, env, Nil, { case C.AST.StructMemberAccess(a1, _) =>
            acc(snd, env, Nil, { case C.AST.StructMemberAccess(a2, _) =>
              assert(a1 == a2)
              cont(a1)
            })})
        case _ => error(s"did not expect $path")
      }

      case _ => super.acc(phrase, env, path, cont)
    }
  }

  override def exp(phrase: Phrase[ExpType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case Phrases.Literal(n) => (path, n.dataType) match {
        case (Nil, _: VectorType) => cont(OpenCLCodeGen.codeGenLiteral(n))
        case _ => super.exp(phrase, env, path, cont)
      }
      case UnaryOp(op, e) => phrase.t.dataType match {
        case _: VectorType => path match {
          case i :: ps => exp(e, env, i :: ps, e =>
            cont(CCodeGen.codeGenUnaryOp(op, e)))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path, cont)
      }
      case BinOp(op, e1, e2) => phrase.t.dataType match {
        case _: VectorType =>
          exp(e1, env, path, e1 =>
            exp(e2, env, path, e2 =>
              cont(CCodeGen.codeGenBinaryOp(op, e1, e2))
          ))
        case _ => super.exp(phrase, env, path, cont)
      }
      case AsVector(n, m, dt, e) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
          exp(e, env, CIntExpr((i * n) + j) :: ps, cont)
        case (i : CIntExpr) :: Nil =>
          OpenCLCodeGen.codeGenVectorLiteral(n.eval, dt, j =>
            Idx(n * m, dt, AsIndex(n * m, Natural((i * n) + j)), e),
            env, cont
          )
        case _ => error(s"unexpected $path")
      }
      case AsVectorAligned(n, _, _, e) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
          exp(e, env, CIntExpr((i * n) + j) :: ps, cont)
        case (i : CIntExpr) :: Nil =>
          exp(e, env, CIntExpr(i * n) :: Nil, arrayE => {
            // TODO: use pointer with correct address space
            // TODO: check that idx is the right offset ...
            val ptr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, arrayE)
            cont( C.AST.FunCall(C.AST.DeclRef(s"vload$n"), immutable.Seq(C.AST.Literal("0"), ptr)) )
          })
        case _ => error(s"unexpected $path")
      }
      case AsScalar(_, m, _, e) => path match {
        case (i : CIntExpr) :: ps =>
          exp(e, env, CIntExpr(i / m) :: CIntExpr(i % m) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      // TODO: this has to be refactored
      case VectorFromScalar(n, st, e) => path match {
        case (_: CIntExpr) :: ps =>
          // in this case we index straight into the vector build from a single scalar
          // it is equivalent to return the scalar `e' without boxing and unboxing it
          exp(e, env, ps, cont)

        case Nil =>
          exp(e, env, Nil, e =>
            cont(OpenCL.AST.VectorLiteral(
              OpenCL.AST.VectorType(n, typ(st).asInstanceOf[C.AST.BasicType]),
              immutable.Seq(e))))

        case _ => error(s"Didn't expect path: $path")
      }

      case IdxVec(_, _, i, e) => CCodeGen.codeGenIdx(i, e, env, path, cont)

      case OpenCLFunction(name, _, _, args) =>
        CCodeGen.codeGenForeignCall(name, args, env, Nil, cont)

      case IdxDistribute(_, _, stride, _, _, e) => path match {
        // TODO: ensure that i % stride == init ?
        case (i : CIntExpr) :: ps => exp(e, env, CIntExpr(i / stride) :: ps, cont)
        case _ => error(s"Expected a C-Integer-Expression on the path.")
      }

      case _ => super.exp(phrase, env, path, cont)
    }
  }

  override def typ(dt: DataType): Type = dt match {
    case VectorType(n, elemType) =>
      OpenCL.AST.VectorType(n, super.typ(elemType).asInstanceOf[BasicType])
    case _ => super.typ(dt)
  }

  override def generateAccess(dt: DataType,
                              expr: Expr,
                              path: Path,
                              env: Environment): Expr = {
    (path, dt) match {
      case ((i: CIntExpr) :: _, _: VectorType) =>
        OpenCL.AST.VectorSubscript(expr, C.AST.ArithmeticExpr(i))
      case _ => super.generateAccess(dt, expr, path, env)
    }
  }

  override def genNat(n: Nat, env: Environment, cont:Expr => Stmt): Stmt = n match {
    case of: BuiltInFunctionCall => cont(C.AST.Literal(of.toString))
    case _ => super.genNat(n, env, cont)
  }

  protected object OpenCLCodeGen {
    def codeGenOpenCLNew(a: AddressSpace,
                         dt: DataType,
                         v: Identifier[VarType],
                         p: Phrase[CommType],
                         env: Environment): Stmt = {
      val ve = Identifier(s"${v.name}_e", v.t.t1)
      val va = Identifier(s"${v.name}_a", v.t.t2)
      val vC = C.AST.DeclRef(v.name)

      C.AST.Block(immutable.Seq(
        C.AST.DeclStmt(OpenCL.AST.VarDecl(vC.name, typ(dt), a)),
        cmd(Phrase.substitute(PhrasePair(ve, va), `for` = v, `in` = p),
          env updatedIdentEnv (ve -> vC)
            updatedIdentEnv (va -> vC))))
    }

    def codeGenOpenCLNewDoubleBuffer(a: AddressSpace,
                                     dt: ArrayType,
                                     in: Phrase[ExpType],
                                     out: Phrase[AccType],
                                     ps: Identifier[VarType x CommType x CommType],
                                     p: Phrase[CommType],
                                     env: Environment): Stmt = {
      import C.AST._
      import BinaryOperator._
      import UnaryOperator._

      val ve = Identifier(s"${ps.name}_e", ps.t.t1.t1.t1)
      val va = Identifier(s"${ps.name}_a", ps.t.t1.t1.t2)
      val done = Identifier(s"${ps.name}_swap", ps.t.t1.t2)
      val swap = Identifier(s"${ps.name}_done", ps.t.t2)

      val tmp1 = DeclRef(freshName("tmp1_"))
      val tmp2 = DeclRef(freshName("tmp2_"))
      val in_ptr = DeclRef(freshName("in_ptr_"))
      val out_ptr = DeclRef(freshName("out_ptr_"))
      val flag = DeclRef(freshName("flag_"))

      Block(immutable.Seq(
        // create variables: `tmp1', `tmp2`, `in_ptr', and `out_ptr'
        DeclStmt(OpenCL.AST.VarDecl(tmp1.name, typ(dt), a)),
        DeclStmt(OpenCL.AST.VarDecl(tmp2.name, typ(dt), a)),
        exp(in, env, CIntExpr(0) :: Nil, e => makePointerDecl(in_ptr.name, a, dt.elemType, UnaryExpr(&, e))),
        makePointerDecl(out_ptr.name, a, dt.elemType, tmp1),
        // create boolean flag used for swapping
        DeclStmt(VarDecl(flag.name, Type.uchar, Some(Literal("1")))),
        // generate body
        cmd(
          Phrase.substitute(PhrasePair(PhrasePair(PhrasePair(ve, va), swap), done), `for` = ps, `in` = p),
          env updatedIdentEnv (ve -> in_ptr) updatedIdentEnv (va -> out_ptr)
            updatedCommEnv (swap -> {
            Block(immutable.Seq(
              ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
              ExprStmt(Assignment(out_ptr, TernaryExpr(flag, tmp2, tmp1))),
              // toggle flag with xor
              ExprStmt(Assignment(flag, BinaryExpr(flag, ^, Literal("1")))),
              OpenCL.AST.Barrier(true, true)
            ))
          })
            updatedCommEnv (done -> {
            Block(immutable.Seq(
              ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
              acc(out, env, CIntExpr(0) :: Nil, o => ExprStmt(Assignment(out_ptr, UnaryExpr(&, o)))),
              OpenCL.AST.Barrier(true, true)
            ))
          }))
      ))
    }

    def codeGenOpenCLParFor(f: OpenCLParFor,
                            n: Nat,
                            dt: DataType,
                            a: Phrase[AccType],
                            i: Identifier[ExpType],
                            o: Phrase[AccType],
                            p: Phrase[CommType],
                            env: Environment): Stmt = {
      assert(!f.unroll)
      val cI = C.AST.DeclRef(f.name)
      val range = RangeAdd(f.init, n, f.step)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

      val init = OpenCL.AST.VarDecl(cI.name, C.AST.Type.int, AddressSpace.Private, init = Some(C.AST.ArithmeticExpr(range.start)))
      val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + range.step))

      Phrase.substitute(a `@` i, `for` = o, `in` = p) |> (p =>

      env.updatedIdentEnv(i -> cI) |> (env => {

        range.numVals match {
          // iteration count is 0 => skip body; no code to be emitted
          case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
          // iteration count is 1 => no loop
          case Cst(1) =>
            C.AST.Stmts(C.AST.Stmts(
              C.AST.Comment("iteration count is exactly 1, no loop emitted"),
              C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(f.init))))),
              updatedGen.cmd(p, env))
            /* FIXME?
          case _ if (range.start.min.min == Cst(0) && range.stop == Cst(1)) ||
                    (range.numVals.min == NegInf && range.numVals.max == Cst(1)) =>
            C.AST.Block(collection.Seq(
              C.AST.DeclStmt(init),
              C.AST.IfThenElse(cond, updatedGen.cmd(p, env) , None)
            ))
             */
          // default case
          case _ =>
            C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
              C.AST.Block(immutable.Seq(updatedGen.cmd(p, env))))
        }}))})
    }

    def codeGenOpenCLParForNat(f: OpenCLParForNat,
                               n: Nat,
                               a: Phrase[AccType],
                               i: NatIdentifier,
                               o: Phrase[AccType],
                               p: Phrase[CommType],
                               env: Environment): Stmt = {
      assert(!f.unroll)
      val cI = C.AST.DeclRef(f.name)
      val range = RangeAdd(f.init, n, f.step)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

        val init = OpenCL.AST.VarDecl(cI.name, C.AST.Type.int, AddressSpace.Private, init = Some(C.AST.ArithmeticExpr(range.start)))
        val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
        val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + range.step))

        // FIRST we must substitute in the indexing of o in the phrase
        Phrase.substitute(a `@d` i, `for` = o, `in` = p) |> (p =>
          // THEN and only THEN we can change the type to use the new index var
          PhraseType.substitute(NamedVar(cI.name, range), `for` = i, in = p) |> (p =>

            env.copy(identEnv = env.identEnv.map {
              case (Identifier(name, AccType(dt)), declRef) =>
                (Identifier(name, AccType(DataType.substitute(NamedVar(cI.name, range), `for` = i, in = dt))), declRef)
              case (Identifier(name, ExpType(dt, read)), declRef) =>
                (Identifier(name, ExpType(DataType.substitute(NamedVar(cI.name, range), `for` = i, in = dt), read)), declRef)
              case x => x
            }) |> (env =>

              range.numVals match {
                // iteration count is 0 => skip body; no code to be emitted
                case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
                // TODO: substitute a `@d` Cst(0) earlier ...
                //                // iteration count is 1 => no loop
                //                case Cst(1) =>
                //                  C.AST.Stmts(C.AST.Stmts(
                //                    C.AST.Comment("iteration count is exactly 1, no loop emitted"),
                //                    C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
                //                    updatedGen.cmd(p, env))
                // default case
                case _ =>
                  C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
                    C.AST.Block(immutable.Seq(updatedGen.cmd(p, env))))
              })))
      })
    }

    def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
      d match {
        case VectorData(vector) =>
          OpenCL.AST.VectorLiteral(
            typ(d.dataType).asInstanceOf[OpenCL.AST.VectorType],
            vector.map(codeGenLiteral))
        case _ => CCodeGen.codeGenLiteral(d)
      }
    }

    def makePointerDecl(name: String,
                        a: AddressSpace,
                        elemType: DataType,
                        expr: Expr): Stmt = {
      import shine.C.AST._
      import shine.OpenCL.AST.{VarDecl, PointerType}
      DeclStmt(
        VarDecl(name, PointerType(a, typ(elemType)), AddressSpace.Private, Some(expr)))
    }

    def codeGenVectorLiteral(n: Int, dt: ScalarType,
                             f: Int => Phrase[ExpType],
                             env: Environment,
                             cont: Expr => Stmt,
                             elements: Vector[Expr] = Vector()): Stmt = {
      if (elements.length < n) {
        exp(f(elements.length), env, Nil, e =>
          codeGenVectorLiteral(n, dt, f, env, cont, elements :+ e))
      } else {
        cont(OpenCL.AST.VectorLiteral(typ(VectorType(n, dt)).asInstanceOf[OpenCL.AST.VectorType], elements))
      }
    }
  }
}
