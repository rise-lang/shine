package idealised.C.CodeGeneration

import idealised._
import idealised.DPIA._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.SurfaceLanguage.Primitives.ForeignFunctionDeclaration
import idealised.DPIA.DSL._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{BoolData, FloatData, IndexData, IntData, VectorData}
import idealised.OpenCL.FunctionalPrimitives.VectorFromScalar
import idealised.SurfaceLanguage.Operators
import lift.arithmetic._

import scala.collection.immutable
import scala.collection.mutable
import scala.language.implicitConversions

object CodeGenerator {
  final case class Environment(identEnv: immutable.Map[Identifier[_ <: BasePhraseTypes], C.AST.DeclRef],
                               commEnv: immutable.Map[Identifier[CommandType], C.AST.Stmt]) {
    def updatedIdentEnv(kv: (Identifier[_ <: BasePhraseTypes], C.AST.DeclRef)): Environment = {
      this.copy(identEnv = identEnv + kv)
    }

    def updatedCommEnv(kv: (Identifier[CommandType], C.AST.Stmt)): Environment = {
      this.copy(commEnv = commEnv + kv)
    }
  }

  type Path = immutable.List[Nat]

  type Declarations = mutable.ListBuffer[C.AST.Decl]
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  def apply(p: Phrase[CommandType], env: CodeGenerator.Environment): CodeGenerator =
    new CodeGenerator(p, env, mutable.ListBuffer[C.AST.Decl](), immutable.Map[String, lift.arithmetic.Range]())
}

class CodeGenerator(val p: Phrase[CommandType],
                    val env: CodeGenerator.Environment,
                    val decls: CodeGenerator.Declarations,
                    val ranges: CodeGenerator.Ranges)
  extends DPIA.Compilation.CodeGenerator[CodeGenerator.Environment, CodeGenerator.Path, C.AST.Stmt, C.AST.Expr, C.AST.Decl, C.AST.DeclRef]
{
  type Environment = CodeGenerator.Environment
  type Path = CodeGenerator.Path
  type Stmt = C.AST.Stmt
  type Decl = C.AST.Decl
  type Expr = C.AST.Expr
  type Ident = C.AST.DeclRef

  override def name: String = "C"

  def addDeclaration(decl: Decl): Unit = {
    if ( decls.exists( _.name == decl.name ) ) {
      println(s"warning: declaration with name ${decl.name} already defined")
    } else {
      decls += decl
    }
  }

  def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(p, env, decls, ranges.updated(key, value))

  override def generate: (scala.Seq[Decl], Stmt) = {
    val stmt = cmd(p, env)
    (decls, stmt)
  }

  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case c: GeneratableCommand => c.codeGen(this)(env)

      case Phrases.IfThenElse(cond, thenP, elseP) =>
        C.AST.IfThenElse(
          exp(cond, env, Nil), cmd(thenP, env), Some(cmd(elseP, env)))

      case i: Identifier[CommandType] => env.commEnv(i)

      case Proj1(pair) => cmd(Lifting.liftPair(pair)._1, env)
      case Proj2(pair) => cmd(Lifting.liftPair(pair)._2, env)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           _: CommandPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def acc(phrase: Phrase[AccType], env: Environment, path: Path): Expr = {
    phrase match {
      case i@Identifier(_, AccType(dt)) => generateAccess(dt, env.identEnv.applyOrElse(i, (_: Phrase[_]) => { println(i); println(env); ??? }), path.reverse)

      case SplitAcc(_, m, _, a) => path match {
        case i :: ps =>           acc(a, env, i / m :: i % m :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }
      case JoinAcc(_, m, _, a) => path match {
        case  i :: j :: ps =>     acc(a, env, i * m + j :: ps)
        case _ :: Nil | Nil =>    error(s"Expected path to contain at least two elements")
      }

      case RecordAcc1(_, _, a) => acc(a, env, Cst(1) :: path)
      case RecordAcc2(_, _, a) => acc(a, env, Cst(2) :: path)

      case ZipAcc1(_, _, _, a) => path match {
        case i :: ps =>           acc(a, env, i :: Cst(1) :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }
      case ZipAcc2(_, _, _, a) => path match {
        case i :: ps =>           acc(a, env, i :: Cst(2) :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }
      case UnzipAcc(_, _, _, _) => ???

      case TruncAcc(_,_,_, a) =>  acc(a, env, path) // ???

      case ScatterAcc(_, _, idxF, a) => path match {
        case i :: ps =>           acc(a, env, OperationalSemantics.evalIndexExp(idxF(i)) :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }

      case a: GeneratableAcc =>   a.codeGen(this)(env, path)

      case Proj1(pair) =>         acc(Lifting.liftPair(pair)._1, env, path)
      case Proj2(pair) =>         acc(Lifting.liftPair(pair)._2, env, path)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path): Expr = {
    phrase match {
      case i@Identifier(_, ExpType(dt)) => generateAccess(dt, env.identEnv.applyOrElse(i, (_: Phrase[_]) => { println(i); println(env); ??? }), path.reverse)

      case Phrases.Literal(n) => (path, n.dataType) match {
        case (Nil, _: IndexType)  =>        codeGenLiteral(n)
        case (Nil, _: ScalarType) =>        codeGenLiteral(n)
        case (i :: Nil, _: VectorType) =>   C.AST.ArraySubscript(codeGenLiteral(n), C.AST.ArithmeticExpr(i))
        case (_ :: _ :: Nil, _: ArrayType) => C.AST.Literal("0.0f") // TODO: (used in gemm like this) !!!!!!!
        case _ =>                 error(s"Unexpected: $n $path")
      }

      case UnaryOp(op, e) => phrase.t.dataType match {
        case _: ScalarType => path match {
          case Nil =>             codeGenUnaryOp(op, exp(e, env, List()))
          case _ =>               error(s"Expected path to be empty")
        }
        case _: VectorType => path match {
          case i :: ps =>         codeGenUnaryOp(op, exp(e, env, i :: ps))
          case _ =>               error(s"Expected path to be not empty")
        }
        case _ =>                 error(s"Expected scalar or vector types")
      }

      case BinOp(op, e1, e2) => phrase.t.dataType match {
        case _: ScalarType => path match {
          case Nil =>             codeGenBinaryOp(op, exp(e1, env, List()), exp(e2, env, List()))
          case _ =>               error(s"Expected path to be empty")
        }
        case _: VectorType => path match {
          case i :: ps =>         codeGenBinaryOp(op, exp(e1, env, i :: ps), exp(e2, env, i :: ps))
          case _ =>               error(s"Expected path to be not empty")
        }
        case _ =>                 error(s"Expected scalar or vector types")
      }

      case Split(n, _, _, e) => path match {
        case i :: j :: ps =>      exp(e, env, i * n + j :: ps)
        case _ :: Nil | Nil =>    error(s"Expected path to contain at least two elements")
      }
      case Join(n, _, _, e) => path match {
        case i :: ps =>           exp(e, env, i / n :: i % n :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }

      case Zip(_, _, _, e1, e2) => path match {
        case i :: Cst(1) :: ps => exp(e1, env, i :: ps)
        case i :: Cst(2) :: ps => exp(e2, env, i :: ps)
        case _ =>                 error(s"Expected path to have at least two values and contain " +
          s"1 or 2 as second value.")
      }
      case Unzip(_, _, _, _) =>   ???

      case Record(_, _, e1, e2) => path match {
        case Cst(1) :: ps =>      exp(e1, env, ps)
        case Cst(2) :: ps =>      exp(e2, env, ps)
        case _ =>                 error(s"Expected path to have at least two values and contain " +
          s"1 or 2 as second value.")
      }
      case Fst(_, _, e) =>        exp(e, env, Cst(1) :: path)
      case Snd(_, _, e) =>        exp(e, env, Cst(2) :: path)

      case TruncExp(_,_,_, e) =>  exp(e, env, path) // ???

      case Gather(_, _, idxF, a) => path match {
        case i :: ps =>           exp(a, env, OperationalSemantics.evalIndexExp(idxF(i)) :: ps)
        case Nil =>               error(s"Expected path to be not empty")
      }

      case Slide(_, _, s2, _, e) => path match {
        case i :: j :: ps =>      exp(e, env, i * s2 + j :: ps)
        case _ :: Nil | Nil =>    error(s"Expected path to contain at least two elements")
      }

        // TODO: this has to be refactored
      case VectorFromScalar(n, st, e) => path match {
        case i :: ps =>
          // in this case we index straight into the vector build from a single scalar
          // it is equivalent to return the scalar `e' without boxing and unboxing it
          exp(e, env, ps)
//          C.AST.ArraySubscript(
//            C.AST.Literal( "(" + s"($st[$n]){" + C.AST.Printer(exp(e, env, ps)) + "})" ),
//            C.AST.ArithmeticExpr(i))

        case Nil =>
          C.AST.Literal( "(" + s"($st[$n]){" + C.AST.Printer(exp(e, env, Nil)) + "})" )
      }

      case e: GeneratableExp =>   e.codeGen(this)(env, path)

      case Proj1(pair) =>         exp(Lifting.liftPair(pair)._1, env, path)
      case Proj2(pair) =>         exp(Lifting.liftPair(pair)._2, env, path)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def codeGenSkip: Stmt = C.AST.Comment("skip")

  override def codeGenSeq(p1: Phrase[CommandType],
                          p2: Phrase[CommandType],
                          env: Environment,
                          gen: CodeGenerator.this.type): Stmt = {
    C.AST.Stmts(gen.cmd(p1, env), gen.cmd(p2, env))
  }

  override def codeGenAssign(a: Phrase[AccType],
                             e: Phrase[ExpType],
                             env: Environment,
                             gen: CodeGenerator.this.type): Stmt = {
    C.AST.Assignment(gen.acc(a, env, List()), gen.exp(e, env, List()))
  }

  override def codeGenNew(dt: DataType,
                          v: Identifier[VarType],
                          p: Phrase[CommandType],
                          env: Environment,
                          gen: CodeGenerator.this.type): Stmt = {
    val ve = Identifier(s"${v.name}_e", v.t.t1)
    val va = Identifier(s"${v.name}_a", v.t.t2)
    val vC = C.AST.DeclRef(v.name)

    C.AST.Block(immutable.Seq(
      C.AST.DeclStmt(C.AST.VarDecl(vC.name, C.AST.Type.fromDataType(dt))),
      gen.cmd( Phrase.substitute(Pair(ve, va), `for`=v, `in`=p),
        env updatedIdentEnv (ve -> vC)
            updatedIdentEnv (va -> vC) ) ))
  }

  override def codeGenNewDoubleBuffer(dt: ArrayType,
                                      in: Phrase[ExpType],
                                      out: Phrase[AccType],
                                      ps: Identifier[VarType x CommandType x CommandType],
                                      p: Phrase[CommandType],
                                      env: Environment,
                                      gen: CodeGenerator.this.type): Stmt = {
    import C.AST._; import UnaryOperator._; import BinaryOperator._

    val   ve = Identifier(s"${ps.name}_e", ps.t.t1.t1.t1)
    val   va = Identifier(s"${ps.name}_a", ps.t.t1.t1.t2)
    val done = Identifier(s"${ps.name}_swap", ps.t.t1.t2)
    val swap = Identifier(s"${ps.name}_done", ps.t.t2)

    val    tmp1 = DeclRef(freshName("tmp1_"))
    val    tmp2 = DeclRef(freshName("tmp2_"))
    val  in_ptr = DeclRef(freshName("in_ptr_"))
    val out_ptr = DeclRef(freshName("out_ptr_"))
    val    flag = DeclRef(freshName("flag_"))

    Block(immutable.Seq(
      // create variables: `tmp1', `tmp2`, `in_ptr', and `out_ptr'
      DeclStmt(VarDecl(tmp1.name, Type.fromDataType(dt))),
      DeclStmt(VarDecl(tmp2.name, Type.fromDataType(dt))),
      makePointerDecl(in_ptr.name, dt.elemType, UnaryExpr(&, gen.exp(in, env, List(0)))),
      makePointerDecl(out_ptr.name, dt.elemType, tmp1),
      // create boolean flag used for swapping
      DeclStmt(VarDecl(flag.name, Type.uchar, Some(Literal("1")))),
      // generate body
      gen.cmd(
        Phrase.substitute(Pair(Pair(Pair(ve, va), swap), done), `for`=ps, `in`=p),
        env updatedIdentEnv (ve -> in_ptr) updatedIdentEnv (va -> out_ptr)
            updatedCommEnv  (swap -> {
              Block(immutable.Seq(
                Assignment( in_ptr, TernaryExpr(flag, tmp1, tmp2)),
                Assignment(out_ptr, TernaryExpr(flag, tmp2, tmp1)),
                // toggle flag with xor
                Assignment( flag, BinaryExpr(flag, ^, Literal("1"))) )) })
            updatedCommEnv  (done -> {
              Block(immutable.Seq(
                Assignment( in_ptr, TernaryExpr(flag, tmp1, tmp2)),
                Assignment(out_ptr, UnaryExpr(&, gen.acc(out, env, List(0)))) )) }))
    ))
  }

  override def codeGenFor(n: Nat,
                          i: Identifier[ExpType],
                          p: Phrase[CommandType],
                          env: Environment,
                          gen: CodeGenerator.this.type): Stmt = {
    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_.name, range)

    val n_ = applySubstitutions(n, env.identEnv)

    range.numVals match {
      // iteration count is 0 => skip body; no code to be emitted
      case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")

      // iteration count is 1 => no loop
      case Cst(1) =>
        C.AST.Stmts(C.AST.Stmts(
          C.AST.Comment("iteration count is exactly 1, no loop emitted"),
          C.AST.DeclStmt(C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))) ),
          updatedGen.cmd(p, env updatedIdentEnv (i -> i_)) )

      case _ =>
        // default case
        val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
        val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n_))
        val increment = C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(updatedGen.cmd(p, env updatedIdentEnv (i -> i_)))))
    }
  }

  override def codeGenParFor(n: Nat,
                             dt: DataType,
                             a: Phrase[AccType],
                             i: Identifier[ExpType],
                             o: Phrase[AccType],
                             p: Phrase[CommandType],
                             env: Environment,
                             gen: CodeGenerator.this.type): Stmt = {
    // in C the parFor is impl
    codeGenFor(n, i, Phrase.substitute(a `@` i, `for`=o, `in`=p), env, gen)
  }

  override def codeGenParForVec(n: Nat,
                                dt: DataType,
                                a: Phrase[AccType],
                                i: Identifier[ExpType],
                                o: Phrase[AccType],
                                p: Phrase[CommandType],
                                env: Environment,
                                gen: CodeGenerator.this.type): Stmt = {
    // in C the parFor is impl
    codeGenFor(n, i, Phrase.substitute(a `@v` i, `for`=o, `in`=p), env, gen)
  }

  override def codeGenIdxAcc(i: Phrase[ExpType],
                             a: Phrase[AccType],
                             env: Environment,
                             ps: Path,
                             gen: CodeGenerator.this.type): Expr = {
    val idx: ArithExpr = gen.exp(i, env, List()) match {
      case C.AST.DeclRef(name) => NamedVar(name, gen.ranges(name))
      case C.AST.ArithmeticExpr(ae) => ae
    }

    gen.acc(a, env, idx :: ps)
  }

  override def codeGenIdxVecAcc(i: Phrase[ExpType],
                                a: Phrase[AccType],
                                env: Environment,
                                ps: Path,
                                gen: CodeGenerator.this.type): Expr = {
    val idx: ArithExpr = gen.exp(i, env, List()) match {
      case C.AST.DeclRef(name) => NamedVar(name, gen.ranges(name))
      case C.AST.ArithmeticExpr(ae) => ae
    }

    gen.acc(a, env, idx :: ps)
  }

  override def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
    d match {
      case i: IndexData =>
        C.AST.ArithmeticExpr(i.n)
      case _: IntData | _: FloatData | _: BoolData =>
        C.AST.Literal(d.toString)
      case VectorData(vector) => d.dataType match {
        case VectorType(n, st) =>
          if (vector.distinct.length == 1) {
            C.AST.Literal( "(" + s"($st[$n]){" + vector.head  + "})" )
          } else {
            C.AST.Literal( "(" + s"($st[$n])" + vector.mkString("{", ",", "}") + ")" )
          }
        case _ => error(s"Expected vector type")
      }
      case _ =>   error(s"Expected scalar or vector types")
    }
  }

  override def codeGenUnaryOp(op: Operators.Unary.Value, e: Expr): Expr = {
    C.AST.UnaryExpr(op, e)
  }

  override def codeGenBinaryOp(op: Operators.Binary.Value,
                               e1: Expr,
                               e2: Expr): Expr = {
    C.AST.BinaryExpr(e1, op, e2)
  }

  override def codeGenIdx(i: Phrase[ExpType],
                          e: Phrase[ExpType],
                          env: Environment,
                          ps: Path,
                          gen: CodeGenerator.this.type): Expr = {
    val idx: ArithExpr = gen.exp(i, env, List()) match {
      case C.AST.DeclRef(name) => NamedVar(name, gen.ranges(name))
      case C.AST.ArithmeticExpr(ae) => ae
    }

    gen.exp(e, env, idx :: ps)
  }

  override def codeGenIdxVec(i: Phrase[ExpType],
                             e: Phrase[ExpType],
                             env: Environment,
                             ps: Path,
                             gen: CodeGenerator.this.type): Expr = {
    val idx: ArithExpr = gen.exp(i, env, List()) match {
      case C.AST.DeclRef(name) => NamedVar(name, gen.ranges(name))
      case C.AST.ArithmeticExpr(ae) => ae
    }

    gen.exp(e, env, idx :: ps)
  }

  override def codeGenForeignFunction(funDecl: ForeignFunctionDeclaration,
                                      inTs: collection.Seq[DataType],
                                      outT: DataType,
                                      args: collection.Seq[Phrase[ExpType]],
                                      env: Environment,
                                      ps: Path,
                                      gen: CodeGenerator.this.type): Expr = {
    gen.addDeclaration(
      C.AST.FunDecl(funDecl.name,
        returnType = C.AST.Type.fromDataType(outT),
        params = (funDecl.argNames zip inTs).map {
          case (name, dt) => C.AST.ParamDecl(name, C.AST.Type.fromDataType(dt)) },
        body = C.AST.Code(funDecl.body)))

    C.AST.FunCall(C.AST.DeclRef(funDecl.name), args.map(gen.exp(_, env, ps)))
  }

  override def generateAccess(dt: DataType,
                              identifier: Ident,
                              path: Path): Expr = {
    (dt, path) match {
      case (_: BasicType, Nil) =>  identifier

      case (_: VectorType, i :: Nil) =>
        val data = C.AST.StructMemberAccess(identifier, C.AST.DeclRef("data"))
        C.AST.ArraySubscript(data, C.AST.ArithmeticExpr(i))

      case (at: ArrayType, _) => generateArrayAccess(at, identifier, path, 0)

      case _ =>
        throw new Exception(s"Can't generate access for `$dt' with `${path.mkString("[", "::", "]")}'")
    }
  }

  private def generateArrayAccess(at: ArrayType, identifier: C.AST.DeclRef, path: Path, index: Nat): Expr = {
    (at, path) match {
      case (ArrayType(_, bt: BasicType), i :: Nil) =>
        C.AST.ArraySubscript(generateAccess(bt, identifier, Nil), C.AST.ArithmeticExpr(i + index))

      case (ArrayType(_, vt: VectorType), i :: j :: Nil) =>
        C.AST.ArraySubscript(generateAccess(vt, identifier, j :: Nil), C.AST.ArithmeticExpr(i + index))

      case (ArrayType(_, et@ArrayType(s, _)), i :: ps) =>
        generateArrayAccess(et, identifier, ps, (i * s) + index)

      case _ =>
        throw new Exception(s"Can't generate access for `$at' with `${path.mkString("[", "::", "]")}'")
    }
  }

  private implicit def convertBinaryOp(op: idealised.SurfaceLanguage.Operators.Binary.Value): idealised.C.AST.BinaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Binary._
    op match {
      case ADD => C.AST.BinaryOperator.+
      case SUB => C.AST.BinaryOperator.-
      case MUL => C.AST.BinaryOperator.*
      case DIV => C.AST.BinaryOperator./
      case MOD => ???
      case GT  => C.AST.BinaryOperator.>
      case LT  => C.AST.BinaryOperator.<
      case EQ  => C.AST.BinaryOperator.==
    }
  }

  private implicit def convertUnaryOp(op: idealised.SurfaceLanguage.Operators.Unary.Value): idealised.C.AST.UnaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Unary._
    op match {
      case NEG => C.AST.UnaryOperator.-
    }
  }

  private def applySubstitutions(n: Nat,
                                 identEnv: immutable.Map[Identifier[_ <: BasePhraseTypes], C.AST.DeclRef]): Nat = {
    // lift the substitutions from the Phrase level to the ArithExpr level
    val substitionMap = identEnv.filter(_._1.t match {
      case ExpType(IndexType(_)) => true
      case AccType(IndexType(_)) => true
      case _ => false
    }).map(i => (NamedVar(i._1.name), NamedVar(i._2.name)) ).toMap[ArithExpr, ArithExpr]
    ArithExpr.substitute(n, substitionMap)
  }

  private def makePointerDecl(name: String,
                              elemType: DataType,
                              expr: Expr): Stmt = {
    import C.AST._
    DeclStmt(
      VarDecl(name, PointerType(Type.fromDataType(elemType)), Some(expr)))
  }
}

