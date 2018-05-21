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
import idealised.SurfaceLanguage.Operators
import lift.arithmetic._

import scala.collection.immutable
import scala.collection.mutable
import scala.language.implicitConversions

object CodeGenerator {
  type Environment = immutable.Map[String, String]
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
  extends DPIA.Compilation.CodeGenerator[CodeGenerator.Environment, CodeGenerator.Path, C.AST.Stmt, C.AST.Expr, C.AST.Decl]
{
  type Environment = CodeGenerator.Environment
  type Path = CodeGenerator.Path
  type Stmt = C.AST.Stmt
  type Decl = C.AST.Decl
  type Expr = C.AST.Expr

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

// case p: ParForVec => primitiveCodeGen.codeGen(p, block, gen)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | Identifier(_, _) |
           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def acc(phrase: Phrase[AccType], env: Environment, path: Path): Expr = {
    phrase match {
      case Identifier(x, _) =>    generateAccess(env(x), path.reverse)

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

// case i: IdxVec    => primitiveCodeGen.codeGen(i, gen, i.t.dataType, List(), List())

      case Proj1(pair) =>         acc(Lifting.liftPair(pair)._1, env, path)
      case Proj2(pair) =>         acc(Lifting.liftPair(pair)._2, env, path)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path): Expr = {
    phrase match {
      case Identifier(x, _) =>    generateAccess(env(x), path.reverse)

      case Phrases.Literal(n) => path match {
        case Nil =>               codeGenLiteral(n)
        case _ =>                 error(s"Expected path to be empty")
      }

      case UnaryOp(op, e) => path match {
        case Nil =>               codeGenUnaryOp(op, exp(e, env, List()))
        case _ =>                 error(s"Expected path to be empty")
      }
      case BinOp(op, e1, e2) => path match {
        case Nil =>               codeGenBinaryOp(op, exp(e1, env, List()), exp(e2, env, List()))
        case _ =>                 error(s"Expected path to be empty")
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

      case e: GeneratableExp =>   e.codeGen(this)(env, path)
// case i: IdxVec    => primitiveCodeGen.codeGen(i, gen, dt, arrayAccess, tupleAccess)

      // TODO: investigate why still required
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
                          v: Identifier[ExpType x AccType],
                          p: Phrase[CommandType],
                          env: Environment,
                          gen: CodeGenerator.this.type): Stmt = {
    C.AST.Block(immutable.Seq(
      C.AST.DeclStmt(C.AST.VarDecl(v.name, C.AST.Type.fromDataType(dt))),
      gen.cmd( Phrase.substitute(Pair(π1(v), π2(v)), `for`=v, `in`=p),
        env + ( v.name -> v.name, v.name -> v.name ) )
    ))
  }

  override def codeGenFor(n: Nat,
                          i: Identifier[ExpType],
                          p: Phrase[CommandType],
                          env: Environment,
                          gen: CodeGenerator.this.type): Stmt = {
    val i_ = freshName("i_")
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_, range)

    val init = C.AST.VarDecl(i_, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(C.AST.DeclRef(i_), C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = C.AST.Assignment(C.AST.DeclRef(i_), C.AST.ArithmeticExpr(NamedVar(i_, range) + 1))

    C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
      C.AST.Block(immutable.Seq(updatedGen.cmd(p, env + (i.name -> i_)))))
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

  override def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
    C.AST.Literal(d.toString)
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
          case (name, dt) => C.AST.VarDecl(name, C.AST.Type.fromDataType(dt)) },
        body = C.AST.Code(funDecl.body)))

    C.AST.FunCall(C.AST.DeclRef(funDecl.name), args.map(gen.exp(_, env, ps)))
  }

  override def generateAccess(identifier: String, paths: Path): Expr = {
    paths match {
      case List() => C.AST.DeclRef(identifier)
      case List(idx) => C.AST.ArraySubscript(C.AST.DeclRef(identifier), C.AST.ArithmeticExpr(idx))
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
    }
  }

  private implicit def convertUnaryOp(op: idealised.SurfaceLanguage.Operators.Unary.Value): idealised.C.AST.UnaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Unary._
    op match {
      case NEG => C.AST.UnaryOperator.-
    }
  }
}

