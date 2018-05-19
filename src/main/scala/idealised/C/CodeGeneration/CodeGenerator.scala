package idealised.C.CodeGeneration

import idealised._
import idealised.DPIA._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.C._
import idealised.C.AST._
import lift.arithmetic.{Cst, NamedVar}

import scala.collection.immutable
import scala.collection.mutable
import scala.language.implicitConversions

object CCodeGen {
  type Environment = immutable.Map[String, String]
  type Path = immutable.List[Nat]

  type Declarations = mutable.ListBuffer[Decl]
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  type CPrimitiveCodeGen = DPIA.Compilation.PrimitiveCodeGen[Environment, Path, C.AST.Stmt, C.AST.Expr, C.AST.Decl]

  def apply(p: Phrase[CommandType],
            env: CCodeGen.Environment,
            primitiveCodeGen: CCodeGen.CPrimitiveCodeGen): CCodeGen =
    apply(p, env, primitiveCodeGen, mutable.ListBuffer[Decl](), immutable.Map[String, lift.arithmetic.Range]())
}

case class CCodeGen(p: Phrase[CommandType],
                    env: CCodeGen.Environment,
                    primitiveCodeGen: CCodeGen.CPrimitiveCodeGen,
                    decls: CCodeGen.Declarations,
                    ranges: CCodeGen.Ranges)
  extends DPIA.Compilation.CodeGenerator[CCodeGen.Environment, CCodeGen.Path, C.AST.Stmt, C.AST.Expr, C.AST.Decl]
{
  type Environment = CCodeGen.Environment
  type Path = CCodeGen.Path
  type Stmt = C.AST.Stmt
  type Decl = C.AST.Decl
  type Expr = C.AST.Expr

  def addDeclaration(decl: Decl): Unit = {
    if ( decls.exists( _.name == decl.name ) ) {
      println(s"warning: declaration with name ${decl.name} already defined")
    } else {
      decls += decl
    }
  }

  def updatedRanges(key: String, value: lift.arithmetic.Range): CCodeGen =
    copy(ranges = ranges.updated(key, value))

  override def generate: (scala.Seq[Decl], Stmt) = {
    val stmt = cmd(p, env)
    (decls, Block(immutable.Seq(stmt)))
  }

  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case c: GeneratableCommand => c.codeGen(this)(env)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | Identifier(_, _) |
           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
        error(s"Don't know how to generate code for $p")
    }
  }

  override def acc(phrase: Phrase[AccType], env: Environment, ps: Path): Expr = {
    phrase match {
      case Identifier(x, _) =>  primitiveCodeGen.generateAccess(env(x), ps.reverse)
      case SplitAcc(_, m, _, a) => ps match {
        case i :: ps =>                       acc(a, env, i / m :: i % m :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case JoinAcc(_, m, _, a) => ps match {
        case  i :: j :: ps =>                 acc(a, env, i * m + j :: ps)
        case _ :: Nil | Nil =>                error(s"Expected path to contain at least two elements")
      }
      case RecordAcc1(_, _, a) =>             acc(a, env, Cst(1) :: ps)
      case RecordAcc2(_, _, a) =>             acc(a, env, Cst(2) :: ps)
      case ZipAcc1(_, _, _, a) => ps match {
        case i :: ps =>                       acc(a, env, i :: Cst(1) :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case ZipAcc2(_, _, _, a) => ps match {
        case i :: ps =>                       acc(a, env, i :: Cst(2) :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }

      case a: GeneratableAcc => a.codeGen(this)(env, ps)

      case Proj1(pair) =>       acc(Lifting.liftPair(pair)._1, env, ps)
      case Proj2(pair) =>       acc(Lifting.liftPair(pair)._2, env, ps)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        error(s"Don't know how to generate code for $p")
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, ps: Path): Expr = {
    phrase match {
      case Identifier(x, _) =>  primitiveCodeGen.generateAccess(env(x), ps.reverse)
      case Phrases.Literal(n) => ps match {
        case Nil =>             primitiveCodeGen.codeGenLiteral(n)
        case _ =>               error(s"Expected path to be empty")
      }
      case UnaryOp(op, e) => ps match {
        case Nil =>             primitiveCodeGen.codeGenUnaryOp(op, exp(e, env, List()))
        case _ =>               error(s"Expected path to be empty")
      }
      case BinOp(op, e1, e2) => ps match {
        case Nil =>             primitiveCodeGen.codeGenBinaryOp(op, exp(e1, env, List()), exp(e2, env, List()))
        case _ =>               error(s"Expected path to be empty")
      }
      case Zip(_, _, _, e1, e2) => ps match {
        case i :: Cst(1) :: ps =>             exp(e1, env, i :: ps)
        case i :: Cst(2) :: ps =>             exp(e2, env, i :: ps)
        case _ =>                             error(s"Expected path to have at least two values and contain " +
          s"1 or 2 as second value.")
      }
      case Split(n, _, _, e) => ps match {
        case i :: j :: ps =>                  exp(e, env, i * n + j :: ps)
        case _ :: Nil | Nil =>                error(s"Expected path to contain at least two elements")
      }
      case Join(n, _, _, e) => ps match {
        case i :: ps =>                       exp(e, env, i / n :: i % n :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case Record(_, _, e1, e2) => ps match {
        case Cst(1) :: ps =>                  exp(e1, env, ps)
        case Cst(2) :: ps =>                  exp(e2, env, ps)
        case _ =>                             error(s"Expected path to have at least two values and contain " +
          s"1 or 2 as second value.")
      }
      case Fst(_, _, e) =>                    exp(e, env, Cst(1) :: ps)
      case Snd(_, _, e) =>                    exp(e, env, Cst(2) :: ps)

      case e: GeneratableExp => e.codeGen(this)(env, ps)

      // TODO: investigate why still required
      case Proj1(pair) =>                     exp(Lifting.liftPair(pair)._1, env, ps)
      case Proj2(pair) =>                     exp(Lifting.liftPair(pair)._2, env, ps)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        error(s"Don't know how to generate code for $p")
    }
  }
}





object CodeGenerator {
  type Environment = immutable.Map[String, String]
  type Path = immutable.List[Nat]
  type Declarations = mutable.ListBuffer[Decl]
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  def apply(p: Phrase[CommandType],
            env: CodeGenerator.Environment,
            primitiveCodeGen: PrimitiveCodeGen): (scala.collection.Seq[Decl], Block) = {
    val gen = CodeGenerator(primitiveCodeGen, mutable.ListBuffer[Decl](), immutable.Map[String, lift.arithmetic.Range]())
    val b = Block(immutable.Seq(gen.cmd(p, env)(gen)))
    (gen.decls, b)
  }

//  def oldApply(p: Phrase[CommandType], primitiveCodeGen: PrimitiveCodeGen): (scala.collection.Seq[Decl], Block) = {
//    val gen = CodeGenerator(primitiveCodeGen, scala.collection.mutable.ListBuffer[Decl](), immutable.Map[String, lift.arithmetic.Range]())
//        val b = gen.cmd(p, Block(immutable.Seq()), gen)
//    (gen.decls, b)
//  }
}

case class CodeGenerator(primitiveCodeGen: PrimitiveCodeGen,
                         decls: CodeGenerator.Declarations,
                         ranges: CodeGenerator.Ranges) {

  def addDeclaration(decl: Decl): Unit = {
    if ( decls.exists( _.name == decl.name ) ) {
      println(s"warning: declaration with name ${decl.name} already defined")
    } else {
      decls += decl
    }
  }

  def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    copy(ranges = ranges.updated(key, value))

  def cmd(phrase: Phrase[CommandType], env: CodeGenerator.Environment)
         (implicit gen: CodeGenerator): Stmt = {
    phrase match {
      case _: Skip =>                         primitiveCodeGen.codeGenSkip
      case Seq(p1, p2) =>                     primitiveCodeGen.codeGenSeq(p1, p2, env)
      case Assign(_, a, e) =>                 primitiveCodeGen.codeGenAssign(a, e, env)
      case New(dt, _, Lambda(v, p)) =>        primitiveCodeGen.codeGenNew(dt, v, p, env)
      case For(n, Lambda(i, p)) =>            primitiveCodeGen.codeGenFor(n, i, p, env)
      case ParFor(n, dt, a, Lambda(i, Lambda(o, p))) =>
                                              primitiveCodeGen.codeGenParFor(n, dt, a, i, o, p, env)
    }
  }

  def acc(p: Phrase[AccType], env: CodeGenerator.Environment, ps: CodeGenerator.Path)
         (implicit gen: CodeGenerator): Expr = {
    p match {
      case Identifier(x, _) =>                generateAccess(env(x), ps.reverse)
      case IdxAcc(_, _, i, a) =>
        gen.exp(i, env, List()) match {
          case ArithmeticExpr(j) =>           gen.acc(a, env, j :: ps)
          case DeclRef(j) =>                  gen.acc(a, env, NamedVar(j) :: ps)
          case idealised.C.AST.Literal(j) =>  gen.acc(a, env, NamedVar(j) :: ps)
        }
      case SplitAcc(_, m, _, a) => ps match {
        case i :: ps =>                       gen.acc(a, env, i / m :: i % m :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case JoinAcc(_, m, _, a) => ps match {
        case  i :: j :: ps =>                 gen.acc(a, env, i * m + j :: ps)
        case _ :: Nil | Nil =>                error(s"Expected path to contain at least two elements")
      }
      case RecordAcc1(_, _, a) =>             gen.acc(a, env, Cst(1) :: ps)
      case RecordAcc2(_, _, a) =>             gen.acc(a, env, Cst(2) :: ps)
      case ZipAcc1(_, _, _, a) => ps match {
        case i :: ps =>                       gen.acc(a, env, i :: Cst(1) :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case ZipAcc2(_, _, _, a) => ps match {
        case i :: ps =>                       gen.acc(a, env, i :: Cst(2) :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }

      // TODO: investigate why still required
      case Proj1(pair) =>                     gen.acc(Lifting.liftPair(pair)._1, env, ps)
      case Proj2(pair) =>                     gen.acc(Lifting.liftPair(pair)._2, env, ps)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        error(s"Don't know how to generate code for $p")
    }
  }

  def exp(p: Phrase[ExpType], env: CodeGenerator.Environment, ps: CodeGenerator.Path)
         (implicit gen: CodeGenerator): Expr = {
    p match {
      case Identifier(x, _) =>                generateAccess(env.getOrElse(x, x), ps.reverse)
      case Phrases.Literal(n) => ps match {
        case Nil =>                           idealised.C.AST.Literal(n.toString)
        case _ =>                             error(s"Expected path to be empty")
      }
      case UnaryOp(op, e) => ps match {
        case Nil =>                           UnaryExpr(op, gen.exp(e, env, List()))
        case _ =>                             error(s"Expected path to be empty")
      }
      case BinOp(op, e1, e2) => ps match {
        case Nil =>                           BinaryExpr(gen.exp(e1, env, List()), op, gen.exp(e2, env, List()))
        case _ =>                             error(s"Expected path to be empty")
      }
      case Zip(_, _, _, e1, e2) => ps match {
        case i :: Cst(1) :: ps =>             gen.exp(e1, env, i :: ps)
        case i :: Cst(2) :: ps =>             gen.exp(e2, env, i :: ps)
        case _ =>                             error(s"Expected path to have at least two values and contain " +
                                                    s"1 or 2 as second value.")
      }
      case Split(n, _, _, e) => ps match {
        case i :: j :: ps =>                  gen.exp(e, env, i * n + j :: ps)
        case _ :: Nil | Nil =>                error(s"Expected path to contain at least two elements")
      }
      case Join(n, _, _, e) => ps match {
        case i :: ps =>                       gen.exp(e, env, i / n :: i % n :: ps)
        case Nil =>                           error(s"Expected path to be not empty")
      }
      case Record(_, _, e1, e2) => ps match {
        case Cst(1) :: ps =>                  gen.exp(e1, env, ps)
        case Cst(2) :: ps =>                  gen.exp(e2, env, ps)
        case _ =>                             error(s"Expected path to have at least two values and contain " +
                                                    s"1 or 2 as second value.")
      }
      case Fst(_, _, e) =>                    gen.exp(e, env, Cst(1) :: ps)
      case Snd(_, _, e) =>                    gen.exp(e, env, Cst(2) :: ps)
      case Idx(_, _, i, e) =>
        gen.exp(i, env, List()) match {
          case ArithmeticExpr(j) =>           gen.exp(e, env, j :: ps)
          case DeclRef(j) =>                  gen.exp(e, env, NamedVar(j) :: ps)
          case idealised.C.AST.Literal(j) =>  gen.exp(e, env, NamedVar(j) :: ps)
        }

      // TODO: investigate why still required
      case Proj1(pair) =>                     gen.exp(Lifting.liftPair(pair)._1, env, ps)
      case Proj2(pair) =>                     gen.exp(Lifting.liftPair(pair)._2, env, ps)

      case Apply(_, _) | NatDependentApply(_, _) | TypeDependentApply(_, _) |
           Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        error(s"Don't know how to generate code for $p")
    }
  }

  def generateAccess(identifier: String, paths: CodeGenerator.Path): Expr = {
    paths match {
      case List() => DeclRef(identifier)
      case List(idx) => ArraySubscript(DeclRef(identifier), ArithmeticExpr(idx))
    }
  }

//  def cmd(p: Phrase[CommandType], block: Block, gen: CodeGenerator): Block = {
//    p match {
//      case DPIA.Phrases.IfThenElse(condP, thenP, elseP) =>
//        val trueBlock = cmd(thenP, Block(), gen)
//        val falseBlock = cmd(elseP, Block(), gen)
//        block + idealised.C.AST.IfThenElse(exp(condP, gen), trueBlock, Some(falseBlock))
//
//      case c: GeneratableComm => c.codeGen(block, gen)
//
//      case a: Assign => primitiveCodeGen.codeGen(a, block, gen)
//      case d: DoubleBufferFor => primitiveCodeGen.codeGen(d, block, gen)
//      case f: For => primitiveCodeGen.codeGen(f, block, gen)
//      case n: New => primitiveCodeGen.codeGen(n, block, gen)
//      case s: idealised.DPIA.ImperativePrimitives.Seq => primitiveCodeGen.codeGen(s, block, gen)
//      case s: idealised.DPIA.ImperativePrimitives.Skip => primitiveCodeGen.codeGen(s, block, gen)
//
//      case p: ParFor => primitiveCodeGen.codeGen(p, block, gen)
//
//      case Apply(_, _) | NatDependentApply(_, _) |
//           TypeDependentApply(_, _) | Identifier(_, _) |
//           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
//        throw new Exception(s"Don't know how to generate code for $p with code generator for ${primitiveCodeGen.name}")
//    }
//  }
//
//  def exp(p: Phrase[ExpType], gen: CodeGenerator): Expr = {
//    p match {
//      case BinOp(op, lhs, rhs) =>
//        BinaryExpr(exp(lhs, gen), convertBinaryOp(op), exp(rhs, gen))
//      case Identifier(name, _) => DeclRef(name)
//      case DPIA.Phrases.Literal(d) => d match {
//        case i: IntData     => idealised.C.AST.Literal(i.i.toString)
//        case b: BoolData    => idealised.C.AST.Literal(b.b.toString)
//        case f: FloatData   => idealised.C.AST.Literal(f.f.toString)
//        case v: VectorData  => idealised.C.AST.Literal(toString(v))
//        case r: RecordData  => idealised.C.AST.Literal(toString(r))
//        case a: ArrayData   => idealised.C.AST.Literal(toString(a))
//
//        case i: IndexData   => ArithmeticExpr(i.n)
//      }
//      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, gen)
//      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, gen)
//      case UnaryOp(op, x)       => UnaryExpr(convertUnaryOp(op), exp(x, gen))
//
//      case f: Fst       => primitiveCodeGen.codeGen(f, gen, f.t.dataType, List(), List())
//      case i: Idx       => primitiveCodeGen.codeGen(i, gen, i.t.dataType, List(), List())
//      case r: Record    => primitiveCodeGen.codeGen(r, gen, r.t.dataType, List(), List())
//      case s: Snd       => primitiveCodeGen.codeGen(s, gen, s.t.dataType, List(), List())
//      case t: TruncExp  => primitiveCodeGen.codeGen(t, gen, t.t.dataType, List(), List())
//
//      case g: GeneratableExp => g.codeGen(gen)
//
//      case Apply(_, _) | NatDependentApply(_, _) |
//           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
//        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
//    }
//  }
//
//  def computeIndex(dt: DataType, arrayIndices: List[Nat], tupleIndices: List[Nat], idx: Nat): Nat = {
//    dt match {
//      case _: DPIA.Types.BasicType => idx
//      case DPIA.Types.ArrayType(n, et) =>
//        val i :: is = arrayIndices
//        computeIndex(et, is, tupleIndices, (idx * n) + i)
//      case RecordType(_, _) => idx // TODO: think about this more ...
//      case _: DataTypeIdentifier => ???
//    }
//  }
//
//  def exp(p: Phrase[ExpType], gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    p match {
//      case Identifier(name, t) =>
//
//        val index: Nat = {
//          if (arrayAccess.nonEmpty) {
//            computeIndex(t.dataType, arrayAccess, tupleAccess, 0)
//          } else {
//            null
//          }
//        }
//
//        val suffix = {
//          if (tupleAccess.nonEmpty) {
//            tupleAccess.map {
//              case lift.arithmetic.Cst(1) => "._1"
//              case lift.arithmetic.Cst(2) => "._2"
//              case _ => throw new Exception("This should not happen")
//            }.foldLeft("")(_ + _)
//          } else {
//            ""
//          }
//        }
//
//        val (originalType, currentType) = (t.dataType, dt)
//        (originalType, currentType) match {
//          case (DPIA.Types.ArrayType(_, st1), VectorType(n, st2))
//            if DataType.scalarType(st1) == DataType.scalarType(st2) => ???
//          case _ =>
//            ArraySubscript(DeclRef(name + suffix), ArithmeticExpr(index))
//        }
//
//      case l: DPIA.Phrases.Literal => primitiveCodeGen.codeGen(l, gen, dt, arrayAccess, tupleAccess)
//
//      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, gen, dt, arrayAccess, tupleAccess)
//      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, gen, dt, arrayAccess, tupleAccess)
//
//      case v: ViewExp => v.codeGen(gen, dt, arrayAccess, tupleAccess)
//
//      case g: Gather => primitiveCodeGen.codeGen(g, gen, dt, arrayAccess, tupleAccess)
//      case j: Join => primitiveCodeGen.codeGen(j, gen, dt, arrayAccess, tupleAccess)
//      case s: Split => primitiveCodeGen.codeGen(s, gen, dt, arrayAccess, tupleAccess)
//      case z: Zip => primitiveCodeGen.codeGen(z, gen, dt, arrayAccess, tupleAccess)
//      case u: Unzip => primitiveCodeGen.codeGen(u, gen, dt, arrayAccess, tupleAccess)
//
//      case f: Fst       => primitiveCodeGen.codeGen(f, gen, dt, arrayAccess, tupleAccess)
//      case i: Idx       => primitiveCodeGen.codeGen(i, gen, dt, arrayAccess, tupleAccess)
//      case r: Record    => primitiveCodeGen.codeGen(r, gen, dt, arrayAccess, tupleAccess)
//      case s: Snd       => primitiveCodeGen.codeGen(s, gen, dt, arrayAccess, tupleAccess)
//      case t: TruncExp  => primitiveCodeGen.codeGen(t, gen, dt, arrayAccess, tupleAccess)
//
//      case g: GeneratableExp => g.codeGen(gen)
//
//      case Apply(_, _) | NatDependentApply(_, _) |
//           TypeDependentApply(_, _) |
//           BinOp(_, _, _) | UnaryOp(_, _) |
//           DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
//        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
//    }
//  }
//
//
//  def acc(p: Phrase[AccType], value: Expr, gen: CodeGenerator): Expr = {
//    p match {
//      case Identifier(name, t) =>
//        t.dataType match {
//          case _: DPIA.Types.BasicType => Assignment(DeclRef(name), value)
//          case _ => throw new Exception(s"Don't know how to generate assignment into variable $name of type $t")
//        }
//      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, gen)
//      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, gen)
//
//      case f: RecordAcc1    => primitiveCodeGen.codeGen(f, value, gen, f.t.dataType, List(), List())
//      case i: IdxAcc        => primitiveCodeGen.codeGen(i, value, gen, i.t.dataType, List(), List())
//      case j: JoinAcc       => primitiveCodeGen.codeGen(j, value, gen, j.t.dataType, List(), List())
//      case s: RecordAcc2    => primitiveCodeGen.codeGen(s, value, gen, s.t.dataType, List(), List())
//      case s: SplitAcc      => primitiveCodeGen.codeGen(s, value, gen, s.t.dataType, List(), List())
//      case t: TruncAcc      => primitiveCodeGen.codeGen(t, value, gen, t.t.dataType, List(), List())
//
//      case Apply(_, _) | NatDependentApply(_, _) |
//           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
//        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
//    }
//  }
//
//  def acc(p: Phrase[AccType], value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    p match {
//      case Identifier(name, t) =>
//        val index: Nat = {
//          if (arrayAccess.nonEmpty) {
//            computeIndex(t.dataType, arrayAccess, tupleAccess, 0)
//          } else {
//            null
//          }
//        }
//
//        val suffix = {
//          if (tupleAccess.nonEmpty) {
//            tupleAccess.map {
//              case lift.arithmetic.Cst(1) => "._1"
//              case lift.arithmetic.Cst(2) => "._2"
//              case _ => throw new Exception("This should not happen")
//            }.foldLeft("")(_ + _)
//          } else {
//            ""
//          }
//        }
//
//        val (originalType, currentType) = (t.dataType, dt)
//        (originalType, currentType) match {
//          case (DPIA.Types.ArrayType(_, st1), VectorType(n, st2))
//            if DataType.scalarType(st1) == DataType.scalarType(st2) => ???
//          case _ =>
//            Assignment(
//              ArraySubscript(DeclRef(name + suffix), ArithmeticExpr(index)),
//              value)
//        }
//
//      case v: ViewAcc => v.codeGen(gen, value, dt, arrayAccess, tupleAccess)
//
//      case f: RecordAcc1    => primitiveCodeGen.codeGen(f, value, gen, dt, arrayAccess, tupleAccess)
//      case i: IdxAcc        => primitiveCodeGen.codeGen(i, value, gen, dt, arrayAccess, tupleAccess)
//      case j: JoinAcc       => primitiveCodeGen.codeGen(j, value, gen, dt, arrayAccess, tupleAccess)
//      case s: RecordAcc2    => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
//      case s: SplitAcc      => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
//      case t: TruncAcc      => primitiveCodeGen.codeGen(t, value, gen, dt, arrayAccess, tupleAccess)
//      case s: ScatterAcc    => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
//      case u: UnzipAcc      => primitiveCodeGen.codeGen(u, value, gen, dt, arrayAccess, tupleAccess)
//
//      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, gen, dt, arrayAccess, tupleAccess)
//      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, gen, dt, arrayAccess, tupleAccess)
//
//      case Apply(_, _) | NatDependentApply(_, _) |
//           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
//        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
//    }
//  }

//  def toString(d: Data): String = {
//    d match {
//      case i: IntData => i.i.toString
//      case b: BoolData => b.b.toString
//      case f: FloatData => f.f.toString
//      case i: IndexData => i.n.toString
//      case v: VectorData => ???
//      case _: RecordData => ???
//      case _: ArrayData => ???
//    }
//  }
//
//  def toString(dt: DataType): String = {
//    dt match {
//      case b: DPIA.Types.BasicType => b match {
//        case Types.bool | Types.int => "int"
//        case Types.float => "float"
//        case _: IndexType => "int"
//        case _: VectorType => ???
//      }
//      case _: DPIA.Types.RecordType => ???
//      case _: DPIA.Types.ArrayType => ???
//      case _: DPIA.Types.DataTypeIdentifier => throw new Exception("This should not happen")
//    }
//  }

  private implicit def convertBinaryOp(op: idealised.SurfaceLanguage.Operators.Binary.Value): idealised.C.AST.BinaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Binary._
    op match {
      case ADD => BinaryOperator.+
      case SUB => BinaryOperator.-
      case MUL => BinaryOperator.*
      case DIV => BinaryOperator./
      case MOD => ???
      case GT  => BinaryOperator.>
      case LT  => BinaryOperator.<
    }
  }

  private implicit def convertUnaryOp(op: idealised.SurfaceLanguage.Operators.Unary.Value): idealised.C.AST.UnaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Unary._
    op match {
      case NEG => UnaryOperator.-
    }
  }

}
