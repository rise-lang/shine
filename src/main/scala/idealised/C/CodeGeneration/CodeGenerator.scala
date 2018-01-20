package idealised.C.CodeGeneration


import idealised.DPIA
import idealised.DPIA._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.C._
import idealised.C.CodeGeneration.PrimitivesCodeGenerator.toC
import idealised.C.AST._

import scala.collection.immutable

object CodeGenerator {
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  case class Environment(ranges: Ranges = immutable.Map[String, lift.arithmetic.Range]()) {
    def updatedRanges(key: String, value: lift.arithmetic.Range): Environment =
      copy(ranges = ranges.updated(key, value))
  }

  def cmd(p: Phrase[CommandType], block: Block, env: Environment): Block = {
    p match {
      case DPIA.Phrases.IfThenElse(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), env)
        val falseBlock = cmd(elseP, Block(), env)
        block + idealised.C.AST.IfThenElse(exp(condP, env), trueBlock, Some(falseBlock))

      case c: GeneratableComm => c.codeGenCmd(block, env)

      case a: Assign => toC(a, block, env)
      case d: DoubleBufferFor => toC(d, block, env)
      case f: For => toC(f, block, env)
      case n: New => toC(n, block, env)
      case s: idealised.DPIA.ImperativePrimitives.Seq => toC(s, block, env)
      case s: idealised.DPIA.ImperativePrimitives.Skip => toC(s, block, env)

      case p: ParFor => toC(p, block, env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | Identifier(_, _) |
           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def exp(p: Phrase[ExpType], env: Environment): Expr = {
    p match {
      case BinOp(op, lhs, rhs) =>
        BinaryExpr(exp(lhs, env), convertBinaryOp(op), exp(rhs, env))
      case Identifier(name, _) => DeclRef(name)
      case DPIA.Phrases.Literal(d, _) => d match {
        case i: IntData     => idealised.C.AST.Literal(i.i.toString)
        case b: BoolData    => idealised.C.AST.Literal(b.b.toString)
        case f: FloatData   => idealised.C.AST.Literal(f.f.toString)
        case v: VectorData  => idealised.C.AST.Literal(toString(v))
        case r: RecordData  => idealised.C.AST.Literal(toString(r))
        case a: ArrayData   => idealised.C.AST.Literal(toString(a))

        case i: IndexData   => ArithmeticExpr(i.i)
      }
      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, env)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, env)
      case UnaryOp(op, x)       => UnaryExpr(convertUnaryOp(op), exp(x, env))

      case f: Fst       => toC(f, env, f.t.dataType, List(), List())
      case i: Idx       => toC(i, env, i.t.dataType, List(), List())
      case r: Record    => toC(r, env, r.t.dataType, List(), List())
      case s: Snd       => toC(s, env, s.t.dataType, List(), List())
      case t: TruncExp  => toC(t, env, t.t.dataType, List(), List())

      case g: GeneratableExp => g.codeGenExp(env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def computeIndex(dt: DataType, arrayIndices: List[Nat], tupleIndices: List[Nat], idx: Nat): Nat = {
    dt match {
      case _: DPIA.Types.BasicType => idx
      case DPIA.Types.ArrayType(n, et) =>
        val i :: is = arrayIndices
        computeIndex(et, is, tupleIndices, (idx * n) + i)
      case RecordType(_, _) => idx // TODO: think about this more ...
      case _: DataTypeIdentifier => ???
    }
  }

  def exp(p: Phrase[ExpType],
          env: Environment,
          dt: DataType,
          arrayAccess: List[Nat],
          tupleAccess: List[Nat]): Expr = {
    p match {
      case Identifier(name, t) =>

        val index: Nat = {
          if (arrayAccess.nonEmpty) {
            computeIndex(t.dataType, arrayAccess, tupleAccess, 0)
          } else {
            null
          }
        }

        val suffix = {
          if (tupleAccess.nonEmpty) {
            tupleAccess.map {
              case lift.arithmetic.Cst(1) => "._1"
              case lift.arithmetic.Cst(2) => "._2"
              case _ => throw new Exception("This should not happen")
            }.foldLeft("")(_ + _)
          } else {
            ""
          }
        }

        val (originalType, currentType) = (t.dataType, dt)
        (originalType, currentType) match {
          case (DPIA.Types.ArrayType(_, st1), VectorType(n, st2))
            if DataType.scalarType(st1) == DataType.scalarType(st2) => ???
          case _ =>
            ArraySubscript(DeclRef(name + suffix), ArithmeticExpr(index))
        }

      case l: DPIA.Phrases.Literal => toC(l, env, dt, arrayAccess, tupleAccess)

      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, env, dt, arrayAccess, tupleAccess)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, env, dt, arrayAccess, tupleAccess)

      case v: ViewExp => v.toOpenMP(env, arrayAccess, tupleAccess, dt)

      case g: Gather => toC(g, env, arrayAccess, tupleAccess, dt)
      case j: Join => toC(j, env, arrayAccess, tupleAccess, dt)
      case s: Split => toC(s, env, arrayAccess, tupleAccess, dt)
      case z: Zip => toC(z, env, arrayAccess, tupleAccess, dt)
      case u: Unzip => toC(u, env, arrayAccess, tupleAccess, dt)

      case f: Fst       => toC(f, env, dt, arrayAccess, tupleAccess)
      case i: Idx       => toC(i, env, dt, arrayAccess, tupleAccess)
      case r: Record    => toC(r, env, dt, arrayAccess, tupleAccess)
      case s: Snd       => toC(s, env, dt, arrayAccess, tupleAccess)
      case t: TruncExp  => toC(t, env, dt, arrayAccess, tupleAccess)

      case g: GeneratableExp => g.codeGenExp(env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) |
           BinOp(_, _, _) | UnaryOp(_, _) |
           DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }


  def acc(p: Phrase[AccType], value: Expr, env: Environment): Expr = {
    p match {
      case Identifier(name, t) =>
        t.dataType match {
          case _: DPIA.Types.BasicType => Assignment(DeclRef(name), value)
          case _ => throw new Exception(s"Don't know how to generate assignment into variable $name of type $t")
        }
      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, env)
      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, env)

      case f: RecordAcc1    => toC(f, value, env, f.t.dataType, List(), List())
      case i: IdxAcc        => toC(i, value, env, i.t.dataType, List(), List())
      case j: JoinAcc       => toC(j, value, env, j.t.dataType, List(), List())
      case s: RecordAcc2    => toC(s, value, env, s.t.dataType, List(), List())
      case s: SplitAcc      => toC(s, value, env, s.t.dataType, List(), List())
      case t: TruncAcc      => toC(t, value, env, t.t.dataType, List(), List())

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def acc(p: Phrase[AccType],
          value: Expr,
          env: Environment,
          dt: DataType,
          arrayAccess: List[Nat],
          tupleAccess: List[Nat]): Expr = {
    p match {
      case Identifier(name, t) =>
        val index: Nat = {
          if (arrayAccess.nonEmpty) {
            computeIndex(t.dataType, arrayAccess, tupleAccess, 0)
          } else {
            null
          }
        }

        val suffix = {
          if (tupleAccess.nonEmpty) {
            tupleAccess.map {
              case lift.arithmetic.Cst(1) => "._1"
              case lift.arithmetic.Cst(2) => "._2"
              case _ => throw new Exception("This should not happen")
            }.foldLeft("")(_ + _)
          } else {
            ""
          }
        }

        val (originalType, currentType) = (t.dataType, dt)
        (originalType, currentType) match {
          case (DPIA.Types.ArrayType(_, st1), VectorType(n, st2))
            if DataType.scalarType(st1) == DataType.scalarType(st2) => ???
          case _ =>
            Assignment(
              ArraySubscript(DeclRef(name + suffix), ArithmeticExpr(index)),
              value)
        }

      case v: ViewAcc => v.toOpenMP(env, value, dt, arrayAccess, tupleAccess)

      case f: RecordAcc1    => toC(f, value, env, dt, arrayAccess, tupleAccess)
      case i: IdxAcc        => toC(i, value, env, dt, arrayAccess, tupleAccess)
      case j: JoinAcc       => toC(j, value, env, dt, arrayAccess, tupleAccess)
      case s: RecordAcc2    => toC(s, value, env, dt, arrayAccess, tupleAccess)
      case s: SplitAcc      => toC(s, value, env, dt, arrayAccess, tupleAccess)
      case t: TruncAcc      => toC(t, value, env, dt, arrayAccess, tupleAccess)
      case s: ScatterAcc    => toC(s, value, env, dt, arrayAccess, tupleAccess)
      case u: UnzipAcc      => toC(u, value, env, dt, arrayAccess, tupleAccess)

      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, env, dt, arrayAccess, tupleAccess)
      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, env, dt, arrayAccess, tupleAccess)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def toString(d: Data): String = {
    d match {
      case i: IntData => i.i.toString
      case b: BoolData => b.b.toString
      case f: FloatData => f.f.toString
      case i: IndexData => i.i.toString
      case v: VectorData => ???
      case _: RecordData => ???
      case _: ArrayData => ???
    }
  }

  def toString(dt: DataType): String = {
    dt match {
      case b: DPIA.Types.BasicType => b match {
        case Types.bool | Types.int => "int"
        case Types.float => "float"
        case _: IndexType => "int"
        case _: VectorType => ???
      }
      case _: DPIA.Types.RecordType => ???
      case _: DPIA.Types.ArrayType => ???
      case _: DPIA.Types.DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }

  def convertBinaryOp(op: idealised.SurfaceLanguage.Operators.Binary.Value): idealised.C.AST.BinaryOperator.Value = {
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

  def convertUnaryOp(op: idealised.SurfaceLanguage.Operators.Unary.Value): idealised.C.AST.UnaryOperator.Value = {
    import idealised.SurfaceLanguage.Operators.Unary._
    op match {
      case NEG => UnaryOperator.-
    }
  }

}
