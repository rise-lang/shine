package idealised.OpenCL.CodeGeneration

import idealised.DPIA
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.{ArrayData, BoolData, Data, FloatData, IndexData, IntData, RecordData, VectorData}
import idealised.DPIA.{Lifting, Nat, Types}
import idealised.DPIA.Types.{AccType, ArrayType, BasicType, CommandType, DataType, DataTypeIdentifier, ExpType, IndexType, RecordType, VectorType}
import idealised.OpenCL.CodeGeneration.PrimitivesCodeGenerator.toOpenCL
import idealised.OpenCL.{GeneratableComm, GeneratableExp, ViewAcc, ViewExp}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST.{ArithExpression, AssignmentExpression, BinaryExpression, Block, Expression, UnaryExpression, VLoad, VStore, VarRef}

import scala.collection.immutable

object CodeGenerator {
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  case class Environment(localSize: Nat,
                         globalSize: Nat,
                         ranges: Ranges = immutable.Map[String, lift.arithmetic.Range]()) {
    def updatedRanges(key: String, value: lift.arithmetic.Range): Environment =
      copy(ranges = ranges.updated(key, value))
  }

  def cmd(p: Phrase[CommandType], block: Block, env: Environment): Block = {
    p match {
      case DPIA.Phrases.IfThenElse(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), env)
        val falseBlock = cmd(elseP, Block(), env)
        (block: Block) += OpenCLAST.IfThenElse(exp(condP, env), trueBlock, falseBlock)

      case c: GeneratableComm => c.codeGenCmd(block, env)

      case a: Assign => toOpenCL(a, block, env)
      case d: DoubleBufferFor => toOpenCL(d, block, env)
      case f: For => toOpenCL(f, block, env)
      case n: New => toOpenCL(n, block, env)
      case s: idealised.DPIA.ImperativePrimitives.Seq => toOpenCL(s, block, env)
      case s: idealised.DPIA.ImperativePrimitives.Skip => toOpenCL(s, block, env)

      case p: ParFor => toOpenCL(p, block, env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | Identifier(_, _) |
           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def exp(p: Phrase[ExpType], env: Environment): Expression = {
    p match {
      case BinOp(op, lhs, rhs) =>
        BinaryExpression(op.toString, exp(lhs, env), exp(rhs, env))
      case Identifier(name, _) => VarRef(name)
      case DPIA.Phrases.Literal(d, _) => d match {
        case i: IntData     => OpenCLAST.Literal(i.i.toString)
        case b: BoolData    => OpenCLAST.Literal(b.b.toString)
        case f: FloatData   => OpenCLAST.Literal(f.f.toString)
        case v: VectorData  => OpenCLAST.Literal(toString(v))
        case r: RecordData  => OpenCLAST.Literal(toString(r))
        case a: ArrayData   => OpenCLAST.Literal(toString(a))

        case i: IndexData   => OpenCLAST.ArithExpression(i.i)
      }
      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, env)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, env)
      case UnaryOp(op, x) =>
        UnaryExpression(op.toString, exp(x, env))

      case f: Fst       => toOpenCL(f, env, f.t.dataType, List(), List())
      case i: Idx       => toOpenCL(i, env, i.t.dataType, List(), List())
      case r: Record    => toOpenCL(r, env, r.t.dataType, List(), List())
      case s: Snd       => toOpenCL(s, env, s.t.dataType, List(), List())
      case t: TruncExp  => toOpenCL(t, env, t.t.dataType, List(), List())

      case g: GeneratableExp => g.codeGenExp(env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def computeIndex(dt: DataType, arrayIndices: List[Nat], tupleIndices: List[Nat], idx: Nat): Nat = {
    dt match {
      case _: BasicType => idx
      case ArrayType(n, et) =>
        val i :: is = arrayIndices
        computeIndex(et, is, tupleIndices, (idx * n) + i)
      case RecordType(lhs, rhs) => ???
      case _: DataTypeIdentifier => ???
    }
  }

  def exp(p: Phrase[ExpType],
          env: Environment,
          dt: DataType,
          arrayAccess: List[Nat],
          tupleAccess: List[Nat]): Expression = {
    p match {
      case Identifier(name, t) =>

        val index: Nat = {
          if (arrayAccess.nonEmpty) {
//            println(s"computeIndex: ${t.dataType}, ${arrayAccess}")
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
            null
          }
        }

        val (originalType, currentType) = (t.dataType, dt)
        (originalType, currentType) match {
          case (ArrayType(_, st1), VectorType(n, st2))
            if DataType.scalarType(st1) == DataType.scalarType(st2) =>
            VLoad(
              VarRef(name),
              DataType.toVectorType(VectorType(n, st2)),
              ArithExpression(index /^ n))
          case _ =>
            VarRef(name, suffix, ArithExpression(index))
        }

      case l: DPIA.Phrases.Literal => toOpenCL(l, env, dt, arrayAccess, tupleAccess)

      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, env, dt, arrayAccess, tupleAccess)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, env, dt, arrayAccess, tupleAccess)

      case v: ViewExp => v.toOpenCL(env, arrayAccess, tupleAccess, dt)

      case g: Gather => toOpenCL(g, env, arrayAccess, tupleAccess, dt)
      case j: Join => toOpenCL(j, env, arrayAccess, tupleAccess, dt)
      case s: Split => toOpenCL(s, env, arrayAccess, tupleAccess, dt)
      case z: Zip => toOpenCL(z, env, arrayAccess, tupleAccess, dt)

      case f: Fst       => toOpenCL(f, env, dt, arrayAccess, tupleAccess)
      case i: Idx       => toOpenCL(i, env, dt, arrayAccess, tupleAccess)
      case r: Record    => toOpenCL(r, env, dt, arrayAccess, tupleAccess)
      case s: Snd       => toOpenCL(s, env, dt, arrayAccess, tupleAccess)
      case t: TruncExp  => toOpenCL(t, env, dt, arrayAccess, tupleAccess)

      case g: GeneratableExp => g.codeGenExp(env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) |
           BinOp(_, _, _) | UnaryOp(_, _) |
           DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }


  def acc(p: Phrase[AccType], value: Expression, env: Environment): Expression = {
    p match {
      case Identifier(name, t) =>
        t.dataType match {
          case _: BasicType => AssignmentExpression(VarRef(name), value)
          case _ => throw new Exception(s"Don't know how to generate assignment into variable $name of type $t")
        }
      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, env)
      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, env)

      case f: RecordAcc1    => toOpenCL(f, value, env, f.t.dataType, List(), List())
      case i: IdxAcc    => toOpenCL(i, value, env, i.t.dataType, List(), List())
      case j: JoinAcc   => toOpenCL(j, value, env, j.t.dataType, List(), List())
      case s: RecordAcc2    => toOpenCL(s, value, env, s.t.dataType, List(), List())
      case s: SplitAcc  => toOpenCL(s, value, env, s.t.dataType, List(), List())
      case t: TruncAcc  => toOpenCL(t, value, env, t.t.dataType, List(), List())

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def acc(p: Phrase[AccType],
          value: Expression,
          env: Environment,
          dt: DataType,
          arrayAccess: List[Nat],
          tupleAccess: List[Nat]): Expression = {
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
            null
          }
        }

        val (originalType, currentType) = (t.dataType, dt)
        (originalType, currentType) match {
          case (ArrayType(_, st1), VectorType(n, st2))
            if DataType.scalarType(st1) == DataType.scalarType(st2) =>
            VStore(
              VarRef(name),
              DataType.toVectorType(VectorType(n, st2)),
              value,
              ArithExpression(index /^ n))
          case _ =>
            AssignmentExpression(
              VarRef(name, suffix, ArithExpression(index)),
              value)
        }

      case v: ViewAcc => v.toOpenCL(env, value, dt, arrayAccess, tupleAccess)

      case f: RecordAcc1    => toOpenCL(f, value, env, dt, arrayAccess, tupleAccess)
      case i: IdxAcc    => toOpenCL(i, value, env, dt, arrayAccess, tupleAccess)
      case j: JoinAcc   => toOpenCL(j, value, env, dt, arrayAccess, tupleAccess)
      case s: RecordAcc2    => toOpenCL(s, value, env, dt, arrayAccess, tupleAccess)
      case s: SplitAcc  => toOpenCL(s, value, env, dt, arrayAccess, tupleAccess)
      case t: TruncAcc  => toOpenCL(t, value, env, dt, arrayAccess, tupleAccess)
      case s: ScatterAcc => toOpenCL(s, value, env, dt, arrayAccess, tupleAccess)

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
      //          case i: Int4Data => Literal(s"(int4)(${i.i0.toString}, ${i.i1.toString}, ${i.i2.toString}, ${i.i3.toString})")
      case v: VectorData => v.a.length match {
        case 2 | 3 | 4 | 8 | 16 =>
          val dt = toString(v.a.head.dataType)
          val n = v.a.length
          s"($dt$n)(" + v.a.map(x => toString(x)).reduce(_ + ", " + _) + ")"
      }
      case _: RecordData => ???
      case _: ArrayData => ???
    }
  }

  def toString(dt: DataType): String = {
    dt match {
      case b: BasicType => b match {
        case Types.bool | Types.int => "int"
        case Types.float => "float"
        case _: IndexType => "int"
        case v: VectorType => toString(v.elemType) + v.size.toString
      }
      case _: RecordType => ???
      case _: ArrayType => ???
      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }
}
