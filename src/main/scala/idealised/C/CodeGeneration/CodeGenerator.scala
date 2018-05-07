package idealised.C.CodeGeneration


import idealised.DPIA
import idealised.DPIA._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.C._
import idealised.C.AST._

import scala.collection.immutable

object CodeGenerator {
  type Ranges = immutable.Map[String, lift.arithmetic.Range]

  def apply(p: Phrase[CommandType], primitiveCodeGen: PrimitiveCodeGen): Block = {
    val gen = CodeGenerator(primitiveCodeGen)
    gen.cmd(p, Block(immutable.Seq()), gen)
  }
}

case class CodeGenerator(primitiveCodeGen: PrimitiveCodeGen,
                         ranges: CodeGenerator.Ranges = immutable.Map[String, lift.arithmetic.Range]()) {

  def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    copy(ranges= ranges.updated(key, value))

  def cmd(p: Phrase[CommandType], block: Block, gen: CodeGenerator): Block = {
    p match {
      case DPIA.Phrases.IfThenElse(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), gen)
        val falseBlock = cmd(elseP, Block(), gen)
        block + idealised.C.AST.IfThenElse(exp(condP, gen), trueBlock, Some(falseBlock))

      case c: GeneratableComm => c.codeGen(block, gen)

      case a: Assign => primitiveCodeGen.codeGen(a, block, gen)
      case d: DoubleBufferFor => primitiveCodeGen.codeGen(d, block, gen)
      case f: For => primitiveCodeGen.codeGen(f, block, gen)
      case n: New => primitiveCodeGen.codeGen(n, block, gen)
      case s: idealised.DPIA.ImperativePrimitives.Seq => primitiveCodeGen.codeGen(s, block, gen)
      case s: idealised.DPIA.ImperativePrimitives.Skip => primitiveCodeGen.codeGen(s, block, gen)

      case p: ParFor => primitiveCodeGen.codeGen(p, block, gen)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | Identifier(_, _) |
           Proj1(_) | Proj2(_) | _: CommandPrimitive =>
        throw new Exception(s"Don't know how to generate code for $p with code generator for ${primitiveCodeGen.name}")
    }
  }

  def exp(p: Phrase[ExpType], gen: CodeGenerator): Expr = {
    p match {
      case BinOp(op, lhs, rhs) =>
        BinaryExpr(exp(lhs, gen), convertBinaryOp(op), exp(rhs, gen))
      case Identifier(name, _) => DeclRef(name)
      case DPIA.Phrases.Literal(d) => d match {
        case i: IntData     => idealised.C.AST.Literal(i.i.toString)
        case b: BoolData    => idealised.C.AST.Literal(b.b.toString)
        case f: FloatData   => idealised.C.AST.Literal(f.f.toString)
        case v: VectorData  => idealised.C.AST.Literal(toString(v))
        case r: RecordData  => idealised.C.AST.Literal(toString(r))
        case a: ArrayData   => idealised.C.AST.Literal(toString(a))

        case i: IndexData   => ArithmeticExpr(i.n)
      }
      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, gen)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, gen)
      case UnaryOp(op, x)       => UnaryExpr(convertUnaryOp(op), exp(x, gen))

      case f: Fst       => primitiveCodeGen.codeGen(f, gen, f.t.dataType, List(), List())
      case i: Idx       => primitiveCodeGen.codeGen(i, gen, i.t.dataType, List(), List())
      case r: Record    => primitiveCodeGen.codeGen(r, gen, r.t.dataType, List(), List())
      case s: Snd       => primitiveCodeGen.codeGen(s, gen, s.t.dataType, List(), List())
      case t: TruncExp  => primitiveCodeGen.codeGen(t, gen, t.t.dataType, List(), List())

      case g: GeneratableExp => g.codeGen(gen)

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

  def exp(p: Phrase[ExpType], gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
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

      case l: DPIA.Phrases.Literal => primitiveCodeGen.codeGen(l, gen, dt, arrayAccess, tupleAccess)

      case p: Proj1[ExpType, _] => exp(Lifting.liftPair(p.pair)._1, gen, dt, arrayAccess, tupleAccess)
      case p: Proj2[_, ExpType] => exp(Lifting.liftPair(p.pair)._2, gen, dt, arrayAccess, tupleAccess)

      case v: ViewExp => v.codeGen(gen, dt, arrayAccess, tupleAccess)

      case g: Gather => primitiveCodeGen.codeGen(g, gen, dt, arrayAccess, tupleAccess)
      case j: Join => primitiveCodeGen.codeGen(j, gen, dt, arrayAccess, tupleAccess)
      case s: Split => primitiveCodeGen.codeGen(s, gen, dt, arrayAccess, tupleAccess)
      case z: Zip => primitiveCodeGen.codeGen(z, gen, dt, arrayAccess, tupleAccess)
      case u: Unzip => primitiveCodeGen.codeGen(u, gen, dt, arrayAccess, tupleAccess)

      case f: Fst       => primitiveCodeGen.codeGen(f, gen, dt, arrayAccess, tupleAccess)
      case i: Idx       => primitiveCodeGen.codeGen(i, gen, dt, arrayAccess, tupleAccess)
      case r: Record    => primitiveCodeGen.codeGen(r, gen, dt, arrayAccess, tupleAccess)
      case s: Snd       => primitiveCodeGen.codeGen(s, gen, dt, arrayAccess, tupleAccess)
      case t: TruncExp  => primitiveCodeGen.codeGen(t, gen, dt, arrayAccess, tupleAccess)

      case g: GeneratableExp => g.codeGen(gen)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) |
           BinOp(_, _, _) | UnaryOp(_, _) |
           DPIA.Phrases.IfThenElse(_, _, _) | _: ExpPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }


  def acc(p: Phrase[AccType], value: Expr, gen: CodeGenerator): Expr = {
    p match {
      case Identifier(name, t) =>
        t.dataType match {
          case _: DPIA.Types.BasicType => Assignment(DeclRef(name), value)
          case _ => throw new Exception(s"Don't know how to generate assignment into variable $name of type $t")
        }
      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, gen)
      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, gen)

      case f: RecordAcc1    => primitiveCodeGen.codeGen(f, value, gen, f.t.dataType, List(), List())
      case i: IdxAcc        => primitiveCodeGen.codeGen(i, value, gen, i.t.dataType, List(), List())
      case j: JoinAcc       => primitiveCodeGen.codeGen(j, value, gen, j.t.dataType, List(), List())
      case s: RecordAcc2    => primitiveCodeGen.codeGen(s, value, gen, s.t.dataType, List(), List())
      case s: SplitAcc      => primitiveCodeGen.codeGen(s, value, gen, s.t.dataType, List(), List())
      case t: TruncAcc      => primitiveCodeGen.codeGen(t, value, gen, t.t.dataType, List(), List())

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | DPIA.Phrases.IfThenElse(_, _, _) | _: AccPrimitive =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def acc(p: Phrase[AccType], value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
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

      case v: ViewAcc => v.codeGen(gen, value, dt, arrayAccess, tupleAccess)

      case f: RecordAcc1    => primitiveCodeGen.codeGen(f, value, gen, dt, arrayAccess, tupleAccess)
      case i: IdxAcc        => primitiveCodeGen.codeGen(i, value, gen, dt, arrayAccess, tupleAccess)
      case j: JoinAcc       => primitiveCodeGen.codeGen(j, value, gen, dt, arrayAccess, tupleAccess)
      case s: RecordAcc2    => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
      case s: SplitAcc      => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
      case t: TruncAcc      => primitiveCodeGen.codeGen(t, value, gen, dt, arrayAccess, tupleAccess)
      case s: ScatterAcc    => primitiveCodeGen.codeGen(s, value, gen, dt, arrayAccess, tupleAccess)
      case u: UnzipAcc      => primitiveCodeGen.codeGen(u, value, gen, dt, arrayAccess, tupleAccess)

      case p: Proj1[AccType, _] => acc(Lifting.liftPair(p.pair)._1, value, gen, dt, arrayAccess, tupleAccess)
      case p: Proj2[_, AccType] => acc(Lifting.liftPair(p.pair)._2, value, gen, dt, arrayAccess, tupleAccess)

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
      case i: IndexData => i.n.toString
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
