package idealised.OpenCL

import idealised.DPIA.Compilation._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo
import idealised.OpenCL.CodeGeneration.PrimitivesCodeGenerator._
import idealised.OpenCL.CodeGeneration.HoistMemoryAllocations
import idealised.{DPIA, _}
import ir.{Type, UndefType}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._

import scala.collection._
import scala.collection.immutable.List
import scala.language.implicitConversions

object CodeGenerator {

  def makeKernel[T <: PhraseType](originalPhrase: Phrase[T],
                                  localSize: Nat, globalSize: Nat): OpenCL.Kernel = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: List[Identifier[ExpType]]
                                           ): (Phrase[ExpType], List[Identifier[ExpType]]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ep: Phrase[ExpType]@unchecked => (ep, ps)
      }
    }

    val (phrase, params) = getPhraseAndParams(originalPhrase, List())
    makeKernel(phrase, params.reverse, localSize, globalSize)
  }

  private def makeKernel(p: Phrase[ExpType], params: List[Identifier[ExpType]],
                         localSize: Nat, globalSize: Nat): OpenCL.Kernel = {
    val p1 = checkTypes(p)

    val outParam = createOutputParam(outT = p1.t)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val (p4, intermediateAllocations) = hoistMemoryAllocations(p3)

    OpenCL.Kernel(
      function = makeKernelFunction(makeParams(outParam, params, intermediateAllocations),
                                    makeBody(p4, localSize, globalSize)),
      outputParam = outParam,
      inputParams = params,
      intermediateParams = intermediateAllocations.map(_.identifier),
      localSize, globalSize
    )
  }

  private def checkTypes(p1: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p1)
    TypeChecker(p1)
    p1
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    identifier("output", AccType(outT.dataType))
  }

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommandType] = {
    val p2 = RewriteToImperative.acc(p)(a)
    xmlPrinter.writeToFile("/tmp/p2.xml", p2)
    TypeChecker(p2) // TODO: only in debug
    p2
  }

  private def substituteImplementations(p: Phrase[CommandType]): Phrase[CommandType] = {
    val p3 = SubstituteImplementations(p,
      SubstituteImplementations.Environment(immutable.Map(("output", OpenCL.GlobalMemory))))
    xmlPrinter.writeToFile("/tmp/p3.xml", p3)
    TypeChecker(p3) // TODO: only in debug
    p3
  }

  private def hoistMemoryAllocations(p: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {
    val (p4, intermediateAllocations) = HoistMemoryAllocations(p)
    xmlPrinter.writeToFile("/tmp/p4.xml", p4)
    TypeChecker(p4) // TODO: only in debug
    (p4, intermediateAllocations)
  }

  private def makeParams(out: Identifier[AccType],
                         ins: List[Identifier[ExpType]],
                         intermediateAllocations: List[AllocationInfo]): List[ParamDecl] = {
    List(makeGlobalParam(out)) ++ // first the output parameter ...
      ins.map(makeInputParam) ++ // ... then the input parameters ...
      intermediateAllocations.map(makeParam) ++ // ... then the intermediate buffers ...
      // ... finally, the parameters for the length information in the type
      // these can only come from the input parameters.
      makeLengthParams(ins.map(_.t.dataType).map(DataType.toType))
  }

  // pass arrays via global and scalar + tuple values via private memory
  private def makeInputParam(i: Identifier[_]): ParamDecl = {
    getDataType(i) match {
      case _: ArrayType => makeGlobalParam(i)
      case _: BasicType => makePrivateParam(i)
      case _: RecordType => makePrivateParam(i)
      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }

  private def makeGlobalParam(i: Identifier[_]): ParamDecl = {
    ParamDecl(
      i.name,
      DataType.toType(getDataType(i)),
      opencl.ir.GlobalMemory,
      const = false)
  }

  private def makePrivateParam(i: Identifier[_]): ParamDecl = {
    ParamDecl(
      i.name,
      DataType.toType(getDataType(i)),
      opencl.ir.PrivateMemory,
      const = false)
  }

  private def makeParam(allocInfo: AllocationInfo): ParamDecl = {
    ParamDecl(
      allocInfo.identifier.name,
      DataType.toType(getDataType(allocInfo.identifier)),
      OpenCL.AddressSpace.toOpenCL(allocInfo.addressSpace),
      const = false
    )
  }

  // returns list of int parameters for each variable in the given types;
  // sorted by name of the variables
  private def makeLengthParams(types: List[Type]): List[ParamDecl] = {
    val lengths = types.flatMap(Type.getLengths)
    lengths.filter(_.isInstanceOf[lift.arithmetic.Var]).distinct.map(v =>
      ParamDecl(v.toString, opencl.ir.Int)
    ).sortBy(_.name)
  }

  private def makeBody(p: Phrase[CommandType], localSize: Nat, globalSize: Nat): Block = {
    CodeGenerator.cmd(p, Block(), Environment(localSize, globalSize))
  }

  private def makeKernelFunction(params: List[ParamDecl], body: Block): Function = {
    Function(name = "KERNEL",
      ret = UndefType, // should really be void
      params = params,
      body = body,
      kernel = true)
  }

  implicit private def getDataType(i: Identifier[_]): DataType = {
    i.t match {
      case ExpType(dataType) => dataType
      case AccType(dataType) => dataType
      case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => dt1
      case _ => throw new Exception("This should not happen")
    }
  }

  case class Environment(localSize: Nat,
                         globalSize: Nat,
                         ranges: mutable.Map[String, lift.arithmetic.Range] = mutable.Map[String, lift.arithmetic.Range]())

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
          case i: IndexData   => OpenCLAST.Literal(i.i.toString)
          case v: VectorData  => OpenCLAST.Literal(toString(v))
          case r: RecordData  => OpenCLAST.Literal(toString(r))
          case a: ArrayData   => OpenCLAST.Literal(toString(a))
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

  def exp(p: Phrase[ExpType],
          env: Environment,
          dt: DataType,
          arrayAccess: List[(Nat, Nat)],
          tupleAccess: List[Nat]): Expression = {
    p match {
      case Identifier(name, t) =>
        val index = {
          val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: Nat)((x, y) => x + y)
          if (i != (0: Nat)) { i } else { null }
        }

        val suffix = {
          val s = tupleAccess.map {
            case lift.arithmetic.Cst(1) => "._1"
            case lift.arithmetic.Cst(2) => "._2"
            case _ => throw new Exception("This should not happen")
          }.foldLeft("")(_ + _)

          if (s != "") { s } else { null }
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
           DPIA.Phrases.IfThenElse(_, _, _) | DPIA.Phrases.Literal(_, _) | _: ExpPrimitive =>
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
          arrayAccess: List[(Nat, Nat)],
          tupleAccess: List[Nat]): Expression = {
    p match {
      case Identifier(name, t) =>
        val index = {
          val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: Nat)((x, y) => x + y)
          if (i != (0: Nat)) { i } else { null }
        }

        val suffix = {
          val s = tupleAccess.map {
            case lift.arithmetic.Cst(1) => "._1"
            case lift.arithmetic.Cst(2) => "._2"
            case _ => throw new Exception("This should not happen")
          }.foldLeft("")(_ + _)
          if (s != "") { s } else { null }
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
