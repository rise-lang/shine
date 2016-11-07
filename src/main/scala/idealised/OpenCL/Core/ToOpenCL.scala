package idealised.OpenCL.Core

import apart.arithmetic.{ArithExpr, Cst, Var}
import idealised.Compiling._
import idealised.Core.OperationalSemantics._
import idealised.Core._
import idealised.DSL.typed._
import idealised.HighLevelCombinators._
import idealised.LowLevelCombinators._
import idealised.OpenCL.Core.CombinatorsToOpenCL._
import idealised.OpenCL.Core.HoistMemoryAllocations.AllocationInfo
import idealised._
import ir.{Type, UndefType}
import opencl.executor._
import opencl.generator.OpenCLAST._
import opencl.ir.{Double, Float, GlobalMemory, Int, LocalMemory}

import scala.collection._
import scala.collection.immutable.List
import scala.collection.Seq
import scala.language.implicitConversions

case class ToOpenCL(localSize: Nat, globalSize: Nat) {

  def makeKernel[T <: PhraseType](originalPhrase: Phrase[T]): OpenCL.Kernel = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: List[IdentPhrase[ExpType]]
                                           ): (Phrase[ExpType], List[IdentPhrase[ExpType]]) = {
      p match {
        case l: LambdaPhrase[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ep: Phrase[ExpType]@unchecked => (ep, ps)
      }
    }

    val (phrase, params) = getPhraseAndParams(originalPhrase, List())
    makeKernel(phrase, params.reverse)
  }

  def apply(p: Phrase[ExpType -> ExpType],
            param: IdentPhrase[ExpType]): OpenCL.Kernel =
    makeKernel(p(param), List(param))

  def apply(p: Phrase[ExpType -> (ExpType -> ExpType)],
            param0: IdentPhrase[ExpType],
            param1: IdentPhrase[ExpType]): OpenCL.Kernel =
    makeKernel(p(param0)(param1), List(param0, param1))

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> ExpType))],
            param0: IdentPhrase[ExpType],
            param1: IdentPhrase[ExpType],
            param2: IdentPhrase[ExpType]): OpenCL.Kernel =
    makeKernel(p(param0)(param1)(param2), List(param0, param1, param2))

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType)))],
            param0: IdentPhrase[ExpType],
            param1: IdentPhrase[ExpType],
            param2: IdentPhrase[ExpType],
            param3: IdentPhrase[ExpType]): OpenCL.Kernel =
    makeKernel(p(param0)(param1)(param2)(param3), List(param0, param1, param2, param3))

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))],
            param0: IdentPhrase[ExpType],
            param1: IdentPhrase[ExpType],
            param2: IdentPhrase[ExpType],
            param3: IdentPhrase[ExpType],
            param4: IdentPhrase[ExpType]): OpenCL.Kernel =
    makeKernel(p(param0)(param1)(param2)(param3)(param4),
      List(param0, param1, param2, param3, param4))

  private def makeKernel(p: Phrase[ExpType], params: List[IdentPhrase[ExpType]]): OpenCL.Kernel = {
    val p1 = inferTypes(p)

    val outParam = createOutputParam(outT = p1.t)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val (p4, intermediateAllocations) = hoistMemoryAllocations(p3)

    OpenCL.Kernel(
      function = makeKernelFunction(makeParams(outParam, params, intermediateAllocations),
                                    makeBody(p4)),
      outputParam = outParam,
      inputParams = params,
      intermediateParams = intermediateAllocations.map(_._2)
    )
  }

  private def inferTypes(p: Phrase[ExpType]): Phrase[ExpType] = {
    val p1 = TypeInference(p)
    xmlPrinter.toFile("/tmp/p1.xml", p1)
    TypeChecker(p1)
    p1
  }

  private def createOutputParam(outT: ExpType): IdentPhrase[AccType] = {
    identifier("output", AccType(outT.dataType))
  }

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommandType] = {
    val p2 = RewriteToImperative.acc(p)(a)
    xmlPrinter.toFile("/tmp/p2.xml", p2)
    TypeChecker(p2)
    p2
  }

  private def substituteImplementations(p: Phrase[CommandType]): Phrase[CommandType] = {
    import SubstituteImplementations.Environment
    val p3 = SubstituteImplementations(p, Environment(immutable.Map(("output", OpenCL.GlobalMemory))))
    xmlPrinter.toFile("/tmp/p3.xml", p3)
    TypeChecker(p3)
    p3
  }

  private def hoistMemoryAllocations(p: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {
    val (p4, intermediateAllocations) = HoistMemoryAllocations(p)
    xmlPrinter.toFile("/tmp/p4.xml", p4)
    TypeChecker(p4)
    (p4, intermediateAllocations)
  }

  private def makeParams(out: IdentPhrase[AccType],
                         ins: List[IdentPhrase[ExpType]],
                         intermediateAllocations: List[AllocationInfo]): List[ParamDecl] = {
    List(makeGlobalParam(out)) ++ // first the output parameter ...
      ins.map(makeInputParam) ++ // ... then the input parameters ...
      intermediateAllocations.map(makeParam) ++ // ... then the intermediate buffers ...
      // ... finally, the parameters for the length information in the type
      // these can only come from the input parameters.
      makeLengthParams(ins.map(_.t.dataType).map(DataType.toType))
  }

  // pass arrays via global and scalar + tuple values via private memory
  private def makeInputParam(i: IdentPhrase[_]): ParamDecl = {
    getDataType(i) match {
      case a: ArrayType => makeGlobalParam(i)
      case b: BasicType => makePrivateParam(i)
      case r: RecordType => makePrivateParam(i)
      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }

  private def makeGlobalParam(i: IdentPhrase[_]): ParamDecl = {
    ParamDecl(
      i.name,
      DataType.toType(getDataType(i)),
      opencl.ir.GlobalMemory,
      const = false)
  }

  private def makePrivateParam(i: IdentPhrase[_]): ParamDecl = {
    ParamDecl(
      i.name,
      DataType.toType(getDataType(i)),
      opencl.ir.PrivateMemory,
      const = false)
  }

  private def makeParam(allocInfo: AllocationInfo): ParamDecl = {
    val (addressSpace, identifier) = allocInfo
    ParamDecl(
      identifier.name,
      DataType.toType(getDataType(identifier)),
      OpenCL.AddressSpace.toOpenCL(addressSpace),
      const = false
    )
  }

  // returns list of int parameters for each variable in the given types;
  // sorted by name of the variables
  private def makeLengthParams(types: List[Type]): List[ParamDecl] = {
    val lengths = types.flatMap(Type.getLengths)
    lengths.filter(_.isInstanceOf[apart.arithmetic.Var]).distinct.map(v =>
      ParamDecl(v.toString, opencl.ir.Int)
    ).sortBy(_.name)
  }

  private def makeBody(p: Phrase[CommandType]): Block = {
    ToOpenCL.cmd(p, Block(), ToOpenCL.Environment(localSize, globalSize))
  }

  private def makeKernelFunction(params: List[ParamDecl], body: Block): Function = {
    Function(name = "KERNEL",
      ret = UndefType, // should really be void
      params = params,
      body = body,
      kernel = true)
  }

  implicit private def getDataType(i: IdentPhrase[_]): DataType = {
    i.t match {
      case ExpType(dataType) => dataType
      case AccType(dataType) => dataType
      case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => dt1
      case _ => throw new Exception("This should not happen")
    }
  }

  object asFunction {
    def apply[F <: FunctionHelper](kernel: OpenCL.Kernel)
                                  (implicit ev: F#T <:< HList): (F#T) => (F#R, TimeSpan[Time.ms]) = {
      (args: F#T) => {
        val lengthMapping = createLengthMap(kernel.inputParams, args)

        val (outputArg, inputArgs) = createKernelArgs(kernel, args, lengthMapping)
        val kernelArgs = (outputArg +: inputArgs).toArray

        val runtime = Executor.execute(kernel.code,
          ArithExpr.substitute(localSize, lengthMapping).eval, 1, 1,
          ArithExpr.substitute(globalSize, lengthMapping).eval, 1, 1,
          kernelArgs)

        val output = castToOutputType[F#R](kernel.outputParam.`type`.dataType, outputArg)

        kernelArgs.foreach(_.dispose)

        (output, TimeSpan.inMilliseconds(runtime))
      }
    }
  }

  private def createKernelArgs(kernel: OpenCL.Kernel,
                               args: HList,
                               lengthMapping: immutable.Map[Core.Nat, Core.Nat]) = {
    val numberOfKernelArgs = 1 + args.length + kernel.intermediateParams.size + lengthMapping.size
    assert(kernel.function.params.length == numberOfKernelArgs)

    println("Allocations on the host: ")
    val outputArg = createOutputArg(DataType.sizeInByte(kernel.outputParam) `with` lengthMapping)
    val inputArgs = createInputArgs(args.toList)
    val intermediateArgs = createIntermediateArgs(kernel, args.length, lengthMapping)
    val lengthArgs = createLengthArgs(lengthMapping)

    (outputArg, inputArgs ++ intermediateArgs ++ lengthArgs)
  }

  private def castToOutputType[R](dt: DataType, output: GlobalArg): R = {
    assert(dt.isInstanceOf[ArrayType])
    (Type.getBaseType(DataType.toType(dt)) match {
      case Float  => output.asFloatArray()
      case Int    => output.asIntArray()
      case Double => output.asDoubleArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
        "not supported: " + dt.toString)
    }).asInstanceOf[R]
  }

  private def createLengthArgs(lengthMapping: immutable.Map[Core.Nat, Core.Nat]) = {
    lengthMapping.map( p => {
      println("length (private): 4 bytes")
      ValueArg.create(p._2.eval)
    } ).toList
  }

  private def createIntermediateArgs(kernel: OpenCL.Kernel,
                                     argsLength: Int,
                                     lengthMapping: immutable.Map[Core.Nat, Core.Nat]) = {
    val intermediateParamDecls = getIntermediateParamDecls(kernel, argsLength)
    (intermediateParamDecls, kernel.intermediateParams).zipped.map { case (pDecl, param) =>
      val size = (DataType.sizeInByte(param) `with` lengthMapping).value.max.eval
      pDecl.addressSpace match {
        case LocalMemory =>
          println(s"intermediate (local): $size bytes")
          LocalArg.create(size)
        case GlobalMemory =>
          println(s"intermediate (global): $size bytes")
          GlobalArg.createOutput(size)
      }
    }
  }

  private def getIntermediateParamDecls(kernel: OpenCL.Kernel, argsLength: Int): List[ParamDecl] = {
    val startIndex = 1 + argsLength
    kernel.function.params.slice(startIndex, startIndex + kernel.intermediateParams.size)
  }

  private def createOutputArg(size: SizeInByte) = {
    println(s"output (global): $size")
    GlobalArg.createOutput(size.value.eval)
  }

  private def createInputArgs(args: List[Any]) = {
    args.map(createInputArg)
  }

  private def createInputArg(arg: Any): KernelArg = {
    arg match {
      case  f: Float => createInputArg(f)
      case af: Array[Float] => createInputArg(af)
      case af: Array[Array[Float]] => createInputArg(af.flatten)
      case af: Array[Array[Array[Float]]] => createInputArg(af.flatten.flatten)
      case af: Array[Array[Array[Array[Float]]]] => createInputArg(af.flatten.flatten.flatten)

      case  i: Int => createInputArg(i)
      case ai: Array[Int] => createInputArg(ai)
      case ai: Array[Array[Int]] => createInputArg(ai.flatten)
      case ai: Array[Array[Array[Int]]] => createInputArg(ai.flatten.flatten)
      case ai: Array[Array[Array[Array[Int]]]] => createInputArg(ai.flatten.flatten.flatten)

      case  d: Double => createInputArg(d)
      case ad: Array[Double] => createInputArg(ad)
      case ad: Array[Array[Double]] => createInputArg(ad.flatten)
      case ad: Array[Array[Array[Double]]] => createInputArg(ad.flatten.flatten)
      case ad: Array[Array[Array[Array[Double]]]] => createInputArg(ad.flatten.flatten.flatten)

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.toString)
    }
  }

  private def createInputArg(x: Float) = {
    println(s"value (private): 4 bytes")
    ValueArg.create(x)
  }

  private def createInputArg(a: Array[Float]) = {
    println(s"input (global): ${SizeInByte(a.length * 4)}")
    GlobalArg.createInput(a)
  }

  private def createInputArg(x: Int) = {
    println(s"value (private): 4 bytes")
    ValueArg.create(x)
  }

  private def createInputArg(a: Array[Int]) = {
    println(s"input (global): ${SizeInByte(a.length * 4)}")
    GlobalArg.createInput(a)
  }

  private def createInputArg(x: Double) = {
    println(s"value (private): 4 bytes")
    ValueArg.create(x)
  }

  private def createInputArg(a: Array[Double]) = {
    println(s"input (global): ${SizeInByte(a.length * 4)}")
    GlobalArg.createInput(a)
  }

  private implicit class SubstitutionsHelper(size: SizeInByte) {
    def `with`(valueMap: immutable.Map[Core.Nat, Core.Nat]): SizeInByte = {
      SizeInByte(ArithExpr.substitute(size.value, valueMap))
    }
  }

  private def createLengthMap(params: Seq[IdentPhrase[ExpType]],
                              args: HList): immutable.Map[Core.Nat, Core.Nat] = {
    val seq = (params, args.toList).zipped.flatMap(createLengthMapping)
    seq.map(x => (x._1, Cst(x._2))).toMap
  }

  private def createLengthMapping(p: IdentPhrase[ExpType], a: Any): Seq[(Core.Nat, Int)] = {
    createLengthMapping(p.t.dataType, a).filter(_._1.isInstanceOf[Var])
  }

  private def createLengthMapping(t: DataType, a: Any): Seq[(Core.Nat, Int)] = {
    (t, a) match {
      case (bt: BasicType, _) => (bt, a) match {
        case (bool, _: Boolean) => Seq()
        case (int, _: Int) => Seq()
        case (float, _: Float) => Seq()
        case (VectorType(_, ebt), _) => createLengthMapping(ebt, a)
        case _ => throw new Exception(s"Expected $bt, but got $a")
      }
      case (at: ArrayType, array: Array[_]) =>
        Seq((at.size, array.length)) ++ createLengthMapping(at.elemType, array.head)
      case (rt: RecordType, tuple: (_, _)) =>
        createLengthMapping(rt.fst, tuple._1) ++
          createLengthMapping(rt.snd, tuple._2)
      case _ => throw new Exception(s"Expected $t but got $a")
    }
  }

}

object ToOpenCL {

  case class Environment(localSize: Nat,
                         globalSize: Nat,
                         ranges: mutable.Map[String, apart.arithmetic.Range])

  object Environment {
    def apply(localSize: Nat, globalSize: Nat): Environment = {
      Environment(localSize, globalSize,
        mutable.Map[String, apart.arithmetic.Range]())
    }
  }

  def cmd(p: Phrase[CommandType], block: Block, env: Environment): Block = {
    p match {
      case IfThenElsePhrase(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), env)
        val falseBlock = cmd(elseP, Block(), env)
        (block: Block) += IfThenElse(exp(condP, env), trueBlock, falseBlock)

      case c: GeneratableComm => c.toOpenCL(block, env)

      case a: Assign => toOpenCL(a, block, env)
      case d: DoubleBufferFor => toOpenCL(d, block, env)
      case f: For => toOpenCL(f, block, env)
      case n: New => toOpenCL(n, block, env)
      case s: idealised.LowLevelCombinators.Seq => toOpenCL(s, block, env)
      case s: idealised.LowLevelCombinators.Skip => toOpenCL(s, block, env)

      case p: ParFor => toOpenCL(p, block, env)

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) | IdentPhrase(_, _) |
           Proj1Phrase(_) | Proj2Phrase(_) |
           _: MidLevelCombinator | _: LowLevelCommCombinator =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def exp(p: Phrase[ExpType], env: Environment): Expression = {
    p match {
      case BinOpPhrase(op, lhs, rhs) =>
        BinaryExpression(op.toString, exp(lhs, env), exp(rhs, env))
      case IdentPhrase(name, _) => VarRef(name)
      case LiteralPhrase(d, _) => d match {
          case i: IntData     => Literal(i.i.toString)
          case b: BoolData    => Literal(b.b.toString)
          case f: FloatData   => Literal(f.f.toString)
          case i: IndexData   => Literal(i.i.toString)
          case v: VectorData  => Literal(Data.toString(v))
          case r: RecordData  => Literal(Data.toString(r))
          case a: ArrayData   => Literal(Data.toString(a))
        }
      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, env)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, env)
      case UnaryOpPhrase(op, x) =>
        UnaryExpression(op.toString, exp(x, env))

      case f: Fst       => toOpenCL(f, env, f.t.dataType, List(), List())
      case i: Idx       => toOpenCL(i, env, i.t.dataType, List(), List())
      case r: Record    => toOpenCL(r, env, r.t.dataType, List(), List())
      case s: Snd       => toOpenCL(s, env, s.t.dataType, List(), List())
      case t: TruncExp  => toOpenCL(t, env, t.t.dataType, List(), List())

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) |
           _: HighLevelCombinator | _: LowLevelExpCombinator =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

  def exp(p: Phrase[ExpType],
          env: Environment,
          dt: DataType,
          arrayAccess: List[(Nat, Nat)],
          tupleAccess: List[Nat]): Expression = {
    p match {
      case IdentPhrase(name, t) =>
        val index = {
          val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: Nat)((x, y) => x + y)
          if (i != (0: Nat)) { i } else { null }
        }

        val suffix = {
          val s = tupleAccess.map {
            case apart.arithmetic.Cst(1) => "._1"
            case apart.arithmetic.Cst(2) => "._2"
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

      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, env, dt, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, env, dt, arrayAccess, tupleAccess)

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

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) |
           BinOpPhrase(_, _, _) | UnaryOpPhrase(_, _) |
           IfThenElsePhrase(_, _, _) | LiteralPhrase(_, _) |
           _: LowLevelExpCombinator | _: HighLevelCombinator =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }


  def acc(p: Phrase[AccType], value: Expression, env: Environment): Expression = {
    p match {
      case IdentPhrase(name, t) =>
        t.dataType match {
          case b: BasicType => AssignmentExpression(VarRef(name), value)
          case _ => throw new Exception(s"Don't know how to generate assignment into variable $name of type $t")
        }
      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, value, env)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, value, env)

      case f: FstAcc    => toOpenCL(f, value, env, f.t.dataType, List(), List())
      case i: IdxAcc    => toOpenCL(i, value, env, i.t.dataType, List(), List())
      case j: JoinAcc   => toOpenCL(j, value, env, j.t.dataType, List(), List())
      case r: RecordAcc => toOpenCL(r, value, env, r.t.dataType, List(), List())
      case s: SndAcc    => toOpenCL(s, value, env, s.t.dataType, List(), List())
      case s: SplitAcc  => toOpenCL(s, value, env, s.t.dataType, List(), List())
      case t: TruncAcc  => toOpenCL(t, value, env, t.t.dataType, List(), List())

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) |
           _: LowLevelAccCombinator =>
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
      case IdentPhrase(name, t) =>
        val index = {
          val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: Nat)((x, y) => x + y)
          if (i != (0: Nat)) { i } else { null }
        }

        val suffix = {
          val s = tupleAccess.map {
            case apart.arithmetic.Cst(1) => "._1"
            case apart.arithmetic.Cst(2) => "._2"
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

      case f: FstAcc    => toOpenCL(f, value, env, dt, arrayAccess, tupleAccess)
      case i: IdxAcc    => toOpenCL(i, value, env, dt, arrayAccess, tupleAccess)
      case j: JoinAcc   => toOpenCL(j, value, env, dt, arrayAccess, tupleAccess)
      case r: RecordAcc => toOpenCL(r, value, env, dt, arrayAccess, tupleAccess)
      case s: SndAcc    => toOpenCL(s, value, env, dt, arrayAccess, tupleAccess)
      case s: SplitAcc  => toOpenCL(s, value, env, dt, arrayAccess, tupleAccess)
      case t: TruncAcc  => toOpenCL(t, value, env, dt, arrayAccess, tupleAccess)

      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, value, env, dt, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, value, env, dt, arrayAccess, tupleAccess)

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) |
           _: LowLevelAccCombinator =>
        throw new Exception(s"Don't know how to generate idealised.OpenCL code for $p")
    }
  }

}
