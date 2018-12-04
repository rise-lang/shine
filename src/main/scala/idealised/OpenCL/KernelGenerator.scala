package idealised.OpenCL

import java.io.{File, PrintWriter}

import idealised._
import idealised.DPIA._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.OpenCL.CodeGeneration.{AdaptKernelBody, AdaptKernelParameters, OpenCLOldCodeGenerator, HoistMemoryAllocations}
import idealised.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo
import opencl.generator.OpenCLAST._
import opencl.generator.OpenCLPrinter

import scala.collection._
import scala.language.implicitConversions

object KernelGenerator {

  def makeKernel[T <: PhraseType](originalPhrase: Phrase[T],
                                  localSize: Nat,
                                  globalSize: Nat): OpenCL.Kernel = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]]
                                           ): (Phrase[ExpType], Seq[Identifier[ExpType]]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ep: Phrase[ExpType]@unchecked => (ep, ps)
      }
    }

    val (phrase, params) = getPhraseAndParams(originalPhrase, Seq())

    makeKernel(phrase, params.reverse, localSize, globalSize)
  }

  private def makeKernel(p: Phrase[ExpType], inputParams: Seq[Identifier[ExpType]],
                         localSize: Nat, globalSize: Nat): OpenCL.Kernel = {
    val outParam = createOutputParam(outT = p.t)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val (p4, intermediateAllocations) = hoistMemoryAllocations(p3)

    val (p5, kernelParams) = adaptKernelParameters(p4,
      makeParams(outParam, inputParams, intermediateAllocations), inputParams)

    val kernelBody = adaptKernelBody(makeBody(p5, localSize, globalSize))

    OpenCL.Kernel(
      function = makeKernelFunction(kernelParams, kernelBody),
      outputParam = outParam,
      inputParams = inputParams,
      intermediateParams = intermediateAllocations.map(_.identifier),
      localSize, globalSize)
  }

  private def checkTypes(p1: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p1)
    TypeCheck(p1)
    p1
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    identifier("output", AccType(outT.dataType))
  }

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommandType] = {
    val p2 = RewriteToImperative.acc(p)(a)
    xmlPrinter.writeToFile("/tmp/p2.xml", p2)
    TypeCheck(p2) // TODO: only in debug
    p2
  }

  private def substituteImplementations(p: Phrase[CommandType]): Phrase[CommandType] = {
    val p3 = SubstituteImplementations(p,
      SubstituteImplementations.Environment(immutable.Map(("output", OpenCL.GlobalMemory))))
    xmlPrinter.writeToFile("/tmp/p3.xml", p3)
    TypeCheck(p3) // TODO: only in debug
    p3
  }

  private def hoistMemoryAllocations(p: Phrase[CommandType]): (Phrase[CommandType], List[AllocationInfo]) = {
    val (p4, intermediateAllocations) = HoistMemoryAllocations(p)
    xmlPrinter.writeToFile("/tmp/p4.xml", p4)
    TypeCheck(p4) // TODO: only in debug
    (p4, intermediateAllocations)
  }

  private def makeParams(out: Identifier[AccType],
                         ins: Seq[Identifier[ExpType]],
                         intermediateAllocations: Seq[AllocationInfo]): Seq[ParamDecl] = {
    Seq(makeGlobalParam(out)) ++ // first the output parameter ...
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
      const = false)
  }

  // returns list of int parameters for each variable in the given types;
  // sorted by name of the variables
  private def makeLengthParams(types: Seq[ir.Type]): Seq[ParamDecl] = {
    val lengths = types.flatMap(ir.Type.getLengths)
    lengths.filter(_.isInstanceOf[lift.arithmetic.Var]).distinct.map(v =>
      ParamDecl(v.toString, opencl.ir.Int) ).sortBy(_.name)
  }

  private def adaptKernelParameters(p: Phrase[CommandType],
                                    params: Seq[ParamDecl],
                                    inputParams: Seq[Identifier[ExpType]]
                                   ): (Phrase[CommandType], Seq[ParamDecl]) = {
    val (p5, newParams) = AdaptKernelParameters(p, params, inputParams)
    xmlPrinter.writeToFile("/tmp/p5.xml", p5)
    TypeCheck(p5) // TODO: only in debug
    (p5, newParams)
  }

  private def makeBody(p: Phrase[CommandType], localSize: Nat, globalSize: Nat): Block = {
    OpenCLOldCodeGenerator.cmd(p, Block(), OpenCLOldCodeGenerator.Environment(localSize, globalSize))
  }

  private def adaptKernelBody(body: Block): Block = {
    val pw = new PrintWriter(new File("/tmp/p6.cl"))
    try pw.write(OpenCLPrinter()(body)) finally pw.close()
    AdaptKernelBody(body)
  }

  private def makeKernelFunction(params: Seq[ParamDecl], body: Block): Function = {
    Function(name = "KERNEL",
      ret = ir.UndefType, // should really be void
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

}
