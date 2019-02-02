package idealised.OpenCL

import java.io.{File, PrintWriter}

import idealised.C.AST.DeclRef
import idealised._
import idealised.DPIA._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.OpenCL.CodeGeneration.{AdaptKernelBody, AdaptKernelParameters, HoistMemoryAllocations}
import idealised.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo

import scala.collection._
import scala.language.implicitConversions

object KernelGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T],
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

    val gen = OpenCL.CodeGeneration.CodeGenerator(localSize, globalSize)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val (p4, intermediateAllocations) = hoistMemoryAllocations(p3)

    val (p5, kernelParams) = adaptKernelParameters(p4,
      makeParams(outParam, inputParams, intermediateAllocations, gen), inputParams)

    val identMap: Predef.Map[Identifier[_ <: BasePhraseTypes], DeclRef] =
      (outParam +: inputParams).map( p => p -> C.AST.DeclRef(p.name) ).toMap

    val intermediateIdentMap: Predef.Map[Identifier[_ <: BasePhraseTypes], DeclRef] =
        intermediateAllocations.flatMap( p =>
          Seq(Identifier(s"${p.identifier.name}_1", p.identifier.`type`.t1) -> C.AST.DeclRef(p.identifier.name),
              Identifier(s"${p.identifier.name}_2", p.identifier.`type`.t2) -> C.AST.DeclRef(p.identifier.name) ) ).toMap

    val env = C.CodeGeneration.CodeGenerator.Environment(identMap ++ intermediateIdentMap, Map.empty)

    val (declarations, code) = gen.generate(p5, env)

    OpenCL.Kernel(
      kernel = makeKernelFunction(kernelParams, adaptKernelBody(C.AST.Block(Seq(code)))),
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
                         intermediateAllocations: Seq[AllocationInfo],
                         gen: CodeGeneration.CodeGenerator): Seq[OpenCL.AST.ParamDecl] = {
    Seq(makeGlobalParam(out, gen)) ++ // first the output parameter ...
      ins.map(makeInputParam(_, gen)) ++ // ... then the input parameters ...
      intermediateAllocations.map(makeParam(_, gen)) ++ // ... then the intermediate buffers ...
      // ... finally, the parameters for the length information in the type
      // these can only come from the input parameters.
      makeLengthParams(ins.map(_.t.dataType))
  }

  // pass arrays via global and scalar + tuple values via private memory
  private def makeInputParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): OpenCL.AST.ParamDecl = {
    getDataType(i) match {
      case _: ArrayType => makeGlobalParam(i, gen)
      case _: DepArrayType => makeGlobalParam(i, gen)
      case _: BasicType => makePrivateParam(i, gen)
      case _: RecordType => makePrivateParam(i, gen)
      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }

  private def makeGlobalParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): OpenCL.AST.ParamDecl = {
    OpenCL.AST.ParamDecl(
      i.name,
      gen.typ(getDataType(i)),
      OpenCL.GlobalMemory)
  }

  private def makePrivateParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): OpenCL.AST.ParamDecl = {
    OpenCL.AST.ParamDecl(
      i.name,
      gen.typ(getDataType(i)),
      OpenCL.PrivateMemory)
  }

  private def makeParam(allocInfo: AllocationInfo, gen: CodeGeneration.CodeGenerator): OpenCL.AST.ParamDecl = {
    OpenCL.AST.ParamDecl(
      allocInfo.identifier.name,
      gen.typ(getDataType(allocInfo.identifier)),
      allocInfo.addressSpace)
  }

  // returns list of int parameters for each variable in the given types;
  // sorted by name of the variables
  private def makeLengthParams(types: Seq[DataType]): Seq[OpenCL.AST.ParamDecl] = {
    val lengths: Seq[Nat] = types.flatMap(DataType.getSizes)
    lengths.filter(_.isInstanceOf[lift.arithmetic.Var]).distinct.map(v =>
      OpenCL.AST.ParamDecl(v.toString, C.AST.Type.int, OpenCL.PrivateMemory) ).sortBy(_.name)
  }

  private def adaptKernelParameters(p: Phrase[CommandType],
                                    params: Seq[OpenCL.AST.ParamDecl],
                                    inputParams: Seq[Identifier[ExpType]]
                                   ): (Phrase[CommandType], Seq[OpenCL.AST.ParamDecl]) = {
    val (p5, newParams) = AdaptKernelParameters(p, params, inputParams)
    xmlPrinter.writeToFile("/tmp/p5.xml", p5)
    TypeCheck(p5) // TODO: only in debug
    (p5, newParams)
  }

  private def adaptKernelBody(body: C.AST.Block): C.AST.Block = {
    val pw = new PrintWriter(new File("/tmp/p6.cl"))
    try pw.write(idealised.C.AST.Printer(body)) finally pw.close()
    AdaptKernelBody(body)
  }

  private def makeKernelFunction(params: Seq[OpenCL.AST.ParamDecl], body: C.AST.Block): OpenCL.AST.KernelDecl = {
    OpenCL.AST.KernelDecl(name = "KERNEL",
      params = params,
      body = body,
      attribute = None)
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
