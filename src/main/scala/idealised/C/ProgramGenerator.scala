package idealised.C

import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{AccType, CommandType, DataType, DataTypeIdentifier, ExpType, PairType, PhraseType, RecordType, TypeChecker}
import idealised.C.AST._
import idealised._
import lift.arithmetic.{NamedVar, Var}

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T]): Program = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]]
                                           ): (Phrase[ExpType], Seq[Identifier[ExpType]]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ep: Phrase[ExpType]@unchecked => (ep, ps)
      }
    }

    val (phrase, params) = getPhraseAndParams(originalPhrase, Seq())

    makeCode(phrase, params.reverse)
  }

  private def makeCode(p: Phrase[ExpType], inputParams: Seq[Identifier[ExpType]]): Program = {
    val outParam = createOutputParam(outT = p.t)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val env = (outParam +: inputParams).map(p => p.name -> p.name ).toMap

    val (decls, code) = C.CodeGeneration.CodeGenerator(p3, env).generate

    C.Program(decls,
      function = makeFunction(makeParams(outParam, inputParams), Block(Seq(code))),
      outputParam = outParam,
      inputParams = inputParams)
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    identifier("output", AccType(outT.dataType))
  }

  private def checkTypes(p1: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p1)
    TypeChecker(p1)
    p1
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

  def makeFunction(params: Seq[ParamDecl], body: Block, name: String = "foo"): FunDecl = {
    FunDecl(name, returnType = Type.void, params, body)
  }

  def makeParams(out: Identifier[AccType],
                 ins: Seq[Identifier[ExpType]]): Seq[ParamDecl] = {
    val sizes = collectSizes(out.`type`.dataType
      +: ins.map(_.`type`.dataType)).toSeq.sortBy(_.toString)
    Seq(makeParam(out)) ++ ins.map(makeParam) ++ sizes.map(makeSizeParam)
  }

  def collectSizes(ts: Seq[DataType]): Set[Var] = {
    import DPIA.Types.{ArrayType, RecordType, BasicType}
    ts.foldLeft(Set[Var]())( (s, t) => {
      s ++ (t match {
        case _: BasicType => Set()
        case ArrayType(size, dt) =>
          size.varList ++ collectSizes(Seq(dt))
        case RecordType(fst, snd) =>
          collectSizes(Seq(fst)) ++ collectSizes(Seq(snd))
        case _: DataTypeIdentifier => ???
      })
    })
  }

  def makeParam(i: Identifier[_]): ParamDecl = {
    import DPIA.Types.{ArrayType, RecordType, BasicType}
    // Turn array types into pointer types

    val paramType = getDataType(i) match {
      case ArrayType(_, dt) =>
        val baseDt = DataType.getBaseDataType(dt)
        PointerType(Type.fromDataType(baseDt))
      case r : RecordType => Type.fromDataType(r)
      case t : BasicType => Type.fromDataType(t)
      case _: DataTypeIdentifier => ???
    }
    ParamDecl(i.name, paramType)
  }

  def makeSizeParam(v: Var): ParamDecl = {
    ParamDecl(v.toString, Type.const_int)
  }

  private def getDataType(i: Identifier[_]): DataType = {
    i.t match {
      case ExpType(dataType) => dataType
      case AccType(dataType) => dataType
      case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => dt1
      case _ => throw new Exception("This should not happen")
    }
  }

}
