package idealised.C

import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{AccType, BasePhraseTypes, CommandType, DataType, DataTypeIdentifier, DepArrayType, ExpType, NatDependentFunctionType, PairType, PhraseType, RecordType, TypeCheck}
import idealised.C.AST._
import idealised._
import lift.arithmetic.{Cst, NamedVar, Var}

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

  private def makeCode(p: Phrase[ExpType],
                       inputParams: Seq[Identifier[ExpType]],
                       name: String = "foo"): Program = {
    val outParam = createOutputParam(outT = p.t)

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap, Map.empty)

    val gen = C.CodeGeneration.CodeGenerator(env)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val (declarations, code) = gen.generate(p3)

    C.Program(
      declarations,
      function    = makeFunction(makeParams(outParam, inputParams, gen), Block(Seq(code)), name),
      outputParam = outParam,
      inputParams = inputParams)
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    outT.dataType match {
      case _: DPIA.Types.BasicType =>
        identifier("output", AccType(DPIA.Types.ArrayType(Cst(1), outT.dataType)))
      case _: DPIA.Types.ArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: DPIA.Types.DepArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: DPIA.Types.RecordType => ???
      case _: DPIA.Types.DataTypeIdentifier => ???
    }
  }

  private def checkTypes(p1: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p1)
    TypeCheck(p1)
    p1
  }

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommandType] = {
    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (DPIA.Types.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` (Cst(0) asPhrase DPIA.Types.IndexType(Cst(1)))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    val p2 = RewriteToImperative.acc(p)(output)
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

  def makeFunction(params: Seq[ParamDecl], body: Block, name: String): FunDecl = {
    FunDecl(name, returnType = Type.void, params, body)
  }

  def makeParams(out: Identifier[AccType],
                 ins: Seq[Identifier[ExpType]],
                 gen: CodeGeneration.CodeGenerator): Seq[ParamDecl] = {
    val sizes = collectSizes(out.`type`.dataType
      +: ins.map(_.`type`.dataType)).toSeq.sortBy(_.toString)
    Seq(makeParam(out, gen)) ++ ins.map(makeParam(_, gen)) ++ sizes.map(makeSizeParam)
  }

  def collectSizes(ts: Seq[DataType]): Set[Var] = {
    import DPIA.Types.{ArrayType, RecordType, BasicType}
    ts.foldLeft(Set[Var]())( (s, t) => {
      s ++ (t match {
        case _: BasicType => Set()
        case ArrayType(size, dt) =>
          size.varList ++ collectSizes(Seq(dt))
        case DepArrayType(size, _, et) =>
          size.varList ++ collectSizes(Seq(et))
        case RecordType(fst, snd) =>
          collectSizes(Seq(fst)) ++ collectSizes(Seq(snd))
        case _: DataTypeIdentifier => ???
      })
    })
  }

  def makeParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): ParamDecl = {
    import DPIA.Types.{ArrayType, RecordType, BasicType}
    // Turn array types into pointer types

    val paramType = getDataType(i) match {
      case ArrayType(_, dt) =>
        val baseDt = DataType.getBaseDataType(dt)
        PointerType(gen.typ(baseDt))
      case DepArrayType(_, _, dt) =>
        val baseDt = DataType.getBaseDataType(dt)
        PointerType(gen.typ(baseDt))
      case r : RecordType => gen.typ(r)
      case t : BasicType => gen.typ(t)
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
