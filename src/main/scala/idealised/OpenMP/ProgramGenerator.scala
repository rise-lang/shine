package idealised.OpenMP

import idealised._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import lift.arithmetic.Cst

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T], name: String = "foo"): OpenMP.Program = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]]
                                           ): (Phrase[ExpType], Seq[Identifier[ExpType]]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ep: Phrase[ExpType]@unchecked => (ep, ps)
      }
    }

    val (phrase, params) = getPhraseAndParams(originalPhrase, Seq())

    makeCode(phrase, params.reverse, name)
  }

  private def makeCode(p: Phrase[ExpType],
                       inputParams: Seq[Identifier[ExpType]],
                       name: String): OpenMP.Program = {
    val outParam = createOutputParam(outT = p.t)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap, Map.empty)

    val gen = OpenMP.CodeGeneration.CodeGenerator(env)

    val (declarations, code) = gen.generate(p3)

    val params = C.ProgramGenerator.makeParams(outParam, inputParams, gen)

    OpenMP.Program(
      declarations,
      function    = C.ProgramGenerator.makeFunction(params, C.AST.Block(Seq(code)), name),
      outputParam = outParam,
      inputParams = inputParams)
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    outT.dataType match {
      case _: BasicType =>
        identifier("output", AccType(ArrayType(Cst(1), outT.dataType)))
      case _: ArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: DPIA.Types.DepArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: RecordType => ???
      case _: DataTypeIdentifier => ???
    }
  }

  private def checkTypes(p1: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p1)
    TypeCheck(p1)
    p1
  }

  private def rewriteToImperative(p: Phrase[ExpType],
                                  a: Phrase[AccType]): Phrase[CommandType] = {
    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` (Cst(0) asPhrase IndexType(Cst(1)))
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

}
