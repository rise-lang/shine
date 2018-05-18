package idealised.OpenMP

import idealised._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T]): OpenMP.Program = {

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

  private def makeCode(p: Phrase[ExpType], inputParams: Seq[Identifier[ExpType]]): OpenMP.Program = {
    val outParam = createOutputParam(outT = p.t)

    val p1 = checkTypes(p)

    val p2 = rewriteToImperative(p1, outParam)

    val p3 = substituteImplementations(p2)

    val env = (outParam +: inputParams).map(p => p.name -> p.name ).toMap

    val (decls, code) = C.CodeGeneration.CodeGenerator(p3, env, new CodeGeneration.PrimitivesToOpenMP)

    OpenMP.Program(
      decls,
      function = C.ProgramGenerator.makeFunction(
        C.ProgramGenerator.makeParams(outParam, inputParams), code),
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

}
