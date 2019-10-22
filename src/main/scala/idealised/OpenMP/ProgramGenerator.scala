package idealised.OpenMP

import idealised._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.AsIndex
import idealised.DPIA.LetNatIdentifier
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import lift.arithmetic.Cst

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T], name: String = "foo"): OpenMP.Program = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]],
                                            defs:Seq[(LetNatIdentifier, Phrase[ExpType])]
                                           ): (Phrase[ExpType], Seq[Identifier[ExpType]], Seq[(LetNatIdentifier, Phrase[ExpType])]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps, defs)
        case ndl: DepLambda[_, _] => getPhraseAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
        case ln:LetNat[ExpType, _]@unchecked => // LetNat(binder, defn:Phrase[ExpType], body) =>
          getPhraseAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
        case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
      }
    }

    val (phrase, params, topLevelLetNats) = getPhraseAndParams(originalPhrase, Seq(), Seq())

    makeCode(phrase, params, topLevelLetNats, name)
  }

  private def makeCode(p: Phrase[ExpType],
                       inputParams: Seq[Identifier[ExpType]],
                       topLevelLetNats:Seq[(LetNatIdentifier, Phrase[ExpType])],
                       name: String): OpenMP.Program = {
    val outParam = createOutputParam(outT = p.t)

    val gen = OpenMP.CodeGeneration.CodeGenerator()

    checkTypes(p) |> (p =>

    rewriteToImperative(p, outParam) |> (p => {

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap, Map.empty, Map.empty, Map.empty)

    val (declarations, code) = gen.generate(p, topLevelLetNats, env)

    val params = C.ProgramGenerator.makeParams(outParam, inputParams, gen)

    val typeDeclarations = C.ProgramGenerator.collectTypeDeclarations(code, params)

    OpenMP.Program(
      typeDeclarations ++ declarations,
      function    = C.ProgramGenerator.makeFunction(params, C.AST.Block(Seq(code)), name),
      outputParam = outParam,
      inputParams = inputParams)
    }))
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    outT.dataType match {
      case _: BasicType =>
        identifier("output", AccType(ArrayType(Cst(1), outT.dataType)))
      case _: ArrayType | _: DepArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: PairType => throw new Exception("Pairs as output parameters currently not supported")
      case _: DataTypeIdentifier | _: NatToDataApply => throw new Exception("This should not happen")
    }
  }

  private def checkTypes(p: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p)
    TypeCheck(p)
    p
  }

  private def rewriteToImperative(p: Phrase[ExpType],
                                  a: Phrase[AccType]): Phrase[CommType] = {
    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` AsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    SimplifyNats(UnrollLoops(TranslationToImperative.acc(p)(output)(
      new idealised.OpenMP.TranslationContext) |> (p => {
      xmlPrinter.writeToFile("/tmp/p2.xml", p)
      TypeCheck(p) // TODO: only in debug
      p
    })))
  }
}
