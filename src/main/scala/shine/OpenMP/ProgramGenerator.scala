package shine.OpenMP

import arithexpr.arithmetic.Cst
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.NatAsIndex
import shine.DPIA.LetNatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine._

import scala.annotation.tailrec
import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T], name: String = "foo"): OpenMP.Program = {

    @tailrec
    /*
     * Precondition: Expression is fully beta-reduced and NOT eta-reduced.
     * TODO: think about enabling fully eta-reduced expressions, i.e. possibly
     *  something like App(mapSeq, f)
     */
    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: immutable.Seq[Identifier[ExpType]],
                                            defs: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])]
                                           ): (Phrase[ExpType], immutable.Seq[Identifier[ExpType]], immutable.Seq[(LetNatIdentifier, Phrase[ExpType])]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps, defs)
        case ndl: DepLambda[_, _] => getPhraseAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
        case ln:LetNat[ExpType, _]@unchecked => // LetNat(binder, defn:Phrase[ExpType], body) =>
          getPhraseAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
        case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
      }
    }

    val (phrase, params, topLevelLetNats) = getPhraseAndParams(originalPhrase, immutable.Seq(), immutable.Seq())

    makeCode(phrase, params, topLevelLetNats, name)
  }

  private def makeCode(p: Phrase[ExpType],
                       inputParams: immutable.Seq[Identifier[ExpType]],
                       topLevelLetNats: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])],
                       name: String): OpenMP.Program = {
    val outParam = createOutputParam(outT = p.t)

    val gen = OpenMP.CodeGeneration.CodeGenerator()

    checkTypes(p) |> (p =>

    rewriteToImperative(p, outParam) |> (p => {

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    val (declarations, code) = gen.generate(p, topLevelLetNats, env)

    val params = C.ProgramGenerator.makeParams(outParam, inputParams, gen)

    val typeDeclarations = C.ProgramGenerator.collectTypeDeclarations(code, params)

    OpenMP.Program(
      typeDeclarations ++ declarations,
      function    = C.ProgramGenerator.makeFunction(params, C.AST.Block(immutable.Seq(code)), name),
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
      case _:DepPairType[_] => throw new Exception("Dependent pairs as output parameters are currently not supported")
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
        a `@` NatAsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    SimplifyNats(UnrollLoops(TranslationToImperative.acc(p)(output)(
      new shine.OpenMP.TranslationContext) |> (p => {
      xmlPrinter.writeToFile("/tmp/p2.xml", p)
      TypeCheck(p) // TODO: only in debug
      p
    })))
  }
}
