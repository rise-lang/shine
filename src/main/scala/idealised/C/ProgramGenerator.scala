package idealised.C

import idealised.C.AST._
import idealised.DPIA.Compilation._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{AccType, CommandType, DataType, DataTypeIdentifier, DepArrayType, ExpType, PairType, PhraseType, TypeCheck, int}
import idealised._
import lift.arithmetic.{Cst, Var}

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T]): Program = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]]
                                           ): (Phrase[ExpType], Seq[Identifier[ExpType]]) = {
      p match {
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps)
        case ndl: NatDependentLambda[_] => getPhraseAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int)) +: ps)
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

    val gen = C.CodeGeneration.CodeGenerator()

    checkTypes(p) |> (p =>

    rewriteToImperative(p, outParam) |> ( p => {

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap, Map.empty, Map.empty)

    val (declarations, code) = gen.generate(p, env)

    val typeDeclarations = collectTypeDeclarations(code)

    C.Program(
      typeDeclarations ++ declarations,
      function    = makeFunction(makeParams(outParam, inputParams, gen), Block(Seq(code)), name),
      outputParam = outParam,
      inputParams = inputParams)
    }))
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

  private def checkTypes(p: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p)
    TypeCheck(p)
    p
  }

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommandType] = {
    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (DPIA.Types.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` (Cst(0) asPhrase DPIA.Types.IndexType(Cst(1)))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    TranslationToImperative.acc(p)(output)(
      new idealised.C.TranslationContext) |> (p => {
      xmlPrinter.writeToFile("/tmp/p2.xml", p)
      TypeCheck(p) // TODO: only in debug
      p
    })
  }

  def makeFunction(params: Seq[ParamDecl], body: Block, name: String): FunDecl = {
    FunDecl(name, returnType = Type.void, params, body)
  }

  def makeParams(out: Identifier[AccType],
                 ins: Seq[Identifier[ExpType]],
                 gen: CodeGeneration.CodeGenerator): Seq[ParamDecl] = {
    Seq(makeParam(out, gen)) ++ ins.map(makeParam(_, gen))
  }

  def makeParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): ParamDecl = {
    import DPIA.Types.{ArrayType, BasicType, RecordType}
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

  def collectTypeDeclarations(code: Stmt): Seq[Decl] = {
    val typeDecls = mutable.Set[Decl]()

    code.visitAndRebuild(new Nodes.VisitAndRebuild.Visitor {
      def collect(t: Type): Unit = t match {
        case _: BasicType =>
        case s: StructType =>
          typeDecls += C.AST.StructTypeDecl(
            s.print,
            s.fields.map{ case (ty, name) => VarDecl(name, ty) }
          )
        case at: ArrayType => collect(at.elemType)
        case pt: PointerType => collect(pt.valueType)
        case ut: UnionType => ut.fields.foreach(collect)
      }

      override def apply(t: Type): Type = { collect(t) ; t }
    })

    typeDecls.toSeq
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
