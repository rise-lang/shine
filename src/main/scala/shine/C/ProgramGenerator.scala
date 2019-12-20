package shine.C

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.{LetNatIdentifier, Lifting}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine._
import arithexpr.arithmetic.{Cst, Var}

import scala.collection._

object ProgramGenerator {

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T],
                                name: String = "foo"): Program = {

    def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                            ps: Seq[Identifier[ExpType]],
                                            defs:Seq[(LetNatIdentifier, Phrase[ExpType])])
      : (Phrase[ExpType], Seq[Identifier[ExpType]], Seq[(LetNatIdentifier, Phrase[ExpType])]) = p match {
        case Apply(f, a) => getPhraseAndParams(Lifting.liftFunction(f).reducing(a), ps, defs)
        case DepApply(f, a) => getPhraseAndParams(Lifting.liftDependentFunction(f)(a), ps, defs)
        case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps, defs)
        case ndl: DepLambda[_, _] =>
          getPhraseAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
        case ln:LetNat[ExpType, _]@unchecked => getPhraseAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
        case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
      }

    val (phrase, params, topLevelLetNats) = getPhraseAndParams(originalPhrase, Seq(), Seq())

    makeCode(phrase, params, topLevelLetNats, name)
  }

  private def makeCode(p: Phrase[ExpType],
                       inputParams: Seq[Identifier[ExpType]],
                       topLevelLetNats:Seq[(LetNatIdentifier, Phrase[ExpType])],
                       name: String): Program = {
    val outParam = createOutputParam(outT = p.t)

    val gen = C.CodeGeneration.CodeGenerator()

    checkTypes(p) |> (p =>

    rewriteToImperative(p, outParam) |> ( p => {

    val env = C.CodeGeneration.CodeGenerator.Environment(
      (outParam +: inputParams).map(p => p -> C.AST.DeclRef(p.name) ).toMap, Map.empty, Map.empty, Map.empty)

    val (declarations, code) = gen.generate(p, topLevelLetNats, env)

    val params = makeParams(outParam, inputParams, gen)

    val typeDeclarations = collectTypeDeclarations(code, params)

    C.Program(
      typeDeclarations ++ declarations,
      function    = makeFunction(params, C.AST.Block(Seq(code)), name),
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

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType]): Phrase[CommType] = {
    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (DPIA.Types.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` AsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    SimplifyNats(UnrollLoops(TranslationToImperative.acc(p)(output)(
      new shine.C.TranslationContext) |> (p => {
      xmlPrinter.writeToFile("/tmp/p2.xml", p)
      TypeCheck(p) // TODO: only in debug
      p
    })))
  }

  def makeFunction(params: Seq[C.AST.ParamDecl], body: C.AST.Block, name: String): C.AST.FunDecl = {
    C.AST.FunDecl(name, returnType = C.AST.Type.void, params, body)
  }

  def makeParams(out: Identifier[AccType],
                 ins: Seq[Identifier[ExpType]],
                 gen: CodeGeneration.CodeGenerator): Seq[C.AST.ParamDecl] = {
    Seq(makeParam(out, gen)) ++ ins.map(makeParam(_, gen))
  }

  def makeParam(i: Identifier[_], gen: CodeGeneration.CodeGenerator): C.AST.ParamDecl = {
    // Turn array types into pointer types

    val paramType = getDataType(i) match {
      case ArrayType(_, dt) =>
        val baseDt = DataType.getBaseDataType(dt)
        C.AST.PointerType(gen.typ(baseDt))
      case DepArrayType(_, NatToDataLambda(_, dt)) =>
        val baseDt = DataType.getBaseDataType(dt)
        C.AST.PointerType(gen.typ(baseDt))
      case r : PairType => gen.typ(r)
      case t : BasicType => gen.typ(t)
      case _: DataTypeIdentifier | _: NatToDataApply | DepArrayType(_, NatToDataIdentifier(_)) =>
        throw new Exception("This should not happen")
    }
    C.AST.ParamDecl(i.name, paramType)
  }

  def makeSizeParam(v: Var): C.AST.ParamDecl = {
    C.AST.ParamDecl(v.toString, C.AST.Type.const_int)
  }

  def collectTypeDeclarations(code: C.AST.Stmt, params: Seq[C.AST.ParamDecl]): Seq[C.AST.Decl] = {
    def visitor(decls: mutable.ArrayBuffer[C.AST.Decl]): C.AST.Nodes.VisitAndRebuild.Visitor = new C.AST.Nodes.VisitAndRebuild.Visitor {
      def collect(t: C.AST.Type): Unit = t match {
        case _: C.AST.BasicType =>
        case s: C.AST.StructType =>
          s.fields.foreach { case (ty, _) => collect(ty) }
          decls += C.AST.StructTypeDecl(
            s.print,
            s.fields.map { case (ty, name) => C.AST.VarDecl(name, ty) }
          )
        case at: C.AST.ArrayType => collect(at.elemType)
        case pt: C.AST.PointerType => collect(pt.valueType)
        case ut: C.AST.UnionType => ut.fields.foreach(collect)
      }

      override def apply(t: C.AST.Type): C.AST.Type = { collect(t) ; t }
    }

    val allocTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    code.visitAndRebuild(visitor(allocTypeDecls))

    val paramTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    params.foreach(_.visitAndRebuild(visitor(paramTypeDecls)))

    (allocTypeDecls ++ paramTypeDecls).distinct
  }

  private def getDataType(i: Identifier[_]): DataType = i.t match {
    case ExpType(dataType, _) => dataType
    case AccType(dataType) => dataType
    case PhrasePairType(ExpType(dt1, _), AccType(dt2)) if dt1 == dt2 => dt1
    case _ => throw new Exception("This should not happen")
  }

}
