package shine.C.primitives.imperative

import arithexpr.arithmetic.Cst
import shine.C.AST.IncludeHeader
import shine.C.{Module, ParamMetaData}
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional
import shine.{C, DPIA}
import util.compiler.DSL.run

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.xml.Elem

final case class CFunctionDefinition(name: String,
                                     definition: Phrase[_ <: PhraseType]) extends CommandPrimitive {

  val ( body: Phrase[ExpType],
        params: Seq[Identifier[ExpType]],
        topLevelLetNats: Seq[(LetNatIdentifier, Phrase[ExpType])]
      ) = splitBodyAndParams(definition, Seq(), Seq())

  val returnType: ExpType = body.t

  val paramTypes: Seq[ExpType] = params.map(_.t)

  @tailrec
  private def splitBodyAndParams(p: Phrase[_],
                         ps: Seq[Identifier[ExpType]],
                         defs: Seq[(LetNatIdentifier, Phrase[ExpType])])
    : (Phrase[ExpType], Seq[Identifier[ExpType]], Seq[(LetNatIdentifier, Phrase[ExpType])]) =
    p match {
      case Apply(f, a) => splitBodyAndParams(Lifting.liftFunction(f).reducing(a), ps, defs)
      case DepApply(f, a) => splitBodyAndParams(Lifting.liftDependentFunction(f)(a), ps, defs)
      case l: Lambda[ExpType, _]@unchecked => splitBodyAndParams(l.body, l.param +: ps, defs)
      case ndl: DepLambda[_, _] => splitBodyAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
      case ln:LetNat[ExpType, _]@unchecked => splitBodyAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
      case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
    }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] =
    CFunctionDefinition(name, VisitAndRebuild(definition, f))

  override def prettyPrint: String = s"$returnType $name(${params.map(p => s"${p.t} ${p.name}")}) {\n  $definition\n}"

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Store = ???

  override def xmlPrinter: Elem = ???

  type CodeGenerator = shine.C.CodeGenerator

  def translateToModule(gen: CodeGenerator): Module = {
    val outParam = createOutputParam(outT = body.t)

    body |>
      ( run(TypeCheck(_: Phrase[ExpType])) andThen
        rewriteToImperative(gen)(outParam) andThen
        generateCode(gen)(outParam) andThen
        makeModule(gen)(outParam) )
  }

  def createOutputParam(outT: ExpType): Identifier[AccType] = outT.dataType match {
    case _: BasicType =>
      identifier("output", AccType(ArrayType(Cst(1), outT.dataType)))
    case _: ArrayType | _: DepArrayType =>
      identifier("output", AccType(outT.dataType))
    case _: PairType => throw new Exception("Pairs as output parameters currently not supported")
    case _: DepPairType => identifier("output", AccType(outT.dataType))
    case _: DataTypeIdentifier | _: NatToDataApply | ContextType | _: ManagedBufferType =>
      throw new Exception(s"unexpected output data type: ${outT.dataType}")
  }

  private def rewriteToImperative(gen: CodeGenerator)
                                 (a: Phrase[AccType])(p: Phrase[ExpType]): Phrase[CommType] = {
    implicit val context: shine.DPIA.Compilation.TranslationContext = gen.translationContext

    val output = (a.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => a
      case (DPIA.Types.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        a `@` functional.NatAsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    output |>
      ( TranslationToImperative.acc(p) _ andThen
        run(TypeCheck(_)) andThen
        UnrollLoops.unroll andThen
        SimplifyNats.simplify )
  }

  private def generateCode(gen: CodeGenerator)
                          (outParam: Identifier[AccType]): Phrase[CommType] => (immutable.Seq[gen.Decl], gen.Stmt) = {
    val env = shine.DPIA.Compilation.CodeGenerator.Environment(
      (outParam +: params).map(p => p -> C.AST.DeclRef(p.name)).toMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    gen.generate(topLevelLetNats, env)
  }

  private def makeModule(gen: CodeGenerator)
                         (outParam: Identifier[AccType]): ((immutable.Seq[gen.Decl], gen.Stmt)) => Module = {
    case (declarations, code) =>
      val params = (outParam +: this.params).map(C.AST.makeParam(gen))
      Module(
        includes = immutable.Seq(IncludeHeader("stdint.h")),
        decls = CFunctionDefinition.collectTypeDeclarations(code, params) ++ declarations,
        functions = immutable.Seq(
          C.Function(
            code = C.AST.FunDecl(name, returnType = C.AST.Type.void, params, C.AST.Block(immutable.Seq(code))),
            paramMetaData =
              ParamMetaData(outParam.`type`.dataType, C.ParamMetaData.Kind.output) +:
                this.params.map(p => ParamMetaData(p.`type`.dataType, C.ParamMetaData.Kind.input))
          )
        )
      )
  }
}

object CFunctionDefinition {
  def fromPhrase(name: String)(p: Phrase[_ <: PhraseType]): CFunctionDefinition =
    CFunctionDefinition(name, p)

  def collectTypeDeclarations(code: C.AST.Stmt, params: immutable.Seq[C.AST.ParamDecl]
                                     ): immutable.Seq[C.AST.Decl] = {
    def visitor(decls: mutable.ArrayBuffer[C.AST.Decl]): C.AST.Nodes.VisitAndRebuild.Visitor = {
      new C.AST.Nodes.VisitAndRebuild.Visitor {
        def collect(t: C.AST.Type): Unit = t match {
          case _: C.AST.BasicType | _: C.AST.OpaqueType =>
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
    }

    val allocTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    code.visitAndRebuild(visitor(allocTypeDecls))

    val paramTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    params.foreach(_.visitAndRebuild(visitor(paramTypeDecls)))

    (allocTypeDecls ++ paramTypeDecls).distinct.toSeq
  }
}
