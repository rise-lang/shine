package shine.OpenCL

import arithexpr.arithmetic.Cst
import shine.C.AST.IncludeSource
import shine.C.primitives.imperative.CFunctionDefinition
import shine.C.{Module, ParamMetaData}
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional
import shine.OpenCL.compilation.HostManagedBuffers
import shine.{C, DPIA}
import util.compiler.DSL.run

import scala.collection.immutable

final case class HostFunctionDefinition(name: String,
                                        definition: Phrase[_ <: PhraseType]) {
  private val cFunDef = CFunctionDefinition(name, definition)
  val body: Phrase[ExpType] = cFunDef.body
  val params: Seq[Identifier[ExpType]] = cFunDef.params
  val topLevelLetNats: Seq[(LetNatIdentifier, Phrase[ExpType])] = cFunDef.topLevelLetNats

  val returnType: ExpType = body.t

  val paramTypes: Seq[ExpType] = params.map(_.t)

  type CodeGenerator = HostCodeGenerator

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
    case _: ArrayType | _: DepArrayType | _: ManagedBufferType =>
      identifier("output", AccType(outT.dataType))
    case _: PairType => throw new Exception("Pairs as output parameters currently not supported")
    case _: DepPairType => identifier("output", AccType(outT.dataType))
    case _: DataTypeIdentifier | _: NatToDataApply | ContextType => throw new Exception("This should not happen")
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
        HostManagedBuffers.populate(params, a.asInstanceOf[Identifier[AccType]]) andThen
        run(TypeCheck(_)) andThen
        UnrollLoops.unroll andThen
        SimplifyNats.simplify )
  }

  private def optionallyManagedParams(outParam: Identifier[AccType]): Seq[Identifier[_ <: BasePhraseType]] =
    (outParam +: params).map(p => HostManagedBuffers.optionallyManaged(p)
      .map(_._1.asInstanceOf[Identifier[_ <: BasePhraseType]]).getOrElse(p))

  private def generateCode(gen: CodeGenerator)
                          (outParam: Identifier[AccType]): Phrase[CommType] => (immutable.Seq[gen.Decl], gen.Stmt) = {
    val env = shine.DPIA.Compilation.CodeGenerator.Environment(
      optionallyManagedParams(outParam).map(p => p -> C.AST.DeclRef(p.name)).toMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    gen.generate(topLevelLetNats, env)
  }

  private def makeModule(gen: CodeGenerator)
                         (outParam: Identifier[AccType]): ((immutable.Seq[gen.Decl], gen.Stmt)) => Module = {
    case (declarations, code) =>
      val params = C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context")) +:
        optionallyManagedParams(outParam).map(C.AST.makeParam(gen))
      Module(
        includes = immutable.Seq(IncludeSource("runtime.h")),
        decls = CFunctionDefinition.collectTypeDeclarations(code, params) ++ declarations,
        functions = immutable.Seq(
          C.Function(
            code = C.AST.FunDecl(name, returnType = C.AST.Type.void, params, C.AST.Block(immutable.Seq(code))),
            paramMetaData =
              ParamMetaData(ContextType, C.ParamMetaData.Kind.input) +:
              ParamMetaData(outParam.`type`.dataType, C.ParamMetaData.Kind.output) +:
                this.params.map(p => ParamMetaData(p.`type`.dataType, C.ParamMetaData.Kind.input))
          )
        )
      )
  }
}