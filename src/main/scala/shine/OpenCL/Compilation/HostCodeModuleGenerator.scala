package shine.OpenCL.Compilation

import shine.{C, DPIA}
import shine.C.AST.{Decl, IncludeSource, ParamKind, Stmt}
import shine.C.Compilation.{ModuleGenerator => CModuleGenerator}
import shine.DPIA.Compilation.{FunDef, ModuleGenerator}
import shine.DPIA.DSL.identifier
import shine.DPIA.Phrases._
import shine.DPIA.Types._

import scala.collection.immutable

object HostCodeModuleGenerator extends ModuleGenerator[FunDef] {
  override type Module = C.Module
  override type CodeGenerator = HostCodeGenerator

  override def createOutputParam(outT: ExpType): Identifier[AccType] =
    outT.dataType match {
      case _: ManagedBufferType => identifier("output", AccType(outT.dataType))
      case _ => CModuleGenerator.createOutputParam(outT)
    }

  override def rewriteToImperative(gen: HostCodeGenerator,
                                   funDef: FunDef,
                                   outParam: Identifier[AccType]
                                  ): Phrase[ExpType] => Phrase[CommType] =
    CModuleGenerator.rewriteToImperative(gen, funDef, outParam)


  override def makeModule(gen: HostCodeGenerator,
                          funDef: FunDef,
                          outParam: Identifier[AccType]
                         ): Phrase[CommType] => Module =
    imperativePasses(funDef, outParam) andThen
      generateCode(gen, funDef, outParam) andThen
      makeHostCodeModule(gen, funDef, outParam)

  def imperativePasses(funDef: FunDef,
                       outParam: Identifier[AccType]
                      ): Phrase[CommType] => Phrase[CommType] =
    HostManagedBuffers.insert(funDef.params, outParam) andThen
      CModuleGenerator.imperativePasses

  def generateCode(gen: HostCodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[CommType] => (Seq[Decl], Stmt) = {
    val env = shine.DPIA.Compilation.CodeGenerator.Environment(
      optionallyManagedParams(funDef.params, outParam)
        .map(p => p -> C.AST.DeclRef(p.name)).toMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    gen.generate(funDef.topLevelLetNats, env)
  }

  def makeHostCodeModule(gen: HostCodeGenerator,
                         funDef: FunDef,
                         outParam: Identifier[AccType]
                        ): ((Seq[Decl], Stmt)) => Module = {
    case (declarations, code) =>
      val params = C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context")) +:
        optionallyManagedParams(funDef.params, outParam).map(makeParam(gen))
      C.Module(
        includes = immutable.Seq(IncludeSource("runtime.h")),
        decls = CModuleGenerator.collectTypeDeclarations(code, params) ++
          declarations,
        functions = immutable.Seq(
          C.AST.Function(
            code = C.AST.FunDecl(funDef.name,
              returnType = C.AST.Type.void,
              params,
              body = C.AST.Block(immutable.Seq(code))),
            paramKinds =
              ParamKind.input(ContextType) +:
                ParamKind.output(outParam) +: funDef.params.map(ParamKind.input)
          )
        )
      )
  }

  private def optionallyManagedParams(params: Seq[Identifier[ExpType]],
                                      outParam: Identifier[AccType]
                                     ): Seq[Identifier[_ <: BasePhraseType]] =
    (outParam +: params).map(p => HostManagedBuffers.optionallyManaged(p)
      .map(_._1.asInstanceOf[Identifier[_ <: BasePhraseType]]).getOrElse(p)
    )

  private def makeParam(gen: CodeGenerator): Identifier[_] => C.AST.ParamDecl =
    C.AST.makeParam({
      case _: DPIA.Types.ManagedBufferType =>
        C.AST.OpaqueType("Buffer")
      case DPIA.Types.ContextType =>
        C.AST.OpaqueType("Context")
      case dt => C.AST.makeParamTy(gen)(dt)
    })
}
