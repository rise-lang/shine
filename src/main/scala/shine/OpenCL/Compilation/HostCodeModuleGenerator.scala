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

  override def toImperative(gen: HostCodeGenerator,
                            funDef: FunDef,
                            outParam: Identifier[AccType]
                           ): Phrase[ExpType] => Phrase[CommType] =
    CModuleGenerator.toImperative(gen, funDef, outParam)


  override def imperativeToModule(gen: HostCodeGenerator,
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
      val selfT = C.AST.OpaqueType(s"${funDef.name}_t")
      val self = C.AST.ParamDecl("self", C.AST.PointerType(selfT))
      val params = Seq(C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context")), self) ++
        optionallyManagedParams(funDef.params, outParam).map(makeParam(gen))
      C.Module(
        includes = immutable.Seq(IncludeSource("runtime.h")),
        decls = CModuleGenerator.collectTypeDeclarations(code, params) ++
          declarations ++ selfTypeDeclarations(gen, funDef.name),
        functions = immutable.Seq(
          selfInitFunction(gen, funDef.name),
          selfDestroyFunction(gen, funDef.name),
          C.AST.Function(
            code = C.AST.FunDecl(s"${funDef.name}_run",
              returnType = C.AST.Type.void,
              params,
              body = C.AST.Block(immutable.Seq(code))),
            paramKinds =
              ParamKind.input(OpaqueType("Context")) +:
                ParamKind.input(OpaqueType(selfT.name)) +:
                ParamKind.output(outParam) +: funDef.params.map(ParamKind.input)
          ),
          selfInitRunFunction(gen, funDef, outParam),
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
      case DPIA.Types.OpaqueType(name) =>
        C.AST.OpaqueType(name)
      case dt => C.AST.makeParamTy(gen)(dt)
    })

  private def selfTypeDeclarations(gen: HostCodeGenerator, selfName: String): Seq[C.AST.Decl] = Seq(
    C.AST.StructTypeDecl(s"struct ${selfName}_t", gen.kernelModules.flatMap { km =>
      km.kernels.map(k => C.AST.VarDecl(k.name, C.AST.OpaqueType("Kernel"), None))
    }),
    C.AST.TypedefDecl(C.AST.StructType(s"${selfName}_t", gen.kernelModules.flatMap { km =>
      km.kernels.map(k => (C.AST.OpaqueType("Kernel"), k.name))
    }), s"${selfName}_t")
  )

  private def selfInitFunction(gen: HostCodeGenerator, selfName: String): C.AST.Function = {
    val ctx = C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context"))
    val selfT = C.AST.OpaqueType(s"${selfName}_t")
    val self = C.AST.ParamDecl("self", C.AST.PointerType(selfT))
    C.AST.Function(
      code = C.AST.FunDecl(s"${selfName}_init",
        returnType = C.AST.Type.void,
        params = Seq(ctx, self),
        body = C.AST.Block(gen.kernelModules.flatMap { km =>
          km.kernels.map(k => C.AST.ExprStmt(C.AST.Assignment(
            C.AST.StructMemberAccess(
              C.AST.UnaryExpr(C.AST.UnaryOperator.*, C.AST.DeclRef("self")),
              C.AST.DeclRef(k.name)),
            C.AST.FunCall(C.AST.DeclRef("loadKernel"), Seq(
              C.AST.DeclRef("ctx"), C.AST.DeclRef(k.name)
            ))
          )))
        })
      ),
      paramKinds = Seq(
        ParamKind.input(OpaqueType("Context")),
        ParamKind.output(OpaqueType(selfT.name)))
    )
  }

  private def selfDestroyFunction(gen: HostCodeGenerator, selfName: String): C.AST.Function = {
    val ctx = C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context"))
    val selfT = C.AST.OpaqueType(s"${selfName}_t")
    val self = C.AST.ParamDecl("self", C.AST.PointerType(selfT))
    C.AST.Function(
      code = C.AST.FunDecl(s"${selfName}_destroy",
        returnType = C.AST.Type.void,
        params = Seq(ctx, self),
        body = C.AST.Block(gen.kernelModules.flatMap { km =>
          km.kernels.map(k => C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("destroyKernel"), Seq(
            C.AST.DeclRef("ctx"), C.AST.StructMemberAccess(
              C.AST.UnaryExpr(C.AST.UnaryOperator.*, C.AST.DeclRef("self")),
              C.AST.DeclRef(k.name))
          ))))
        })
      ),
      paramKinds = Seq(
        ParamKind.input(OpaqueType("Context")),
        ParamKind.input(OpaqueType(selfT.name)))
    )
  }

  // generates a convenience function to both initialize the computation context and run it
  private def selfInitRunFunction(gen: HostCodeGenerator,
                                  funDef: FunDef,
                                  outParam: Identifier[AccType]): C.AST.Function = {
    val selfT = C.AST.OpaqueType(s"${funDef.name}_t")
    val coreParams = optionallyManagedParams(funDef.params, outParam).map(makeParam(gen))
    C.AST.Function(
      code = C.AST.FunDecl(s"${funDef.name}_init_run",
        returnType = C.AST.Type.void,
        params = C.AST.ParamDecl("ctx", C.AST.OpaqueType("Context")) +: coreParams,
        body = C.AST.Block(Seq(
          C.AST.DeclStmt(C.AST.VarDecl(funDef.name, selfT, None)),
          C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(s"${funDef.name}_init"), Seq(
            C.AST.DeclRef("ctx"), C.AST.UnaryExpr(C.AST.UnaryOperator.&, C.AST.DeclRef(funDef.name))
          ))),
          C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(s"${funDef.name}_run"),
            Seq(C.AST.DeclRef("ctx"), C.AST.UnaryExpr(C.AST.UnaryOperator.&, C.AST.DeclRef(funDef.name))) ++
            coreParams.map(p => C.AST.DeclRef(p.name))
          )),
          C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(s"${funDef.name}_destroy"), Seq(
            C.AST.DeclRef("ctx"), C.AST.UnaryExpr(C.AST.UnaryOperator.&, C.AST.DeclRef(funDef.name))
          )))
        ))
      ),
      paramKinds =
        ParamKind.input(OpaqueType("Context")) +:
          ParamKind.output(outParam) +: funDef.params.map(ParamKind.input)
    )
  }
}
