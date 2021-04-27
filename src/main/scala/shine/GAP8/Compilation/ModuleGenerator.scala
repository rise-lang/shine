package shine.GAP8.Compilation

import shine.C.AST.{IncludeHeader, ParamKind}
import shine.C.Compilation.{ModuleGenerator => CModuleGenerator}
import shine.C.{Compilation, Module}
import shine.DPIA.Compilation.FunDef
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.{C, DPIA}

import scala.collection.immutable

object ModuleGenerator extends DPIA.Compilation.ModuleGenerator[FunDef] {
  override type Module = C.Module
  override type CodeGenerator = Compilation.CodeGenerator

  override def createOutputParam(outT: ExpType): Identifier[AccType] =
    CModuleGenerator.createOutputParam(outT)

  def toImperative(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[ExpType] => Phrase[CommType] =
    CModuleGenerator.toImperative(gen, funDef, outParam)

  override def imperativeToModule(gen: CodeGenerator,
                                  funDef: FunDef,
                                  outParam: Identifier[AccType]
                                 ): Phrase[CommType] => Module = {
    imperativePasses andThen
      generateCode(gen, funDef, outParam) andThen
      makeGAP8Module(gen, funDef, outParam)
  }

  def imperativePasses: Phrase[CommType] => Phrase[CommType] =
    CModuleGenerator.imperativePasses

  def generateCode(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[CommType] => (Seq[gen.Decl], gen.Stmt) =
    CModuleGenerator.generateCode(gen, funDef, outParam)

  def makeGAP8Module(gen: CodeGenerator,
                  funDef: FunDef,
                  outParam: Identifier[AccType]
                 ): ((Seq[gen.Decl], gen.Stmt)) => Module = {
    case (declarations, code) =>
      val params = (outParam +: funDef.params).
        map(C.AST.makeParam(C.AST.makeParamTy(gen)))

      val structDecl = C.AST.StructTypeDecl(
        "struct cluster_params",
        params.map(i => C.AST.VarDecl(i.name, i.t))
      )

      val structCastDecl = {
        C.AST.DeclStmt(
          C.AST.VarDecl(
            "cl_params",
            C.AST.PointerType(
              C.AST.StructType("cluster_params", Seq())
            ),
            Some(C.AST.Cast(C.AST.PointerType(C.AST.StructType("cluster_params", Seq())), C.AST.Literal("args")))
          )
        )
      }

      val unpackingStmts = params.map{paramDecl =>
        C.AST.DeclStmt(
          C.AST.VarDecl(
            paramDecl.name,
            paramDecl.t,
            Some(C.AST.StructMemberAccess(C.AST.Literal("(*cl_params)"), C.AST.DeclRef(paramDecl.name)))
        ))
      }

      Module(
        includes = immutable.Seq(IncludeHeader("stdint.h")),
        decls = CModuleGenerator.collectTypeDeclarations(code, params) ++ declarations ++ Seq(structDecl),
        functions = immutable.Seq(
          C.AST.Function(
            code =
              C.AST.FunDecl(
                funDef.name,
                returnType = C.AST.Type.void,
                Seq(C.AST.ParamDecl("args", C.AST.PointerType(C.AST.Type.void))),
                C.AST.Block(immutable.Seq(structCastDecl) ++ unpackingStmts ++ immutable.Seq(code))),
            paramKinds =
              immutable.Seq(ParamKind(DPIA.Types.OpaqueType("void*"), C.AST.ParamKind.Kind.input))
          )
        )
      )
  }
}
