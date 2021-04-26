package shine.GAP8.Compilation

import arithexpr.arithmetic.Cst
import shine.C.AST.{IncludeHeader, ParamKind}
import shine.C.{Compilation, Module}
import shine.DPIA.Compilation.Passes._
import shine.DPIA.Compilation.{FunDef, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.functional
import shine.{C, DPIA}
import util.compiler.DSL.run
import shine.C.Compilation.{ModuleGenerator => CModuleGenerator}

import scala.collection.{immutable, mutable}

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

  //Culprit
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

      //C.AST.Cast or sth
      val structCastStmt = C.AST.Code("struct cluster_params* cl_params = (struct cluster_params*) args;")
      //C.AST.DeclStmt maybe?
      val unpackingStmt = C.AST.Code(
        params.map(pdecl => pdecl.t.print + " " + pdecl.name + " = " + "cl_params->" + pdecl.name + ";").mkString("\n")
      )


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
                C.AST.Block(immutable.Seq(structCastStmt, unpackingStmt, code))),
            paramKinds =
            //Construct new ParamKind() of type analogous to void*, assert in Function.scala fails
              ParamKind(
                outParam.`type`.dataType, C.AST.ParamKind.Kind.output
              ) +: funDef.params.map(p =>
                ParamKind(p.`type`.dataType, C.AST.ParamKind.Kind.input)
              )
          )
        )
      )
  }
}
