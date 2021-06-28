package shine.GAP8.Compilation

import shine.C.AST.{ParamDecl, ParamKind}
import shine.DPIA.Compilation.{FunDef, ModuleGenerator}
import shine.DPIA.Phrases.{Identifier, Phrase}
import shine.DPIA.Types.{AccType, CommType, ExpType}
import shine.{C, DPIA}

// scalastyle:off
object HostCodeModuleGenerator extends ModuleGenerator[FunDef] {

  override type Module = C.Module
  override type CodeGenerator = HostCodeGenerator


  override def createOutputParam(outT: ExpType): Identifier[AccType] = ???

  override def toImperative(gen: HostCodeGenerator, funDef: FunDef, outParam: Identifier[AccType]): Phrase[ExpType] => Phrase[CommType] =
    C.Compilation.ModuleGenerator.toImperative(gen, funDef, outParam)

  override def imperativeToModule(gen: HostCodeGenerator, funDef: FunDef, outParam: Identifier[AccType]): Phrase[CommType] => Module =
    

  def generateClusterEntryPoint(entryPointName: String, taskFunction: String): C.AST.Function = {
    C.AST.Function(
      code = C.AST.FunDecl(
        name = entryPointName,
        returnType = C.AST.Type.void,
        params = Seq(
          C.AST.ParamDecl("args", C.AST.PointerType(C.AST.Type.void))
        ),
        body = C.AST.Block(Seq(
          C.AST.ExprStmt(C.AST.FunCall(
            C.AST.DeclRef("pi_cl_team_fork"),
              Seq(
                C.AST.FunCall(C.AST.DeclRef("pi_cl_cluster_nb_cores"), Seq()),
                C.AST.DeclRef(taskFunction),
                C.AST.DeclRef("args")
              )
          ))
        ))
      ),
      paramKinds = Seq(
        ParamKind(DPIA.Types.OpaqueType("void*"), C.AST.ParamKind.Kind.input)
      )
    )
  }

  def generateKickoffFunc(entryPointName: String): C.AST.Function = {
    C.AST.Function(
      code = C.AST.FunDecl(
        name = "main",
        returnType = C.AST.Type.int,
        params = Seq(
          C.AST.ParamDecl("argc", C.AST.Type.int),
          C.AST.ParamDecl("argv", C.AST.PointerType(C.AST.Type.char))
        ),
        body = C.AST.Block(Seq(
          C.AST.ExprStmt(C.AST.FunCall(
            C.AST.DeclRef("printf"),
            Seq(C.AST.Literal(s"\n\n\t *** ${entryPointName} ***\n\n"))
          )),
          C.AST.Return(
            C.AST.FunCall(
              C.AST.DeclRef("pmsis_kickoff"),
              Seq(
                C.AST.Cast(
                  C.AST.PointerType(C.AST.Type.void), C.AST.DeclRef(entryPointName)
                )
              )
            )
          )
        ))
      ),
      paramKinds = Seq(
        ParamKind(DPIA.Types.int, C.AST.ParamKind.Kind.input),
        ParamKind(DPIA.Types.OpaqueType("char*"), C.AST.ParamKind.Kind.input)
      )
    )
  }

  /**
    * struct cluster_params cl_params;
    * cl_params.e621 = b1;
    * cl_params.output = b0;
    * */
  def generatePackingCode(structName: String, params: Seq[ParamDecl]): Seq[C.AST.Stmt] = {
    ???
  }
}
