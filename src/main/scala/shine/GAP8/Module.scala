package shine.GAP8

import shine.{C, DPIA}
import shine.C.AST.ParamKind

case class Module(hostCode: C.Module,
                  acceleratorFunctions: Seq[C.Module]) {
  def compose(other: Module): Module =
    Module(
      hostCode.compose(other.hostCode),
      acceleratorFunctions ++ other.acceleratorFunctions
    )
}

object Module {

  val clusterFunctionName = "cluster_core_task"

  def compose(ms: Seq[Module]): Module = ms.reduce(_ compose _)

  def translateToString(m: Module): String = {
    val accFunctions = m.acceleratorFunctions.map(C.Module.translateToString)
    val hostCode = C.Module.translateToString(m.hostCode)

    s"""
       |//Accelerator functions
       |${accFunctions.mkString("\n\n")}
       |//Host code
       |${hostCode}
       |""".stripMargin
  }

  def fromCModule(cmodule: C.Module): Module = {

    val accFunction = cmodule
      .functions
      .find(_.name.equalsIgnoreCase(clusterFunctionName))

    val params = accFunction
      .map(_.params)
      .getOrElse(Seq())

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

    val wrappedFunction = C.AST.Function(
      code =
        C.AST.FunDecl(
          name = clusterFunctionName,
          returnType = C.AST.Type.void,
          params = Seq(C.AST.ParamDecl("args", C.AST.PointerType(C.AST.Type.void))),
          body = C.AST.Block(
            Seq(structCastDecl)
              ++ unpackingStmts
              ++ accFunction.map(f => Seq(f)).getOrElse(Seq()).map(_.code.body)
          )
        ),
      paramKinds =
        Seq(ParamKind(DPIA.Types.OpaqueType("void*"), C.AST.ParamKind.Kind.input))
    )

    Module(
      cmodule.copy(
        decls = cmodule.decls ++ Seq(structDecl),
        functions = cmodule.functions.filterNot(_.name.equalsIgnoreCase(clusterFunctionName)) ++ Seq(wrappedFunction)
      ),
      Seq()
    )

  }
}
