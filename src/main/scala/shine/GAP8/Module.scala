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
    val accFunctions = m.acceleratorFunctions
      .map(injectUnpacking)
      .map(C.Module.translateToString)

    val hostCode = C.Module.translateToString(m.hostCode)

    s"""
       |//Accelerator functions
       |${accFunctions.mkString("\n\n")}
       |//Host code
       |$hostCode
       |""".stripMargin
  }

  def fromCModule(cmodule: C.Module): Module = {
    Module(
      C.Module(Seq(), Seq(), Seq()),
      Seq(cmodule)
    )
  }

  /**
    * Injects necessary declarations and structure unpacking code. Changes interface of the generated function
    * to conform with the GAP8 low-level accelerator function interface
    * */
  def injectUnpacking(cmodule: C.Module): C.Module = {
    val unpacked = generateUnpackingCode(cmodule)

    cmodule.copy(
      decls = unpacked._1,
      functions = unpacked._2
    )
  }

  private def generateUnpackingCode(cmodule: C.Module): (Seq[C.AST.Decl], Seq[C.AST.Function]) = {
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

    //TODO: Recheck, this is not needed if HWCE is not used. (Move to AcceleratorCodeGenerator?)
    /**
      * Sets the corresponding bit in the event mask to 1. This is presumably necessary to enable the logic needed
      * to work with HWCE
      * */
    val setEventMaskStmt = C.AST.ExprStmt(C.AST.FunCall(
      C.AST.DeclRef("eu_evt_maskSet"),
      Seq(
        C.AST.Literal("1 << ARCHI_CL_EVT_ACC0")
      )
    ))

    val wrappedFunction = C.AST.Function(
      code =
        C.AST.FunDecl(
          name = clusterFunctionName,
          returnType = C.AST.Type.void,
          params = Seq(C.AST.ParamDecl("args", C.AST.PointerType(C.AST.Type.void))),
          body = C.AST.Block(
            Seq(setEventMaskStmt)
              ++ Seq(structCastDecl)
              ++ unpackingStmts
              ++ accFunction.map(f => Seq(f)).getOrElse(Seq()).map(_.code.body)
          )
        ),
      paramKinds =
        Seq(ParamKind(rise.core.types.DataType.OpaqueType("void*"), C.AST.ParamKind.Kind.input))
    )

    val otherFunctions = cmodule.functions.filterNot(_.name.equalsIgnoreCase(clusterFunctionName))

    (cmodule.decls ++ Seq(structDecl), otherFunctions ++ Seq(wrappedFunction))
  }
}
