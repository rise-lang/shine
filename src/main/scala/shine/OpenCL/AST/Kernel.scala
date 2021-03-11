package shine.OpenCL.AST

import shine.C.AST.{Function, ParamKind}
import shine.OpenCL
import shine.OpenCL.{GlobalSize, LocalSize}

class Kernel(override val code: OpenCL.AST.KernelDecl,
             override val paramKinds: Seq[ParamKind],
             val wgConfig: Option[(LocalSize, GlobalSize)]
            ) extends Function(code, paramKinds) {
  def attribute: Option[OpenCL.AST.RequiredWorkGroupSize] = code.attribute
}

object Kernel {
  def apply(code: OpenCL.AST.KernelDecl,
            paramKinds: Seq[ParamKind],
            wgConfig: Option[(LocalSize, GlobalSize)]): Kernel =
    new Kernel(code, paramKinds, wgConfig)
}