package shine.OpenCL

import shine.{C, OpenCL}

class Kernel(override val code: OpenCL.AST.KernelDecl,
             override val paramMetaData: Seq[C.ParamMetaData],
             val wgConfig: Option[(LocalSize, GlobalSize)]) extends C.Function(code, paramMetaData) {
  def attribute: Option[OpenCL.AST.RequiredWorkGroupSize] = code.attribute
}

object Kernel {
  def apply(code: OpenCL.AST.KernelDecl,
            paramKinds: Seq[C.ParamMetaData],
            wgConfig: Option[(LocalSize, GlobalSize)]): Kernel =
    new Kernel(code, paramKinds, wgConfig)
}
