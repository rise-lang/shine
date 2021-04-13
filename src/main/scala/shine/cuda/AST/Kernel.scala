package shine.cuda.AST

import shine.C.AST.ParamKind
import shine.OpenCL
import shine.OpenCL._

class Kernel(override val code: OpenCL.AST.KernelDecl,
             override val paramKinds: Seq[ParamKind],
             override val wgConfig: Option[(LocalSize, GlobalSize)],
             val dynamicSharedMemory: Long) extends OpenCL.AST.Kernel(code, paramKinds, wgConfig) {
}

object Kernel {
  def apply(code: OpenCL.AST.KernelDecl,
            paramKinds: Seq[ParamKind],
            wgConfig: Option[(LocalSize, GlobalSize)],
            dynamicSharedMemory : Long): Kernel =
    new Kernel(code, paramKinds, wgConfig, dynamicSharedMemory)
}
