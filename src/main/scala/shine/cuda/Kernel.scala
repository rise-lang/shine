package shine.cuda

import shine.{C, OpenCL}
import shine.OpenCL._

class Kernel(override val code: OpenCL.AST.KernelDecl,
             override val paramMetaData: Seq[C.ParamMetaData],
             override val wgConfig: Option[(LocalSize, GlobalSize)],
             val dynamicSharedMemory: Long) extends OpenCL.Kernel(code, paramMetaData, wgConfig) {
}

object Kernel {
  def apply(code: OpenCL.AST.KernelDecl,
            paramKinds: Seq[C.ParamMetaData],
            wgConfig: Option[(LocalSize, GlobalSize)],
            dynamicSharedMemory : Long): Kernel =
    new Kernel(code, paramKinds, wgConfig, dynamicSharedMemory)
}
