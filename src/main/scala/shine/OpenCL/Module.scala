package shine.OpenCL

case class Module(host: shine.C.Module, kernels: Seq[KernelModule]) {
  def compose(other: Module): Module =
    Module(
      host compose other.host,
      kernels ++ other.kernels)
}

object Module {
  def compose(ms: Seq[Module]): Module = ms.reduce(_ compose _)

  def translateToString(m: Module): String =
    s"""
       |${m.kernels.map { km =>
      // assumes a single kernel per module
      val name = km.kernels(0).code.name
      s"""const char ${name}_source[] =
         |"${util.gen.opencl.kernel.asString(km).linesIterator.toArray.mkString(s"${'"'}\n${'"'}")}";
         |""".stripMargin
    }.mkString("\n")}
       |#define loadKernel(ctx, ident) loadKernelFromSource(ctx, #ident, ident##_source, sizeof(ident##_source) - 1)
       |${util.gen.c.function.asString(m.host)}
       |""".stripMargin

  def dumpToDirectory(dir: java.io.File)(m: Module): Unit = {
    util.writeToPath(s"${dir.getAbsolutePath}/host.c",
      s"""#define loadKernel(ctx, ident) loadKernelFromFile(ctx, #ident, #ident ".cl")
         |util.gen.c.function.asString(host)
         |""".stripMargin)
    m.kernels.foreach { km =>
      // assumes a single kernel per module
      val fileName = km.kernels(0).code.name
      util.writeToPath(s"${dir.getAbsolutePath}/$fileName.cl",
        util.gen.opencl.kernel.asString(km))
    }
  }
}
