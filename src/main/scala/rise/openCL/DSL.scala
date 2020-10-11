package rise.openCL

import rise.core.DSL._
import rise.core.{Expr, Primitive}
import rise.openCL

object DSL {
  object mapGlobal {
    def apply(): Expr = openCL.primitives.mapGlobal(0).primitive
    def apply(e: Expr): Expr = mapGlobal(0)(e)
    def apply(dim: Int): Expr = openCL.primitives.mapGlobal(dim).primitive
  }

  object mapLocal {
    def apply(): Expr = openCL.primitives.mapLocal(0).primitive
    def apply(e: Expr): Expr = mapLocal(0)(e)
    def apply(dim: Int): Expr = openCL.primitives.mapLocal(dim).primitive
  }

  object mapWorkGroup {
    def apply(): Expr = openCL.primitives.mapWorkGroup(0).primitive
    def apply(e: Expr): Expr = mapWorkGroup(0)(e)
    def apply(dim: Int): Expr = openCL.primitives.mapWorkGroup(dim).primitive
  }

  def toMem: Primitive = openCL.primitives.oclToMem.primitive
  def toFun(to: Expr, f: Expr): Expr = fun(x => to(f(x)))
  val toGlobal: Expr = toMem(rise.core.types.AddressSpace.Global)
  def toGlobalFun(f: Expr): Expr = toFun(toGlobal, f)
  val toLocal: Expr = toMem(rise.core.types.AddressSpace.Local)
  def toLocalFun(f: Expr): Expr = toFun(toLocal, f)
  val toPrivate: Expr = toMem(rise.core.types.AddressSpace.Private)
  def toPrivateFun(f: Expr): Expr = toFun(toPrivate, f)

  def oclReduceSeq: Primitive = openCL.primitives.oclReduceSeq.primitive
  def oclReduceSeqUnroll: Primitive = openCL.primitives.oclReduceSeqUnroll.primitive
  def oclIterate: Primitive = openCL.primitives.oclIterate.primitive
  def oclCircularBuffer: Primitive = openCL.primitives.oclCircularBuffer.primitive
  def oclRotateValues: Primitive = openCL.primitives.oclRotateValues.primitive
}
