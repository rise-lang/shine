package rise.Cuda

import rise.core.DSL._
import rise.core._
import rise.core.semantics.HalfData
import rise.core.types._
import rise.core.types.MatrixLayout._
import rise.Cuda.primitives._

object DSL {
  object mapBlock {
    def apply(): MapBlock = MapBlock('x')
    def apply(e: Expr): Expr = MapBlock('x')(e)
    def apply(dim: Char): Expr = MapBlock(dim).primitive
  }

  object mapGlobal {
    def apply(): MapGlobal = MapGlobal('x')
    def apply(e: Expr): Expr = MapGlobal('x')(e)
    def apply(dim: Char): Expr = MapGlobal(dim).primitive
  }

  object mapThreads {
    def apply(): MapThreads = MapThreads('x')
    def apply(e: Expr): Expr = MapThreads('x')(e)
    def apply(dim: Char): Expr = MapThreads(dim).primitive
  }

  object mapWarp {
    def apply(): MapWarp = MapWarp('x')
    def apply(e: Expr): Expr = MapWarp('x')(e)
    def apply(dim: Char): Expr = MapWarp(dim).primitive
  }

  object mapLane {
    def apply(): MapLane = MapLane('x')
    def apply(e: Expr): Expr = MapLane('x')(e)
    def apply(dim: Char): Expr = MapLane(dim).primitive
  }

  object toFragmentA {
    def apply(ldm: Nat): Expr = ToFragmentA(Row_Major)(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentA(layout)(ldm)
  }

  object toFragmentB {
    def apply(ldm: Nat): Expr = ToFragmentB(Row_Major)(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentB(layout)(ldm)
  }

  object toFragmentAccumulator {
    def apply(ldm: Nat): Expr = ToFragmentAccumulator(Row_Major)(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentAccumulator(layout)(ldm)
  }

  object fromFragment {
    def apply(ldm: Nat): Expr = FromFragment(Row_Major)(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = FromFragment(layout)(ldm)
  }

  object toSharedMemoryShift {
    def apply(shift: Nat): Expr = ToSharedMemoryShift(shift)
    def apply(shift: Nat, array: Expr): Expr = ToSharedMemoryShift(shift)(array)
  }

  def generateFragment: Primitive = GenerateFragment.primitive
  def mapFragmentElements: Primitive = MapFragmentElements(WmmaAccumulator(1,1,1,f32)).primitive
  def globalToShared: Primitive = GlobalToShared.primitive
  def tensorMMA: Primitive = TensorMMA.primitive

  def h(f: Float): Literal = literal(HalfData(f))
}