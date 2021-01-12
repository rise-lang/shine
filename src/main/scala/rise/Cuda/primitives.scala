package rise.Cuda

import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive
import rise.core.DSL.Type._
import rise.core.types._

object primitives {

  protected def mapType: Type =
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}

  @primitive case class mapBlock(dim: Char) extends Primitive with Builder {
    mapType
  }

  @primitive case class mapGlobal(dim: Char) extends Primitive with Builder {
    mapType
  }

  @primitive case class mapThreads(dim: Char) extends Primitive with Builder {
    mapType
  }

  @primitive case class mapWarp(dim: Char) extends Primitive with Builder {
    mapType
  }

  @primitive case class mapLane(dim: Char) extends Primitive with Builder {
    mapType
  }

  @primitive object toFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            impl{fragType: FragmentType =>
              impl{matrixLayout: MatrixLayout =>
                ArrayType(rows, ArrayType(columns, dt)) ->: Fragment(rows, columns, d3, dt, fragType, matrixLayout)}}}}}}
  }

  @primitive object fromFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            Fragment(rows, columns, d3, dt) ->: ArrayType(rows, ArrayType(columns, dt))}}}}
  }

  @primitive object generateFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            impl{fragType: FragmentType =>
              dt ->: Fragment(rows, columns, d3, dt, fragType, MatrixLayout.Row_Major)}}}}}
  }

  @primitive object tensorMMA extends Primitive with Builder {
    impl{layoutA: MatrixLayout =>
      impl{layoutB: MatrixLayout =>
        impl{m: Nat =>
          impl{n: Nat =>
            impl{k: Nat =>
              impl{dt: DataType =>
                impl{dt2: DataType =>
                  Fragment(m, k, n, dt, FragmentType.AMatrix, layoutA) ->:
                    Fragment(k, n, m, dt, FragmentType.BMatrix, layoutB) ->:
                    Fragment(m, n, k, dt2) ->: Fragment(m, n, k, dt2)}}}}}}}
  }

  @primitive object toSharedMemoryShift extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat =>
        expl((_: Nat) =>
          impl{t: DataType =>
            ArrayType(m, ArrayType(n, t)) ->: ArrayType(m, ArrayType(n, t))})}}
  }

  @primitive object mapFragmentElements extends Primitive with Builder {
      impl{fragmentType: DataType =>
        impl{dt: DataType =>
          (dt ->: dt) ->: fragmentType ->: fragmentType}}
  }

  @primitive object globalToShared extends Primitive with Builder {
    impl{t: DataType => t ->: t}
  }
}