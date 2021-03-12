package rise.Cuda

import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive
import rise.core.DSL.Type._
import rise.core.types._

object primitives {

  @primitive case class mapBlock(dim: Int) extends Primitive with Builder {
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}
  }

  @primitive case class mapGlobal(dim: Int) extends Primitive with Builder {
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}
  }

  @primitive case class mapThreads(dim: Int) extends Primitive with Builder {
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}
  }

  @primitive case class mapWarp(dim: Int) extends Primitive with Builder {
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}
  }

  @primitive case class mapLane(dim: Int) extends Primitive with Builder {
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}
  }

  @primitive object asFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            impl{fragType: FragmentKind =>
              impl{matrixLayout: MatrixLayout =>
                ArrayType(rows, ArrayType(columns, dt)) ->: FragmentType(rows, columns, d3, dt, fragType, matrixLayout)}}}}}}
  }

  @primitive object asMatrix extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            FragmentType(rows, columns, d3, dt) ->: ArrayType(rows, ArrayType(columns, dt))}}}}
  }

  @primitive object generateFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            impl{fragType: FragmentKind =>
              dt ->: FragmentType(rows, columns, d3, dt, fragType, MatrixLayout.Row_Major)}}}}}
  }

  @primitive object tensorMMA extends Primitive with Builder {
    impl{layoutA: MatrixLayout =>
      impl{layoutB: MatrixLayout =>
        impl{m: Nat =>
          impl{n: Nat =>
            impl{k: Nat =>
              impl{dt: DataType =>
                impl{dt2: DataType =>
                  FragmentType(m, k, n, dt, FragmentKind.AMatrix, layoutA) ->:
                    FragmentType(k, n, m, dt, FragmentKind.BMatrix, layoutB) ->:
                    FragmentType(m, n, k, dt2) ->: FragmentType(m, n, k, dt2)}}}}}}}
  }

  @primitive object toSharedMemoryShift extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat =>
        expl((_: Nat) =>
          impl{t: DataType =>
            ArrayType(m, ArrayType(n, t)) ->: ArrayType(m, ArrayType(n, t))})}}
  }

  @primitive object mapFragment extends Primitive with Builder {
    impl{rows: Nat =>
      impl{columns: Nat =>
        impl{d3: Nat =>
          impl{dt: DataType =>
            impl{fragmentKind: FragmentKind =>
              impl{matrixLayout: MatrixLayout =>
              (dt ->: dt) ->: FragmentType(rows, columns, d3, dt, fragmentKind, matrixLayout) ->:
                FragmentType(rows, columns, d3, dt, fragmentKind, matrixLayout)}}}}}}
  }

  @primitive object globalToShared extends Primitive with Builder {
    impl{t: DataType => t ->: t}
  }
}
