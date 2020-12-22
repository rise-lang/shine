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

  @primitive case class toFragmentA(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              ArrayType(m, ArrayType(k, dt)) ->: WmmaAMatrix(m, n, k, dt, layout))}}}}
  }

  @primitive case class toFragmentB(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              ArrayType(k, ArrayType(n, dt)) ->: WmmaBMatrix(m, n, k, dt, layout))}}}}
  }

  @primitive case class toFragmentAccumulator(layout: MatrixLayout) extends Primitive with Builder {
      impl{m: Nat =>
        impl{n: Nat  =>
          impl{k: Nat  =>
            impl{dt: DataType =>
              expl((_: Nat) =>
                ArrayType(m, ArrayType(n, dt)) ->: WmmaAccumulator(m, n, k, dt))}}}}
  }

  @primitive case class fromFragment(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              WmmaAccumulator(m, n, k, dt) ->: ArrayType(m, ArrayType(n, dt)))}}}}
  }

  @primitive object generateFragment extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            dt ->: WmmaAccumulator(m, n, k, dt)}}}}
  }

  @primitive object tensorMMA extends Primitive with Builder {
    impl{layoutA: MatrixLayout =>
      impl{layoutB: MatrixLayout =>
        impl{m: Nat =>
          impl{n: Nat =>
            impl{k: Nat =>
              impl{dt: DataType =>
                impl{dt2: DataType =>
                  WmmaAMatrix(m, n, k, dt, layoutA) ->:
                    WmmaBMatrix(m, n, k, dt, layoutB) ->:
                    WmmaAccumulator(m, n, k, dt2) ->: WmmaAccumulator(m, n, k, dt2)}}}}}}}
  }

  @primitive object toSharedMemoryShift extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat =>
        expl((_: Nat) =>
          impl{t: DataType =>
            ArrayType(m, ArrayType(n, t)) ->: ArrayType(m, ArrayType(n, t))})}}
  }

//  @primitive object MapFragmentElements extends Primitive with Builder {
//      impl{fragType: WmmaFragment =>
//        impl{dt: DataType =>
//          (dt ->: dt) ->: fragType ->: fragType}}
//  }

  @primitive case class mapFragmentElements(fragType: WmmaFragment) extends Primitive with Builder {
    //  impl{fragType: WmmaFragment =>
    impl{dt: DataType =>
      (dt ->: dt) ->: fragType ->: fragType}
  }

  @primitive object globalToShared extends Primitive with Builder {
    impl{t: DataType => t ->: t}
  }
}