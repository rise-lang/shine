package rise.Cuda

import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive
import rise.core.TypeLevelDSL._
import rise.core.types._

object primitives {

  protected def mapTypeScheme: Type =
    impl{n: Nat =>
      impl{s: DataType =>
        impl{t: DataType => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)}}}

  @primitive case class MapBlock(dim: Char) extends Primitive with Builder {
    mapTypeScheme
  }

  @primitive case class MapGlobal(dim: Char) extends Primitive with Builder {
    mapTypeScheme
  }

  @primitive case class MapThreads(dim: Char) extends Primitive with Builder {
    mapTypeScheme
  }

  @primitive case class MapWarp(dim: Char) extends Primitive with Builder {
    mapTypeScheme
  }

  @primitive case class MapLane(dim: Char) extends Primitive with Builder {
    mapTypeScheme
  }

  @primitive case class ToFragmentA(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              ArrayType(m, ArrayType(k, dt)) ->: WmmaAMatrix(m, n, k, dt, layout))}}}}
  }

  @primitive case class ToFragmentB(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              ArrayType(k, ArrayType(n, dt)) ->: WmmaBMatrix(m, n, k, dt, layout))}}}}
  }

  @primitive case class ToFragmentAccumulator(layout: MatrixLayout) extends Primitive with Builder {
      impl{m: Nat =>
        impl{n: Nat  =>
          impl{k: Nat  =>
            impl{dt: DataType =>
              expl((_: Nat) =>
                ArrayType(m, ArrayType(n, dt)) ->: WmmaAccumulator(m, n, k, dt))}}}}
  }

  @primitive case class FromFragment(layout: MatrixLayout) extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            expl((_: Nat) =>
              WmmaAccumulator(m, n, k, dt) ->: ArrayType(m, ArrayType(n, dt)))}}}}
  }

  @primitive object GenerateFragment extends Primitive with Builder {
    impl{m: Nat =>
      impl{n: Nat  =>
        impl{k: Nat  =>
          impl{dt: DataType =>
            dt ->: WmmaAccumulator(m, n, k, dt)}}}}
  }

//  @primitive object TensorMMA extends Primitive with Builder {
//    impl{layoutA: MatrixLayout =>
//      impl{layoutB: MatrixLayout =>
//        impl{m: Nat =>
//          impl{n: Nat =>
//            impl{k: Nat =>
//              impl{dt: DataType =>
//                impl{dt2: DataType =>
//                  WmmaAMatrix(m, n, k, dt, layoutA) ->:
//                    WmmaBMatrix(m, n, k, dt, layoutB) ->:
//                    WmmaAccumulator(m, n, k, dt2) ->: WmmaAccumulator(m, n, k, dt2)}}}}}}}
//  }

  @primitive case class TensorMMA(layoutA: MatrixLayout) extends Primitive with Builder {
//    impl{layoutA: MatrixLayout =>
//      impl{layoutB: MatrixLayout =>
        impl{m: Nat =>
          impl{n: Nat =>
            impl{k: Nat =>
              impl{dt: DataType =>
                impl{dt2: DataType =>
                  WmmaAMatrix(m, n, k, dt, layoutA) ->:
                    WmmaBMatrix(m, n, k, dt, layoutA) ->:
                    WmmaAccumulator(m, n, k, dt2) ->: WmmaAccumulator(m, n, k, dt2)}}}}}
  }

  @primitive object ToSharedMemoryShift extends Primitive with Builder {
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

  @primitive case class MapFragmentElements(fragType: WmmaFragment) extends Primitive with Builder {
    //  impl{fragType: WmmaFragment =>
    impl{dt: DataType =>
      (dt ->: dt) ->: fragType ->: fragType}
  }

  @primitive object GlobalToShared extends Primitive with Builder {
    impl{t: DataType => t ->: t}
  }
}