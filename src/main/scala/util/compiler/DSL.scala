package util.compiler

object DSL {
  import Operations._
  implicit class CompositionHelperPC[S, T, S_, T_](pc: S => (S_, T_ => T)) {
    // _pc >>: pc
    def >>:[_S, _T](_pc : _S => (S, T => _T)): _S => (S_, T_ => _T) =
      composePC(_pc, pc)

    // pc x pc2
    def x[S2, T2, S2_, T2_](pc2: S2 => (S2_, T2_ => T2)
                           ): ((S, S2)) => ((S_, S2_), (T_, T2_) => (T, T2)) =
      tensorPC(pc, pc2)
  }

  implicit class CompositionHelperC[S, T](c: S => T) {
    // pc |>: c
    def |>:[_S, _T](pc: _S => (S, T => _T)): _S => _T = composeC(pc, c)

    // c x c2
    def x[S2, T2](c2: S2 => T2): ((S, S2)) => (T, T2) = tensorC(c, c2)
  }

  def map[S, T](c: S => T): Seq[S] => Seq[T] = star(c)

  def partialCompiler[S, T, S_, T_](down: S => S_, up: T_ => T): S => (S_, T_ => T) = PC_func(down, up)

  implicit class PCFuncHelper[S, S_](down: S => S_) {
    def `^v`[T, T_](up: T_ => T): S => (S_, T_ => T) = PC_func(down, up)

    def ▼▲[T, T_](up: T_ => T): S => (S_, T_ => T) = PC_func(down, up)
//    val comp1 =
//      checkTypes _ andThen
//        rewriteToImperative(outParam) andThen
//        (   generateCode(gen, outParam, cFunDef) _
//          ▼▲
//          makeModule(gen, outParam, cFunDef)
//          )
  }

  implicit class PCApplyHelper[S, T](pc: S => (T, T => T)) {
    def asCompiler: S => T = composeC(pc, identity[T])(_)
  }

  def run[S](f: S => ()): S => S = s => {
    f(s)
    s
  }
}
