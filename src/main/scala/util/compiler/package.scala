package util

package object compiler {
  type Compiler[S, T] = S => T

  // (S, T) --o (S', T')
  type PartialCompiler[S, T, S_, T_] = S => (S_, T_ => T)
}
