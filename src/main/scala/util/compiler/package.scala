package util

package object compiler {
  // A compiler is a function translating a program in language S into
  // a program in language T
  type Compiler[S, T] = S => T

  // A partial compiler is a “piece” of a compiler:
  // partial compilers need “help” from one or more child compilers to produce
  // a complete result. (From "The Compiler Forest" by Mihai Budiu, Joel
  // Galenson, and Gordon D. Plotkin)
  //
  // Partial compilers can be composed with each other:
  //   pc1 composeWithPC pc2
  // resulting in new partial compilers.
  //
  // Partial compilers can also be composed with compilers:
  //   pc composeWith c
  // resulting in a complete compiler.
  //
  // For more operations to compose partial compilers in various ways, see
  //
  type PartialCompiler[S, T, S_, T_] = S => (S_, T_ => T)
}
