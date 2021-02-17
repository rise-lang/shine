package util.compiler

import DSL._

object PartialCompiler {
  def apply[S, T, S_, T_](reduction: S => S_,
                          generation: (S, T_) => T
                         ): PartialCompiler[S, T, S_, T_] = {
    s => {
      val s_ = reduction(s)
      (s_, t_ => generation(s, t_))
    }
  }

  def functor[S, T, S_, T_](f: S => S_, g: T_ => T): S => (S_, T_ => T) =
    PartialCompiler(f, {
      case (_, t_) => g(t_)
    })

  def idPC[S, T]: PartialCompiler[S, T, S, T] =
    PartialCompiler.functor(s => s, t => t)

  def binaryPC[S, T](prefix: S => S,
                     suffix: S => S,
                     compose: (T, T) => T
                      ): PartialCompiler[S, T, (S, S), (T, T)] =
    PartialCompiler.functor(s => (prefix(s), suffix(s)), {
      case (t_prefix, t_suffix) => compose(t_prefix, t_suffix)
    })

  def binaryPC[S, T](split: S => (S, S),
                     compose: (T, T) => T
                    ): PartialCompiler[S, T, (S, S), (T, T)] =
    PartialCompiler.functor(split, {
      case (t_prefix, t_suffix) => compose(t_prefix, t_suffix)
    })

  def seqPC[S, T](decompose: S => Seq[S], compose: Seq[T] => T
                  ): PartialCompiler[S, T, Seq[S], Seq[T]] =
    PartialCompiler.functor(decompose, compose)

  case class stage[S, S_](f: S => S_) {
    def apply[T]: PartialCompiler[S, T, S_, T] =
      PartialCompiler.functor(f, t => t)
  }

  def stagedPC[S1, S2, S3, T](trans1: S1 => S2,
                              trans2: S2 => S3): PartialCompiler[S1, T, S3, T] =
    stage[S1, S2](trans1)[T] <<>> stage[S2, S3](trans2)[T]
}
