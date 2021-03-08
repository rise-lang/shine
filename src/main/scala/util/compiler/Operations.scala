package util.compiler

import util.compiler.PartialCompiler.idPC

// The notation in the comments is inspired by
// "The Compiler Forest" by Mihai Budiu, Joel Galenson, and Gordon D. Plotkin
object Operations {
  // Composition: PC<<C>>
  def composeC[S, T, S_, T_](pc: S => (S_, T_ => T),
                             c: S_ => T_
                            ): S => T =
    (s: S) => {
      val (s_, g) = pc(s)
      g(c(s_))
    }

  // Composition: PC<<PC>>
  def composePC[S, T, S_, T_, S__, T__](pc: S => (S_, T_ => T),
                                        pc_ : S_ => (S__, T__ => T_)
                                       ): S => (S__, T__ => T) =
    (s: S) => {
      val (s_, g) = pc(s)
      val (s__, g_) = pc_(s_)
      (s__, g compose g_)
    }

  // Tensor: C1 (x) C2
  def tensorC[S1, S2, T1, T2](c1: S1 => T1,
                              c2: S2 => T2
                             ): ((S1, S2)) => (T1, T2) = {
    case (s1, s2) => (c1(s1), c2(s2))
  }

  // Tensor: PC1 (x) PC2
  def tensorPC[S1, S2, T1, T2, S1_, S2_, T1_, T2_](pc1: S1 => (S1_, T1_ => T1),
                                                   pc2: S2 => (S2_, T2_ => T2),
                                                  ): ((S1, S2)) => ((S1_, S2_), (T1_, T2_) => (T1, T2)) = {
    case (s1, s2) =>
      val (s1_, g1) = pc1(s1)
      val (s2_, g2) = pc2(s2)
      ((s1_, s2_), {
        case (t1, t2) =>
          val (t2_, t1_) = (g2(t2), g1(t1))
          (t1_, t2_)
      })
  }

  // Star: C*
  def star[S, T](c: S => T): Seq[S] => Seq[T] = ss => ss.map(c(_))

  // Conditional: COND
  def condC[S, T](pred: S => Boolean,
                  c1: S => T,
                  c2: S => T
                 ): S => T =
    s => if (pred(s)) { c1(s) } else { c2(s) }

  def condPC[S, T, S_, T_](pred: S => Boolean,
                           pc1: S => (S_, T_ => T),
                           pc2: S => (S_, T_ => T)
                          ): S => (S_, T_ => T) =
    s => if (pred(s)) { pc1(s) } else { pc2(s) }

  // Cases: CASES
  def casesC[S, T, S1, S2](w: S => Either[S1, S2],
                           c1: S1 => T,
                           c2: S2 => T): S => T =
    s => w(s) match {
      case Left(s1) => c1(s1)
      case Right(s2) => c2(s2)
    }

  def casesPC[S, T, S_, T_, S1, S2](w: S => Either[S1, S2],
                                    pc1: S1 => (S_, T_ => T),
                                    pc2: S2 => (S_, T_ => T)): S => (S_, T_ => T) =
    s => w(s) match {
      case Left(s1) => pc1(s1)
      case Right(s2) => pc2(s2)
    }

  // Iteration: DO
  def iterate[S, T](pc: S => (S, T => T),
                    pred: S => Boolean): Int => S => (S, T => T) = {
    case 0 => idPC[S, T]
    case n => condPC(pred, idPC[S, T], composePC(pc, iterate(pc, pred)(n-1)))
  }
}
