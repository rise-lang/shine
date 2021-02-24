package util

object TupleOps extends App {
  object T2 {
    def mapFst[A, B, C](f: A => C): ((A, B)) => (C, B) = {
      case (lhs, rhs) => (f(lhs), rhs)
    }

    def mapSnd[A, B, C](f: B => C): ((A, B)) => (A, C) = {
      case (lhs, rhs) => (lhs, f(rhs))
    }
  }

  implicit class TupOps2[A, B](val x: (A, B)) extends AnyVal {
    def :+[C](y: C): (A, B, C) = (x._1, x._2, y)
    def +:[C](y: C): (C, A, B) = (y, x._1, x._2)
    def ++[C, D](y: (C, D)): (A, B, C, D) = (x._1, x._2, y._1, y._2)
  }

  implicit class TupOps3[A, B, C](val x: (A, B, C)) extends AnyVal {
    def :+[D](y: D): (A, B, C, D) = (x._1, x._2, x._3, y)
    def +:[D](y: D): (D, A, B, C) = (y, x._1, x._2, x._3)
    def ++[D, E](y: (D, E)): (A, B, C, D, E) = (x._1, x._2, x._3, y._1, y._2)
    def ++[D, E, F](y: (D, E, F)): (A, B, C, D, E, F) = (x._1, x._2, x._3, y._1, y._2, y._3)
  }

  implicit class TupOps4[A, B, C, D](val x: (A, B, C, D)) extends AnyVal {
    def :+[E](y: E): (A, B, C, D, E) = (x._1, x._2, x._3, x._4, y)
    def +:[E](y: E): (E, A, B, C, D) = (y, x._1, x._2, x._3, x._4)
    def ++[E, F](y: (E, F)): (A, B, C, D, E, F) = (x._1, x._2, x._3, x._4, y._1, y._2)
    def ++[E, F, G](y: (E, F, G)): (A, B, C, D, E, F, G) = (x._1, x._2, x._3, x._4, y._1, y._2, y._3)
    def ++[E, F, G, H](y: (E, F, G, H)): (A, B, C, D, E, F, G, H) = (x._1, x._2, x._3, x._4, y._1, y._2, y._3, y._4)
  }

  implicit class TupOps5[A, B, C, D, E](val x: (A, B, C, D, E)) extends AnyVal {
    def :+[F](y: F): (A, B, C, D, E, F) = (x._1, x._2, x._3, x._4, x._5, y)
    def +:[F](y: F): (F, A, B, C, D, E) = (y, x._1, x._2, x._3, x._4, x._5)
    def ++[F, G](y: (F, G)): (A, B, C, D, E, F, G) = (x._1, x._2, x._3, x._4, x._5, y._1, y._2)
    def ++[F, G, H](y: (F, G, H)): (A, B, C, D, E, F, G, H) = (x._1, x._2, x._3, x._4, x._5, y._1, y._2, y._3)
    def ++[F, G, H, I](y: (F, G, H, I)): (A, B, C, D, E, F, G, H, I) = (x._1, x._2, x._3, x._4, x._5, y._1, y._2, y._3, y._4)
    def ++[F, G, H, I, J](y: (F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = (x._1, x._2, x._3, x._4, x._5, y._1, y._2, y._3, y._4, y._5)
  }

  implicit class TupOps6[A, B, C, D, E, F](val x: (A, B, C, D, E, F)) extends AnyVal {
    def :+[G](y: G): (A, B, C, D, E, F, G) = (x._1, x._2, x._3, x._4, x._5, x._6, y)
    def +:[G](y: G): (G, A, B, C, D, E, F) = (y, x._1, x._2, x._3, x._4, x._5, x._6)
    def ++[G, H](y: (G, H)): (A, B, C, D, E, F, G, H) = (x._1, x._2, x._3, x._4, x._5, x._6, y._1, y._2)
    def ++[G, H, I](y: (G, H, I)): (A, B, C, D, E, F, G, H, I) = (x._1, x._2, x._3, x._4, x._5, x._6, y._1, y._2, y._3)
    def ++[G, H, I, J](y: (G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = (x._1, x._2, x._3, x._4, x._5, x._6, y._1, y._2, y._3, y._4)
    def ++[G, H, I, J, K](y: (G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = (x._1, x._2, x._3, x._4, x._5, x._6, y._1, y._2, y._3, y._4, y._5)
    def ++[G, H, I, J, K, L](y: (G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = (x._1, x._2, x._3, x._4, x._5, x._6, y._1, y._2, y._3, y._4, y._5, y._6)
  }

  implicit class TupOps7[A, B, C, D, E, F, G](val x: (A, B, C, D, E, F, G)) extends AnyVal {
    def :+[H](y: H): (A, B, C, D, E, F, G, H) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y)
    def +:[H](y: H): (H, A, B, C, D, E, F, G) = (y, x._1, x._2, x._3, x._4, x._5, x._6, x._7)
    def ++[H, I](y: (H, I)): (A, B, C, D, E, F, G, H, I) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2)
    def ++[H, I, J](y: (H, I, J)): (A, B, C, D, E, F, G, H, I, J) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2, y._3)
    def ++[H, I, J, K](y: (H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2, y._3, y._4)
    def ++[H, I, J, K, L](y: (H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2, y._3, y._4, y._5)
    def ++[H, I, J, K, L, M](y: (H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2, y._3, y._4, y._5, y._6)
    def ++[H, I, J, K, L, M, N](y: (H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y._1, y._2, y._3, y._4, y._5, y._6, y._7)
  }

  implicit class TupOps8[A, B, C, D, E, F, G, H](val x: (A, B, C, D, E, F, G, H)) extends AnyVal {
    def :+[I](y: I): (A, B, C, D, E, F, G, H, I) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y)
    def +:[I](y: I): (I, A, B, C, D, E, F, G, H) = (y, x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
    def ++[I, J](y: (I, J)): (A, B, C, D, E, F, G, H, I, J) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2)
    def ++[I, J, K](y: (I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3)
    def ++[I, J, K, L](y: (I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3, y._4)
    def ++[I, J, K, L, M](y: (I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3, y._4, y._5)
    def ++[I, J, K, L, M, N](y: (I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3, y._4, y._5, y._6)
    def ++[I, J, K, L, M, N, O](y: (I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3, y._4, y._5, y._6, y._7)
    def ++[I, J, K, L, M, N, O, P](y: (I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8)
  }

  implicit class TupOps9[A, B, C, D, E, F, G, H, I](val x: (A, B, C, D, E, F, G, H, I)) extends AnyVal {
    def :+[J](y: J): (A, B, C, D, E, F, G, H, I, J) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y)
    def +:[J](y: J): (J, A, B, C, D, E, F, G, H, I) = (y, x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9)
    def ++[J, K](y: (J, K)): (A, B, C, D, E, F, G, H, I, J, K) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2)
    def ++[J, K, L](y: (J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3)
    def ++[J, K, L, M](y: (J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4)
    def ++[J, K, L, M, N](y: (J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4, y._5)
    def ++[J, K, L, M, N, O](y: (J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4, y._5, y._6)
    def ++[J, K, L, M, N, O, P](y: (J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4, y._5, y._6, y._7)
    def ++[J, K, L, M, N, O, P, Q](y: (J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8)
    def ++[J, K, L, M, N, O, P, Q, R](y: (J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9)
  }

  implicit class TupOps10[A, B, C, D, E, F, G, H, I, J](val x: (A, B, C, D, E, F, G, H, I, J)) extends AnyVal {
    def :+[K](y: K): (A, B, C, D, E, F, G, H, I, J, K) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y)
    def +:[K](y: K): (K, A, B, C, D, E, F, G, H, I, J) = (y, x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)
    def ++[K, L](y: (K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2)
    def ++[K, L, M](y: (K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3)
    def ++[K, L, M, N](y: (K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4)
    def ++[K, L, M, N, O](y: (K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5)
    def ++[K, L, M, N, O, P](y: (K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5, y._6)
    def ++[K, L, M, N, O, P, Q](y: (K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5, y._6, y._7)
    def ++[K, L, M, N, O, P, Q, R](y: (K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8)
    def ++[K, L, M, N, O, P, Q, R, S](y: (K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9)
    def ++[K, L, M, N, O, P, Q, R, S, T](y: (K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, y._1, y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9, y._10)
  }

  // generator
  for (n <- 2 to 10) yield {
    val ts = (0 until n).map(i => ('A'+i).toChar).mkString(", ")
    val u = ('A'+n).toChar
    val xs = (0 until n).map(i => "x._"+(i+1)).mkString(", ")
    println(s"implicit class TupOps$n[$ts](val x: ($ts)) extends AnyVal {")
    println(s"  def :+[$u](y: $u): ($ts, $u) = ($xs, y)")
    println(s"  def +:[$u](y: $u): ($u, $ts) = (y, $xs)")
    for (m <- 2 to n) yield {
      val us = (0 until m).map(i => ('A'+(i+n)).toChar).mkString(", ")
      val ys = (0 until m).map(i => "y._"+(i+1)).mkString(", ")
      println(s"  def ++[$us](y: ($us)): ($ts, $us) = ($xs, $ys)")
    }
    println("}")
    println()
  }
}
