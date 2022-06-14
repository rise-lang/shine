package exploration.matmath

import exploration.matmath.Operators._

sealed trait Value[T]

case class Scalar[T](value: T) extends Value[T]

case class Vec[T](values: T*) extends Value[T]{
  require(values.nonEmpty)
}
object Vec {
  def generate[T](size: Int)(gen: => T): Vec[T] = Vec[T](Seq.fill(size)(gen):_*)
}

case class Mat[T](cols: Seq[T]*) extends Value[T]{
  require(this.shape._1>0)
  require(this.shape._2>0)
  require(cols.forall(_.length == cols.head.length))
}
object Mat {
  def generate[T](n: Int, m: Int)(gen: => T): Mat[T] = Mat[T](Seq.fill(n,m)(gen):_*)
}