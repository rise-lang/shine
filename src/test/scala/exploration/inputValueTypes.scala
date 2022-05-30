package exploration


sealed trait Value[T]

case class Scalar[T](value: T)

case class Vec[T](size:Int, values: T*) extends Value[T]
object Vec {
  def generate[T](size: Int)(gen: => T): Vec[T] = Vec[T](size, Seq.fill(size)(gen) : _*)
}

case class Mat[T](size: (Int, Int), cols: Vec[T]*) extends Value[T]
object Mat {
  def generate[T](n: Int, m: Int)(gen: => T): Mat[T] = Mat[T]((n,m), Seq.fill(n)(Vec.generate(m)(gen)) : _*)
}