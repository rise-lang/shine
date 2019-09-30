package util

trait AssertSame[T] {
  def apply(a: T, b: T, msg: String): Unit
}

object AssertSame {
  implicit val int: AssertSame[Int] = new AssertSame[Int] {
    def apply(a: Int, b: Int, msg: String): Unit = assert(a == b, msg)
  }
  implicit val float: AssertSame[Float] = new AssertSame[Float] {
    def apply(a: Float, b: Float, msg: String): Unit = assert(Math.abs(a - b) < 0.001, msg)
  }
  implicit def array[T](implicit sameElem: AssertSame[T]): AssertSame[Array[T]] = new AssertSame[Array[T]] {
    def apply(a: Array[T], b: Array[T], msg: String): Unit = {
      assert(a.length == b.length, msg)
      a.zip(b).foreach(tuple => sameElem(tuple._1, tuple._2, msg))
    }
  }
}