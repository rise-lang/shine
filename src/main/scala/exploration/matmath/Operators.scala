package exploration.matmath

object Operators {
  implicit class SimpleValOps(value: Value[_]) {
    def flatSize: Int = value match {
      case mat: Mat[_] => mat.shape._1 * mat.shape._2
      case vec: Vec[_] => vec.size
      case _: Scalar[_] => 1
    }
  }

  implicit class SimpleVecOps(v: Vec[_]) {
    def size: Int = v.values.size
  }

  implicit class SimpleMatOps[T](mat: Mat[T]) {
    def shape: (Int, Int) = (mat.cols.length, mat.cols.head.size)
    def transpose: Mat[T] = Mat(mat.cols.transpose:_*)

    def padClamp(padding: Int): Mat[T] = padClamp(padding,padding,padding,padding)
    def padClamp(left: Int, right: Int, top: Int, bottom: Int): Mat[T] = {
      val cols = mat.cols
      Mat(cols
        .reverse.padTo(left, cols.head)
        .reverse.padTo(right, cols.last)
        .map(col => col
          .reverse.padTo(bottom, col.head)
          .reverse.padTo(top, col.last)
        ):_*
      )
    }

    def slide(sizeOuter: Int, sizeInner: Int)(stepOuter: Int = 1, stepInner:Int = 1): Seq[Seq[Mat[T]]] = {
      mat.cols
        .map(col => col.sliding(sizeInner, stepInner).toSeq)
        .sliding(sizeOuter, stepOuter).toSeq
        .map(_.transpose.map(slide => Mat(slide:_*)))
    }
  }

  implicit class ArithmeticMatOps[T](val mat: Mat[T])(implicit num: Numeric[T]) {
    import num._
    def *(v: Vec[T]): Vec[T] = Vec(mat.cols
      .lazyZip(v.values)
      .map((mi, _) => mi
        .lazyZip(v.values)
        .map((mij, vi) => mij * vi)
        .sum
      ): _*
    )
  }
}
