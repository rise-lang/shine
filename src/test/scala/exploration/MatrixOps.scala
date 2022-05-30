package exploration

object MatrixOps {
  implicit class MV[T](m: Mat[T])(implicit num: Numeric[T]){
    import num._
    def *(v:Vec[T]): Vec[T] = Vec(m.size._1, m.cols
      .lazyZip(v.values)
      .map((mi, _) => mi.values
        .lazyZip(v.values)
        .map((mij, vi) => mij * vi)
        .sum
      ): _*
    )
  }
}
