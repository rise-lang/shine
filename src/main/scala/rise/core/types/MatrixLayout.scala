package rise.core.types

sealed trait MatrixLayout

object MatrixLayout {
  object Row_Major extends MatrixLayout { override def toString = "Row_Major" }
  object Col_Major extends MatrixLayout { override def toString = "Col_Major" }
  object None extends MatrixLayout
}

final case class MatrixLayoutIdentifier(name: String) extends MatrixLayout {
  override def toString: String = name

  // TODO: this needs revisiting
  var layout: MatrixLayout = MatrixLayout.None

  def setLayout(matrixLayout: MatrixLayout): Unit = {
    if (layout == MatrixLayout.None)
      layout = matrixLayout
    else if (layout != matrixLayout)
      throw new Exception(s"could not unify $layout and $matrixLayout")
  }
}
