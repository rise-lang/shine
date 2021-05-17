package shine.cuda.AST

import shine.C.AST.BasicType
import shine.DPIA.Nat
import shine.DPIA.Types.{FragmentKind, MatrixLayout, MatrixLayoutIdentifier}

object Wmma {
  def toString(layout: MatrixLayout): String = {
    layout match {
      case MatrixLayout.Row_Major => "nvcuda::wmma::row_major"
      case MatrixLayout.Col_Major => "nvcuda::wmma::col_major"
      case i: MatrixLayoutIdentifier =>
        if (i.layout != MatrixLayout.None)
          toString(i.layout)
        else
          throw new Exception(s"layout $i not infered!")
      case _ => throw new Exception("this should not happen")
    }
  }
}

case class ExternArrayType(override val elemType: shine.C.AST.Type) extends shine.C.AST.ArrayType(elemType, None, false) {
  override def print: String = "extern" + super.print
}

object Type {
  import shine.C.AST.ArrayType
  import shine.C.AST.Type._

  val half = BasicType("__half")
  val pipeline = BasicType("nvcuda::experimental::pipeline")

  def sizeInBytes(dt: shine.C.AST.Type): Long =
    dt match {
      case _: int.type => 4
      case _: u8.type  | _: i8.type => 1
      case _: u16.type | _: i16.type | _: half.type => 2
      case _: u32.type | _: i32.type | _: float.type => 4
      case _: u64.type | _: i64.type | _: double.type => 8
      case ArrayType(elemType, size, _) => size match {
        case Some(n) =>
          n.eval * sizeInBytes(elemType)
        case None =>
          throw new Exception("array with no arraysize")
      }
      case _ =>
        throw new Exception(s"$dt has unknown size in bytes")
    }
}
