package shine.cuda.ast

import shine.C.AST.BasicType
import shine.DPIA.Nat
import shine.DPIA.Types.MatrixLayout
import shine.cuda.codegen.CodeGenerator

//Helper class for TypeChecking for Wmma-DataTypes
object Wmma {
  import shine.DPIA.Types._

  def typ(dt: DataType) = CodeGenerator.apply().typ(dt)

  val d1 = (16: Nat, 16: Nat, 16: Nat)
  val d2 = (32: Nat, 8: Nat, 16: Nat)
  val d3 = (8: Nat, 32: Nat, 16: Nat)
  val d4 = (8: Nat, 8: Nat, 4: Nat)

  //Supported dimensions for wmma
  val dimensions = Array(d1, d2, d3, d4)

  //Supported dataTypes for wmma
  val dataTypesDimensions: Array[Tuple2[Tuple2[DataType, DataType], Array[Tuple3[Nat, Nat, Nat]]]] = Array(
    ((f16, f16), Array(d1, d2, d3)),
    ((f16, f32), Array(d1, d2, d3)),
    ((f64, f64), Array(d4)),
    ((i8, i32), Array(d1, d2, d3)),
    ((u8, i32), Array(d1, d2, d3)))

  def checkDimensionsAndTypes(m: Nat, n: Nat, k: Nat, dataType: DataType, dataTypeAcc: DataType): Unit = {
    dataTypesDimensions.foreach(dataTypeDimension =>
      if (dataTypeDimension._1 == (dataType, dataTypeAcc) &&
        dataTypeDimension._2.contains((m, n, k))) return)

    throw new Exception(s"found invalid types ($dataType, $dataTypeAcc) and dimensions ($m, $n, $k) for wmma-api")
  }

  def checkFragmentAB(m: Nat, n: Nat, k: Nat, dataType: shine.C.AST.Type): Unit = {
    dataTypesDimensions.foreach(dataTypeDimension =>
      if (typ(dataTypeDimension._1._1) == dataType &&
        dataTypeDimension._2.contains((m, n, k))) return)

    throw new Exception(s"found invalid type $dataType and dimensions ($m, $n, $k) for wmma-api")
  }

  def checkFragmentAccumulator(m: Nat, n: Nat, k: Nat, dataType: shine.C.AST.Type): Unit = {
    dataTypesDimensions.foreach(dataTypeDimension =>
      if (typ(dataTypeDimension._1._2) == dataType &&
        dataTypeDimension._2.contains((m, n, k))) return)

    throw new Exception(s"found invalid type $dataType and dimensions ($m, $n, $k) for wmma-api")
  }

  def toString(layout: MatrixLayout): String = {
    layout match {
      case MatrixLayout.Row_Major => "nvcuda::wmma::row_major"
      case MatrixLayout.Col_Major => "nvcuda::wmma::col_major"
      case _ => throw new Exception("this should not happen")
    }
  }
}

case class WmmaAMatrix(m: Nat,
                       n: Nat,
                       k: Nat,
                       dataType: shine.C.AST.Type,
                       layout: MatrixLayout)
  extends BasicType(s"nvcuda::wmma::fragment<nvcuda::wmma::matrix_a, $m, $n, $k, $dataType," +
    s"${Wmma.toString(layout)}>") {

  Wmma.checkFragmentAB(m, n, k, dataType)
}

case class WmmaBMatrix(m: Nat,
                       n: Nat,
                       k: Nat,
                       dataType: shine.C.AST.Type,
                       layout: MatrixLayout)
  extends BasicType(s"nvcuda::wmma::fragment<nvcuda::wmma::matrix_b, $m, $n, $k, $dataType," +
    s"${Wmma.toString(layout)}>") {

  Wmma.checkFragmentAB(m, n, k, dataType)
}

case class WmmaAccumulator(m: Nat,
                           n: Nat,
                           k: Nat,
                           dataType: shine.C.AST.Type)
  extends BasicType(s"nvcuda::wmma::fragment<nvcuda::wmma::accumulator, $m, $n, $k, $dataType>") {

  Wmma.checkFragmentAccumulator(m, n, k, dataType)
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
