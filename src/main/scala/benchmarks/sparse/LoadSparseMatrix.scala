package benchmarks.sparse

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class COOMatrix(numRows:Int, numCols:Int, entries:Array[((Int,Int),Float)])

case class TwoArrayCSR(numRows:Int, numCols:Int, offsets:Array[Int], entries:Array[Array[(Int, Float)]])

case class ThreeArrayCSR(numRows:Int, numCols:Int, offsets:Array[Int], colIdx:Array[Array[Int]], entries:Array[Array[Float]])


object COOMatrix {
  def loadMatrixMarketFormat(file:File):COOMatrix = {
    val src = Source.fromFile(file)
    val lines = src.getLines().buffered

    skipComments(lines) match {
      case lines =>
        val (numRow, numCol) = parseRowColumn(lines.next())
        val entries = parseEntries(lines)
        val result = COOMatrix(numRow, numCol, entries)
        src.close()
        result
    }
  }

  private def skipComments(lines:BufferedIterator[String]):BufferedIterator[String] = {
    lines.head match {
      case null => lines
      case x if x.startsWith("%") =>
        lines.next()
        skipComments(lines)
      case _ => lines
    }
  }

  private def parseRowColumn(line:String):(Int, Int) = {
    val splits = line.split(" ")
    val numRows = splits(0).toInt
    val numCols = splits(1).toInt
    (numRows, numCols)
  }

  private def parseEntries(lines:BufferedIterator[String]):Array[((Int, Int), Float)] = {
    val accum = new ArrayBuffer[((Int, Int), Float)]()
    while(lines.hasNext) {
      val entry = lines.next().split(" ")
      val rowIdx = entry(0).toInt
      val colIdx = entry(1).toInt
      val value = entry(2).toFloat
      accum += (((rowIdx, colIdx), value))
    }
    accum.toArray
  }
}

object TwoArrayCSR {
  def apply(spm:COOMatrix):TwoArrayCSR = {
    val offsetBuffer = new ArrayBuffer[Int]()
    val nnzBuffer = new ArrayBuffer[Array[(Int, Float)]]()
    var offset = 0

    spm.entries.groupBy { case ((rowIdx, _) , _) => rowIdx }.toSeq.sortBy(_._1).foreach {
      case (_, row) =>
        offsetBuffer += offset
        val sortedRow = row.sortBy { case ((_, colIdx), _) => colIdx }
        offset += sortedRow.length
        nnzBuffer += sortedRow.map{ case ((_, colIdx), value) => (colIdx - 1, value) }
    }

    new TwoArrayCSR(spm.numRows, spm.numCols, offsetBuffer.toArray, nnzBuffer.toArray)
  }

  def loadMatrixMarketFormat(file:File) = apply(COOMatrix.loadMatrixMarketFormat(file))
}

object ThreeArrayCSR {
  def apply(spm:COOMatrix):ThreeArrayCSR = {
    val offsetBuffer = new ArrayBuffer[Int]()
    val nnzIdxBuffer = new ArrayBuffer[Array[Int]]()
    val nnzValueBuffer = new ArrayBuffer[Array[Float]]()
    var offset = 0

    spm.entries.groupBy { case ((rowIdx, _) , _) => rowIdx }.toSeq.sortBy(_._1).foreach {
      case (_, row) =>
        offsetBuffer += offset
        val sortedRow = row.sortBy { case ((_, colIdx), _) => colIdx }
        offset += sortedRow.length
        nnzIdxBuffer += sortedRow.map{ case ((_, colIdx), _) => colIdx - 1 }
        nnzValueBuffer += sortedRow.map { case ((_, _), x) => x }
    }

    new ThreeArrayCSR(spm.numRows, spm.numCols, offsetBuffer.toArray, nnzIdxBuffer.toArray, nnzValueBuffer.toArray)
  }
  def loadMatrixMarketFormat(file:File) = apply(COOMatrix.loadMatrixMarketFormat(file))
}
