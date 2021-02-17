package benchmarks.icfp2021

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class COOMatrix(numRows:Int, numCols:Int, entries:Array[((Int,Int),Float)])

case class TwoArrayCSR(numRows:Int, numCols:Int, lengths:Array[Int], offsets:Array[Int],  entries:Array[Array[(Int, Float)]]) {
  def lengthDistribution:Array[Int] = {
    val maxLength = lengths.max
    val counts = Array.fill(maxLength+1)(0)
    lengths.foreach {
      i => counts(i) = counts(i) + 1
    }
    counts
  }
}

case class ThreeArrayCSR(numRows:Int, numCols:Int, lengths:Array[Int], offsets:Array[Int], colIdx:Array[Array[Int]], entries:Array[Array[Float]]) {
  def padOnHost(upTo:Int) = {
    if(numRows == upTo) this else {
      val extraNum = upTo - numRows
      ThreeArrayCSR(
        numRows = upTo,
        numCols = numCols,
        lengths = this.lengths ++ Array.fill(extraNum)(0),
        offsets = this.offsets ++ Array.fill(extraNum)(this.offsets.last),
        colIdx = this.colIdx ++ Array.fill(extraNum)(Array[Int]()),
        entries = this.entries ++ Array.fill(extraNum)(Array[Float]())
      )
    }
  }
}


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

  private def skipComments(lines:scala.collection.BufferedIterator[String]):scala.collection.BufferedIterator[String] = {
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

  private def parseEntries(lines:scala.collection.BufferedIterator[String]):Array[((Int, Int), Float)] = {
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

  def fromCustomFormat(file: File):TwoArrayCSR = {
    val src = Source.fromFile(file)
    val lines = src.getLines().buffered


    val (numRows, numCols) = {
      val splits = lines.next().split(" ")
      (splits(0).toInt, splits(1).toInt)
    }
    val lengths = Array.fill(numRows)(-1)
    val offsets = Array.fill(numRows+1)(0)
    val rows:Array[Array[(Int, Float)]] = Array.fill(numRows)(null)

    var currentRow = -1
    var currentCol = -1
    while(lines.hasNext) {
      val line = lines.next()
      if(line == "%") {
        currentRow += 1
        currentCol = -1
      } else {
        val splits = line.split(" ")
        val offset = splits(0).toInt
        val length = splits(1).toInt

        lengths(currentRow) = length
        offsets(currentRow+1) = offset
        val arr = Array.fill(length)((0, 0.0f))
        for(i <- 0 until length) {
          val splits = lines.next().split(" ")
          arr(i) = (splits(0).toInt, splits(1).toFloat)
        }
        rows(currentRow) = arr
      }
    }

    println(s"Loaded ${rows.map(_.length).sum} values")

    TwoArrayCSR(numRows, numCols, lengths, offsets, rows)
  }
}

object ThreeArrayCSR {

  def fromCustomFormat(file: File):ThreeArrayCSR = {
    val src = Source.fromFile(file)
    val lines = src.getLines().buffered

    val (numRows, numCols) = {
      val splits = lines.next().split(" ")
      (splits(0).toInt, splits(1).toInt)
    }
    val offsets = Array.fill(numRows+1)(0)
    val lengths = Array.fill(numRows)(0)
    val rowsIdx:Array[Array[Int]] = Array.fill(numRows)(null)
    val rowsVal:Array[Array[Float]] = Array.fill(numRows)(null)

    var currentRow = -1
    var currentCol = -1

    while(lines.hasNext) {
      val line = lines.next()
      if(line == "%") {
        currentRow += 1
        currentCol = -1
      } else {
        val splits = line.split(" ")
        val offset = splits(0).toInt
        val length = splits(1).toInt

        lengths(currentRow) = length
        offsets(currentRow+1) = offset
        val idxs = Array.fill(length)(0)
        val vals = Array.fill(length)(0.0f)
        for(i <- 0 until length) {
          val splits = lines.next().split(" ")
          idxs(i) = splits(0).toInt
          vals(i) = splits(1).toFloat
        }
        rowsIdx(currentRow) = idxs
        rowsVal(currentRow) = vals
      }
    }

    println(s"Loaded ${rowsVal.map(_.length).sum} values")

    ThreeArrayCSR(numRows, numCols, lengths, offsets, rowsIdx, rowsVal)
  }
}
