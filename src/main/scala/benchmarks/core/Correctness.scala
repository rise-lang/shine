package benchmarks.core

sealed trait Correctness {
  final def isCorrect:Boolean = this == Correct

  def check():Unit = {
    println("Correctness check:")
    println(this.printoutText)
    assert(this.isCorrect)
  }

  def printoutText:String = {
    def wrongValueText(wrongValue:Int):String = if(wrongValue > 0) { s"$wrongValue values are wrong;" } else ""
    def wrongSizeText(sp: Option[SizePair]):String = sp.map(
      {
        case SizePair(actual, expected) => s"Size wrong: expected $expected but $actual found;"
      }).getOrElse("")

    this match {
      case Correct => "Correct"
      case Unchecked => "Unchecked"
      case Wrong(wrongValue, wrongSize) => wrongValueText(wrongValue) ++ wrongSizeText(wrongSize)
    }
  }
}

case class SizePair(actualOutputSize:Int, expectedOutputSize:Int)

case object Correct extends Correctness
case object Unchecked extends Correctness
final case class Wrong(numberWrong:Int, wrongSize:Option[SizePair]) extends Correctness

object Correctness {
  def apply(kernelOutput:Array[Float], scalaOutput:Array[Float]):Correctness = {
    if(kernelOutput.length == scalaOutput.length) {
      val numWrong = countWrong(kernelOutput, scalaOutput)
      if(numWrong == 0) Correct else Wrong(numWrong, wrongSize = None)
    } else {
      val (kOut, sOut) = matchSize(kernelOutput, scalaOutput)
      val wrongCount = countWrong(kOut, sOut)
      Wrong(wrongCount, wrongSize = Some(SizePair(kernelOutput.length, scalaOutput.length)))
    }
  }

  private def countWrong(a:Array[Float], b:Array[Float]):Int = {
    a.zip(b).zipWithIndex.filter(x => Math.abs(x._1._1 - x._1._2) > 0.1).foreach(x => println(x._2))
    a.zip(b).count{case (x,y) => Math.abs(x - y) > 0.01}
  }

  private def matchSize(a:Array[Float], b:Array[Float]):(Array[Float], Array[Float]) = {
    if(a.length > b.length) (a.take(b.length), b) else (a, b.take(a.length))
  }
}