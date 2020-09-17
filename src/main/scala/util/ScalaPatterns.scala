package util

import scala.reflect.ClassTag

object ScalaPatterns {

  def pad[T:ClassTag](input:Array[T], padSize:Int, padValue:T):Array[T] = {
    val pad = Array.fill[T](padSize)(padValue)
    pad ++ input ++ pad
  }

  //Scala implementations for common lift primitives that aren't supported in scala by default
    def pad2D[T:ClassTag](input:Array[Array[T]], padSize:Int, padValue:T):Array[Array[T]] = {
      val pad1D = Array.fill(padSize)(padValue)
      val pad2D = Array.fill(padSize * 2 + input.head.length)(padValue)

      val data = Array.fill(padSize)(pad2D) ++ input.map(row => pad1D ++ row ++ pad1D) ++ Array.fill(padSize)(pad2D)
      data
    }
    def slide2D[T:ClassTag](input:Array[Array[T]], stencilSize:Int): Array[Array[Array[Array[T]]]] = {
      input.map(_.sliding(stencilSize, 1).toArray).sliding(stencilSize, 1).map(_.transpose).toArray
    }
}
