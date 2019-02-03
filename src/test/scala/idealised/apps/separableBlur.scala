package idealised.apps

import idealised._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class separableBlur extends idealised.util.Tests {
  // Separable Convolution:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1

  val W = SizeVar("W")
  val H = SizeVar("H")

  val mul2 = fun(t => t._1 * t._2)
  val add = fun(x => fun(a => x + a))
  val dot = fun(a => fun(b => zip(a, b) :>> map(mul2) :>> reduceSeq(add, 0.0f)))

  // TODO: pad
  // TODO: loop unrolling ? OpenCLGenerator::generateForLoopUnrolled
  // TODO: register rotation (!= blocking)
  // Larisa's MapSeqSlide, tiling25Dfix branch, OpenCLGenerator:generateMapSeqSlideLoop, TestMapSeqSlide::reduceSlide2DTest9PointWithWeights
  // TODO: separable rewrite
  // TODO: vectorisation

  val weights2d = LiteralExpr(ArrayData(
    Array(1, 2, 1, 2, 4, 2, 1, 2, 1).map(f => FloatData(f / 16.0f))))
  val weights1d = LiteralExpr(ArrayData(Array(1, 2, 1).map(f => FloatData(f / 4.0f))))

  val blur = {
    val slide2d = map(slide(3, 1)) >>> slide(3, 1) >>> map(transpose())
    val map2d = mapSeq(mapSeq(fun(nbh => dot(weights2d)(join(nbh)))))

    fun(ArrayType(H, ArrayType(W, float)))(input =>
      input :>> /* pad2d >>> */ slide2d :>> map2d
    )
  }

  println("----- BLUR -----")
  generate(blur)

  val separated_blur = {
    val horizontal = mapSeq(slide(3, 1) >>> mapSeq(dot(weights1d)))
    val vertical = slide(3, 1) >>> mapSeq(transpose() >>> mapSeq(dot(weights1d)))
    fun(ArrayType(H, ArrayType(W, float)))(input =>
      input :>> vertical :>> horizontal
    )
  }

  println("----- SEPARATED BLUR -----")
  generate(separated_blur)
  println("----- SEPARATED BLUR, FUSED -----")
  generate({
    fun(ArrayType(H, ArrayType(W, float)))(input =>
      input :>> slide(3, 1) :>> mapSeq(
        transpose() >>> slide(3, 1) >>> mapSeq(mapSeq(dot(weights1d)) >>> dot(weights1d))
      )
    )
  })
  println("----- SEPARATED BLUR, REGISTER ROTATION -----")
  generate({
    fun(ArrayType(H, ArrayType(W, float)))(input =>
      input :>> slide(3, 1) :>> mapSeq(
        transpose() >>> map(dot(weights1d)) >>> mapSeqSlide(3, 1, dot(weights1d))
      )
    )
  })

  // need to detect overlap and reuse opportunity
  /*
  map(dot) |> slide(3, 1) |> map(dot)
  -> slide(3, 1) |> map(map(dot)) |> map(dot)
  -> slide(3, 1) |> map(map(dot) |> dot)
  -> mapSeqSlide(3, 1, mapSeq(dot) |> dot) // rotating 9 unreduced values
  -> mapSeq(dot) |> mapSeqSlide(3, 1, dot) // two passes
  -> map(dot) |> mapSeqSlide(3, 1, dot) // not valid

  -> mapSeqSlide(3, 1, dot, dot) // two functions, looks unflexible and hacky

  -> slideSeq(3, 1, dot) |> map(dot) // more flexible? need map to fold left
  -> mapInput(dot) |> slideSeq(3, 1) |> mapOutput(dot) // more flexible?
  */

  def generate[T <: Type](e: Expr[T]) = {
    val phrase = TypeInference(e, collection.Map()).convertToPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    SyntaxChecker(program.code)
    println(program.code)
  }

}
