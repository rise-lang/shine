package exploration

import org.scalatest.Ignore
import rise.core.TypedDSL.{add, fst, fun, l, map, reduce, snd, transpose, zip}
import rise.core.types.{ArrayType, f32, infer}

@Ignore
class explore extends shine.test_util.Tests {

  // input size
  val N = 1 << 9

  // example expressions
  val mm = infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
              zip(ak, bk))) $ transpose(b) )) $ a))
  )

  val dot = infer(
    fun(ArrayType(N, f32))(a =>
      fun(ArrayType(N, f32))(b =>
        reduce(add)(l(0.0f)) o map(fun(x => fst(x) * snd(x))) $ zip(a,b)))
  )

  val scal = infer(fun(ArrayType(N, f32))(input =>
    fun(f32)(alpha =>
      map(fun(x => alpha * x)) $ input))
  )

  test("test exploration") {

    // -- todo -- json path relative
//    riseExploration(mm, "/home/jo/development/rise-lang/shine/exploration/configuration/dot_0.json")
//    riseExploration(mm, "/home/jo/development/rise-lang/shine/exploration/configuration/dot_1.json")
//    riseExploration(mm, "/home/jo/development/rise-lang/shine/exploration/configuration/dot_2.json")

    // remove input size things from here
  }

}

