package lift.core

import lift.core.primitives.{map, slide, transpose}
import lift.core.DSL._

object HighLevelConstructs {
  val slide2D = nFun(sz => nFun(st => fun(a =>
    a |> map(slide(sz)(st)) |> slide(sz)(st) |> map(transpose)
  )))
}
