package shine.DPIA

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.{Nat, _}
import util.gen

class Store extends test_util.Tests {

  test("scalar values") {
    def plusNum(n: Int, code: String): Unit = {
      println(code)
      "\\+".r.findAllIn(code).length shouldBe n
    }
    // this is surprising behaviour
    plusNum(2, gen.CProgram(fun(int)(x =>
      toMem(x + l(2)) |> fun(y => y * y)
    )).code)
    // this is surprising behaviour
    plusNum(2, gen.CProgram(fun(int)(x =>
      let(x + l(2))(fun(y => y * y) )
    )).code)
    // this is what we actually would expect
    plusNum(1, gen.CProgram(fun(int)(x =>
      (x + l(2)) |> store { y =>
        y * y
      }
    )).code)
  }

  test("array values") {
    val code = gen.CProgram(depFun((n: Nat) => fun(ArrayType(n, int))(xs =>
      xs |> store2(how = mapSeq(fun(x => x))) |> fun(xs =>
        xs |> mapSeq(fun(x => x))
      )
    ))).code
    println(code)
    "for".r.findAllIn(code).length shouldBe 2
  }

  test("array of array values") {
    val code = gen.CProgram(depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, int)))(xs =>
      xs |> store(how = mapSeq(mapSeq(fun(x => x)))) { xs =>
        xs |> mapSeq(mapSeq(fun(x => x)))
      }
    ))).code
    println(code)
    "for".r.findAllIn(code).length shouldBe 4
  }

}
