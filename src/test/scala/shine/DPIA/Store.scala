package shine.DPIA

import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.types._
import util.gen
import util.gen.c.function

class Store extends test_util.Tests {

  test("scalar values") {
    def plusNum(n: Int, code: String): Unit = {
      logger.debug(code)
      "\\+".r.findAllIn(code).length shouldBe n
    }
    // this is surprising behaviour
    plusNum(2, function.asStringFromExpr(fun(int)(x =>
      toMem(x + l(2)) |> fun(y => y * y)
    )))
    // this is surprising behaviour
    plusNum(2, function.asStringFromExpr(fun(int)(x =>
      let(x + l(2)) be (y => y * y)
    )))
    // this is what we actually would expect
    plusNum(1, function.asStringFromExpr(fun(int)(x =>
      (x + l(2)) |> store { y =>
        y * y
      }
    )))
  }

  test("array values") {
    val code = function.asStringFromExpr(depFun((n: Nat) => fun(ArrayType(n, int))(xs =>
      xs |> store2(how = mapSeq(fun(x => x))) |> fun(xs =>
        xs |> mapSeq(fun(x => x))
      )
    )))
    logger.debug(code)
    "for".r.findAllIn(code).length shouldBe 2
  }

  test("array of array values") {
    val code = function.asStringFromExpr(depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, int)))(xs =>
      xs |> store(how = mapSeq(mapSeq(fun(x => x)))) { xs =>
        xs |> mapSeq(mapSeq(fun(x => x)))
      }
    )))
    logger.debug(code)
    "for".r.findAllIn(code).length shouldBe 4
  }

}
