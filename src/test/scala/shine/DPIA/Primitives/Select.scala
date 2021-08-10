package shine.DPIA.Primitives

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Lambda
import rise.core.primitives._
import rise.core.types.DataType._
import util.gen.c.function

class Select extends test_util.Tests {
  val id: ToBeTyped[Lambda] = fun(x => x)

  test("select from generate") {
    val e =
      generate(fun(x =>
        `if`(x < lidx(2, 6))
          .`then`(l(0))
          .`else`(
            `if`(x < lidx(4, 6))
              .`then`(l(1))
              .`else`(l(2))
          )
      )) :: (6`.`int) |> mapSeq(id)

    function.asStringFromExpr(e)
  }

  test("select from generate from gather") {
    val e = fun(12`.`IndexType(6))(input =>
      gather(input)(generate(fun(x =>
        `if`(x < lidx(2, 6))
          .`then`(l(0))
          .`else`(
            `if`(x < lidx(4, 6))
              .`then`(l(1))
              .`else`(l(2))
          )
      ))) |> mapSeq(id)
    )

    function.asStringFromExpr(e)
  }
}
