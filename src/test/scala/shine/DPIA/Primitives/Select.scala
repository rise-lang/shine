package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import util._

class Select extends shine.test_util.Tests {
  val id = fun(x => x)

  test("select from generate") {
    val e =
      generate(fun(x => {
        select(x < lidx(2, 6), l(0), select(x < lidx(4, 6), l(1), l(2)))
      })) :: (6`.`int) |> mapSeq(id)

    gen.CProgram(e)
  }

  test("select from generate from gather") {
    val e = fun(12`.`IndexType(6))(input =>
      gather(input, generate(fun(x => {
        select(x < lidx(2, 6), l(0), select(x < lidx(4, 6), l(1), l(2)))
      }))) |> mapSeq(id)
    )

    gen.CProgram(e)
  }
}
