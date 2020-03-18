package exploration

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types.{f32}

class executeC extends shine.test_util.Tests {

  test("test execution"){
    val simpleScal =
    nFun(n => fun(n `.` f32)(input => fun(f32)(alpha =>
      input |> mapSeq(fun(x => alpha * x)))
    ))

    val performanceValue = exploration.search.executeC(simpleScal, 10)

    println("performanceValue: " + performanceValue)
  }

}
