package idealised.rewriting

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._

class split_join extends idealised.util.Tests {

  test("Apply split-join-rule on simple map-id program") {
    val e1 = fun(ArrayType(128, int))(a => a :>> map(fun(x => x)))
    val e2 = TypeInference(e1, Map())

    assertThrows[MatchError] {
      // this should throw as there is a fun around the map
      Rules.splitJoin(64)(e2)
    }

    val e3 = Elevate.applyAt(Rules.splitJoin(64))(Position(2))(e2)

    println(e3)
  }

}
