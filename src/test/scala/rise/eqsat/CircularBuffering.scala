package rise.eqsat

import rise.elevate.circularBuffering._
import Basic.proveEquiv

class CircularBuffering extends test_util.Tests {
  test("highLevel to circBuf") {
    proveEquiv(wrapExpr(highLevel), wrapExpr(circBuf), Seq(
      rules.eta, rules.beta,
      rules.takeBeforeMap, rules.dropBeforeMap,
      //rules.takeInSlide, rules.dropInSlide
    ))
  }
}