package rise.eqsat

import ExprDSL._

class ExprCheck extends test_util.Tests {
  test("withArgument") {
    // (λ. (λ. ((λ. (0 1)) (0 1)))) --> (λ. (λ. ((0 1) 0)))
    // (λ. (0 1)) (0 1) --> (0 1) 0
    assert((app(%(0), %(1)) withArgument app(%(0), %(1)))
      == app(app(%(0), %(1)), %(0)))
    // r1 = (app (lam (app "%6" (app "%5" "%0"))) "%0")
    // r2 = (app (lam (app "%6" r1)) "%0")
    // r3 = (app (lam (app "%6" r2)) %0)
    // (app map (lam (app "%6" r3)))
    // --> (app map (lam (app "%6" (app "%5" (app "%4" (app "%3" (app "%2" "%0")))))))
    assert((app(%(6), app(%(5), %(0))) withArgument %(0))
      == app(%(5), app(%(4), %(0))))
    assert((app(%(6), app(%(5), app(%(4), app(%(3), %(0))))) withArgument %(0))
      == app(%(5), app(%(4), app(%(3), app(%(2), %(0))))))
  }
}
