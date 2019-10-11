import lift.core.DSL._
import lift.core.semantics._
import lift.core._

val const = fun(x => fun(x => l(5))) $ l(2)
val const0 = Apply(fun(x => fun(x => Literal(IntData(5)))), Literal(IntData(2)))
val const1 = fun(x => Literal(IntData(5)))
val liftConst0 = lifting.liftFunExpr(const0)
val liftConst1 = lifting.liftFunExpr(const1)
println(liftConst0.map(f => f(Identifier("x"))))
println(liftConst1.map(f => f(Identifier("x"))))

val cbn = fun(y => fun(x => x + x) $ y + l(2))
val dt = nFun(n => l(5))