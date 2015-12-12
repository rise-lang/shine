import PhraseExtensions._

object Test extends App {

  val v1 = identifier("v1")
  v1.t = ExpType(int)
  val l = Lambda(v1, v1)

  println(TypeChecker( l ))

  val v2 = identifier("v2")
  v2.t = ExpType(int)
  println(TypeChecker( l(v2) ))

  val one = IntLiteral(1)
  val two = IntLiteral(2)
  println(TypeChecker(one + two))

  val l2 = Lambda(v1, one % v1)
  println(TypeChecker( l2(two) ))

  val p = Pair(v1, one)
  println(TypeChecker( p ))

  val acc = Ident[AccType]("x")
  acc.t = AccType(int)
  val assign: Assign = acc := one
  val skip: Seq = assign `;` Skip()

  println(TypeChecker( skip ))

}
