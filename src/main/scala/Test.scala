import PhraseType._

object Test extends App {

  val v1 = Ident[ExpType]("v1")
  v1.t = ExpType(int)
  val l = Lambda(v1, v1)

  println(TypeChecker(l))

  val ty: ExpType -> ExpType
    = FunctionType(ExpType(int), ExpType(float))

  val v2 = Ident[ExpType]("v2")
  v2.t = ExpType(int)
  println(TypeChecker(Apply(l, v2)))

  val one = IntLiteral(1)
  val two = IntLiteral(2)
  val add = BinOp(BinOp.Op.ADD, one, two)
  println(TypeChecker(add))

  val l2 = Lambda(v1, BinOp(BinOp.Op.ADD, one, v1))
  println(TypeChecker(Apply(l2, two)))

}
