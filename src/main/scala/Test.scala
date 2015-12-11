
object Test extends App {


  val v1 = Ident("v1")
  v1.t = Exp(int)
  val l = Lambda(v1, v1)

  println(TypeChecker(l))

  val v2 = Ident("v2")
  v2.t = Exp(int)
  println(TypeChecker(Apply(l, v2)))

}
