import PhraseExtensions._
import PhraseType._

object Test extends App {

  val v1 = identifier("v1", ExpType(int))
  val l = \(v1, v1)

  println(TypeChecker( l ))

  val v2 = identifier("v2", ExpType(int))
  println(TypeChecker( l(v2) ))

  val one = IntLiteral(1)
  val two = IntLiteral(2)
  println(TypeChecker(one + two))

  val l2 = \(ExpType(int))(x => one % x)
  println(TypeChecker( l2(two) ))

  val p: Pair[ExpType, ExpType] = (v1, one)
  println(TypeChecker( p ))

  val acc = identifier("x", AccType(int))
  val assign: Assign = acc := one
  val s: Seq = assign `;` skip

  println(TypeChecker( s ))

  val scopedLambda: Phrase[ (ExpType x AccType) -> CommandType ] =
    \ ( ExpType(int) x AccType(int) ) {
      pair =>
        π2(pair) := IntLiteral(42)
    }

  val n = New( scopedLambda )
}
