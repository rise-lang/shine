import PhraseExtensions._

import scala.collection.immutable.HashMap

object Test extends App {

//  val v1 = identifier("v1", ExpType(int))
//  val l = \(v1, v1)
//
//  println(TypeChecker( l ))
//
//  val v2 = identifier("v2", ExpType(int))
//  println(TypeChecker( l(v2) ))
//
//  val one = IntLiteral(1)
//  val two = IntLiteral(2)
//  println(TypeChecker(one + two))
//
//  val l2 = \(ExpType(int))(x => one % x)
//  println(TypeChecker( l2(two) ))
//
//  val p: Pair[ExpType, ExpType] = (v1, one)
//  println(TypeChecker( p ))
//
//  val acc = identifier("x", AccType(int))
//  val assign: Assign = acc := one
//  val s: Seq = assign `;` skip
//
//  println(TypeChecker( s ))

  // makeIfThenElse(CommandType())( Pair(Pair(π1(v) % 2, π2(v) := π1(v) + 1), π2(v) := π1(v) + 10) ) `;`
  {
    var store = HashMap[String, Int]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = New( λ( ExpType(int) x AccType(int) ) { v =>
      (π2(v) := 42 + 1) `;`
      New( λ( ExpType(int) x AccType(int) ) { v2 =>
        (π2(v2) := π1(v) + 1) `;`
        (π2(v)  := π1(v2))
      } ) `;`
      `if`(π1(v) % 2,
        thenP = π2(v) := π1(v) + 1,
        elseP = π2(v) := π1(v) + 10 ) `;`
      (out := π1(v))
    } )

    println( p )

    println( OperationalSemantics.evalCommand(store, p) )

    println( TypeChecker(p) )
  }

}
