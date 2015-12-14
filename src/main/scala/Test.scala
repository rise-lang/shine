import PhraseExtensions._

import scala.collection.immutable.HashMap

object Test extends App {
  // first test
  {
    var store = HashMap[String, Int]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = `new`( λ( ExpType(int) x AccType(int) ) { v =>
      (π2(v) := 42 + 1) `;`
      `new`( λ( ExpType(int) x AccType(int) ) { v2 =>
        (π2(v2) := π1(v) + 1) `;`
        (π2(v)  := π1(v2))
      } ) `;`
      `if`(π1(v) % 2,
        thenP = π2(v) := π1(v) + 1,
        elseP = π2(v) := π1(v) + 10 ) `;`
      (out := π1(v))
    } )

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.evalCommand(store, p) )

    // same program in scala
    {
      var out = 0;
      {
        var v = 0
        v = 42 + 1;
        {
          var v2 = 0
          v2 = v + 1
          v = v2
        }
        if (v % 2 == 0) {
          v = v + 1
        } else {
          v = v + 10
        }
        out = v
      }
      println(out)
    }
  }

  // second test
  {
    var store = HashMap[String, Int]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = `new`( λ( ExpType(int) x AccType(int) ) { v =>
      (π2(v) := 42 + 1) `;`
      `for`(10, { i =>
        π2(v) := i + π1(v)
      }) `;` skip `;`
      `if`(π1(v) % 2,
        thenP = π2(v) := π1(v) + 1,
        elseP = π2(v) := π1(v) + 10 ) `;`
      (out := π1(v))
    } )

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.evalCommand(store, p) )

    // same program in scala
    {
      var out = 0;
      {
        var v = 42 + 1
        for (i <- 0 until 10) {
          v = i + v
        }; ;
        if (v % 2 == 0) {
          v = v + 1
        } else {
          v = v + 10
        }
        out = v
      }
      println(out)
    }
  }

}
