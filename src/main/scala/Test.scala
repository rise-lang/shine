import OperationalSemantics._
import PhraseExtensions._

import scala.collection.immutable.HashMap

object Test extends App {
  // first test
  {
    var store = HashMap[String, Data]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = `new`(v =>
      (π2(v) := 42 + 1) `;`
        `new`(v2 =>
          (π2(v2) := π1(v) + 1) `;`
            (π2(v) := π1(v2))
        ) `;`
        `if`(π1(v) % 2,
          thenP = π2(v) := π1(v) + 1,
          elseP = π2(v) := π1(v) + 10) `;`
        (out := π1(v))
    )

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))

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
    var store = HashMap[String, Data]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = `new`( v =>
      (π2(v) := 42 + 1) `;`
        `for`(10, { i =>
          π2(v) := i + π1(v)
        }) `;` skip `;`
        `if`(π1(v) % 2,
          thenP = π2(v) := π1(v) + 1,
          elseP = π2(v) := π1(v) + 10) `;`
        (out := π1(v))
    )

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))

    // same program in scala
    {
      var out = 0;
      {
        var v = 42 + 1
        for (i <- 0 until 10) {
          v = i + v
        }
        ;
        ;
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

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> ArrayData(Vector(1, 2, 3, 4, 5)))
    store = store + (in2.name -> ArrayData(Vector(2, 3, 4, 5, 6)))
    store = store + (out.name -> ArrayData(Vector(0, 0, 0, 0, 0)))

    val f = λ( (x, y) => x + y )

    val p = out := Map(f, Zip(in1, in2))

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))
  }

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> ArrayData(Vector(1, 2, 3, 4, 5)))
    store = store + (in2.name -> ArrayData(Vector(2, 3, 4, 5, 6)))
    store = store + (out.name -> ArrayData(Vector(0, 0, 0, 0, 0)))

    val f = λ( (x, y) => x + y )

    val p = `for`(Length(in1), { i =>
      λ(AccType(int) x ExpType(RecordType(int, int))) {
        p => π1(p) := f(π2(p))
      }(Pair(out `@` i, Zip(in1, in2) `@` i))
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))
  }

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> ArrayData(Vector(1, 2, 3, 4, 5)))
    store = store + (in2.name -> ArrayData(Vector(2, 3, 4, 5, 6)))
    store = store + (out.name -> ArrayData(Vector(0, 0, 0, 0, 0)))

    val f = λ( (x, y) => x + y )
//   ==
//    val f = λ(ExpType(RecordType(int, int))) { x => x._1 + x._2 }

    val p = `for`(Length(in1), { i =>
      out `@` i := f(Zip(in1, in2) `@` i)
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))
  }

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> ArrayData(Vector(1, 2, 3, 4, 5)))
    store = store + (in2.name -> ArrayData(Vector(2, 3, 4, 5, 6)))
    store = store + (out.name -> ArrayData(Vector(0, 0, 0, 0, 0)))

    val p = `for`(Length(in1), { i =>
      out `@` i := (Zip(in1, in2) `@` i)._1 + (Zip(in1, in2) `@` i)._2
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.evalCommand(store, p))
  }

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> ArrayData(Vector(1, 2, 3, 4, 5)))
    store = store + (in2.name -> ArrayData(Vector(2, 3, 4, 5, 6)))
    store = store + (out.name -> ArrayData(Vector(0, 0, 0, 0, 0)))

    val p = `for`(Length(out), { i =>
      (out `@` i) := in1 `@` i + in2 `@` i
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.evalCommand(store, p) )
  }

}
