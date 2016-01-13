import Core._
import Core.OperationalSemantics.implicits._
import DSL._

import scala.collection.immutable.HashMap

object Test extends App {

  type Data = OperationalSemantics.Data
  val makeArrayData = OperationalSemantics.makeArrayData

  // first test
  {
    var store = HashMap[String, Data]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p = `new`(v =>
      (π2(v) := Literal(42) + Literal(1)) `;`
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

    println(Printer.toC(store, p))

    println(OperationalSemantics.eval(store, p))

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
        if (v % 2 != 0) {
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
        elseP = π2(v) := π1(v) + 10 ) `;`
      (out := π1(v))
    )

    println(p)

    println(TypeChecker(p))

    println(Printer.toC(store, p))

    println(OperationalSemantics.eval(store, p))

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
        if (v % 2 != 0) {
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

    val N = 5

    val in1 = identifier("in1", ExpType(ArrayType(N, int)))
    val in2 = identifier("in2", ExpType(ArrayType(N, int)))

    val add = λ( x => x._1 + x._2 )

    val expression = map(add, zip(in1, in2))

    val resultType = TypeChecker(expression) match {
      case ExpType(dt) => AccType(dt)
    }

    val out = identifier("out", resultType)
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val p0 = out := expression

    import Rewriting.RewriteRules._

    val p1 = mapToFor.rewrite(p0)

    val p2 = p1 match {
      case ForPhrase(n, Lambda(i, Assign(acc, expr))) =>
        ForPhrase(n, Lambda(i, Assign(acc, betaReduction.rewrite(expr))))
    }

    val p3 = p2 match {
      case ForPhrase(n, Lambda(i, Assign(acc, BinOp(op, FieldAccess(j, lhs), FieldAccess(k, rhs))))) =>
        ForPhrase(n, Lambda(i, Assign(acc, BinOp(op, FieldAccess(j, zipIndex.rewrite(lhs)), FieldAccess(k, zipIndex.rewrite(rhs))))))
    }

    val p4 = p3 match {
      case ForPhrase(n, Lambda(i, Assign(acc, BinOp(op, lhs, rhs)))) =>
        ForPhrase(n, Lambda(i, Assign(acc, BinOp(op, recordFieldAccess.rewrite(lhs), recordFieldAccess.rewrite(rhs)))))
    }

    val p = p4

    println( Printer.toC(store, p) )

    println(p)

    println(OperationalSemantics.eval(store, p))
  }

  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val f = λ( x => x._1 + x._2 )

    val p = out := map(f, zip(in1, in2))

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.eval(store, p))
  }
  // out := map f in
  //  =>
  // for (length in) ( λi. (λa e. a := f e) (out @ i) (in @ i) )
  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val f = λ( x => x._1 + x._2 )

    val p = `for`(length(in1), { i =>
      λ(AccType(int) x ExpType(RecordType(int, int))) {
        p => π1(p) := f(π2(p))
      }(Pair(out `@` i, zip(in1, in2) `@` i))
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.eval(store, p))
  }
  // λi. (λa e. a := f e) (out @ i) (in @ i)
  //  =>
  // λi. (out @ i) := f (in @ i)
  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val f = λ( x => x._1 + x._2 )

    val p = `for`(length(in1), { i =>
      out `@` i := f(zip(in1, in2) `@` i)
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.eval(store, p))
  }
  // f = λp. π1(p) + π2(p)
  // f (in @ i)
  //  =>
  // (π1 (in @ i)) + (π2 (in @ i))
  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val p = `for`(length(in1), { i =>
      out `@` i := (zip(in1, in2) `@` i)._1 + (zip(in1, in2) `@` i)._2
    })

    println(p)

    println(TypeChecker(p))

    println(OperationalSemantics.eval(store, p))
  }
  // (zip x y) @ i
  //  =>
  // record (x @ i) (y @ i)
  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val p = `for`(length(out), { i =>
      (out `@` i) := Record(in1 `@` i, in2 `@` i)._1 + Record(in1 `@` i, in2 `@` i)._2
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // (record (x @ i) (y @ i)) _1
  //  =>
  // (x @ i)
  {
    var store = HashMap[String, Data]()

    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
    val out = identifier("out", AccType(ArrayType(5, int)))
    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))

    val p = `for`(length(out), { i =>
      (out `@` i) := in1 `@` i + in2 `@` i
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }

  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)
    val g = λ( x => map(f, x) )
    val p = out := join(map(g, split(2, zip(x, y))))

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // out := join in
  // in: Array(n, Array(m, _))
  //  =>
  // for n (λi.
  //    for m (λj.
  //        (λ a e. a := e @ j) (out @ (i*n+j)) (in @ i) ))
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)
    val g = λ( x => map(f, x) )

    val p = `for`(2, { i =>
      `for`(2, { j =>
        λ( AccType(int) x ExpType(ArrayType(2, int)) ) { p =>
          π1(p) := π2(p) `@` j
        }(Pair(out `@` (i*2+j), map(g, split(2, zip(x, y))) `@` i))
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // (λj. (λa e. a := e @ j) (out @ i1) (in @ i2))
  //  =>
  // λj. (out @ i1) := ((in @ i2) @ j)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)
    val g = λ( x => map(f, x) )

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := (map(g, split(2, zip(x, y))) `@` i) `@` j
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // map(f, in) @ i
  //  =>
  // f (in @ i)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)
    val g = λ( x => map(f, x) )

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := g(split(2, zip(x, y)) `@` i) `@` j
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // beta reduction of g
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := map(f, split(2, zip(x, y)) `@` i) `@` j
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // map(f, in) @ i
  //  =>
  // f (in @ i)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := f((split(2, zip(x, y)) `@` i) `@` j)
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // ((split n in) @ i) @ j
  //  =>
  // in @ (i*n+j)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := f( zip(x, y) `@` (i*2+j) )
      })
    })
    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // (zip x y) @ i
  //  =>
  // record (x @ i) (y @ i)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val f = λ( x => x._1 + x._2)

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := f( Record(x `@` (i*2+j), y `@` (i*2+j)) )
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // f = λp. p._1 + p._2
  // f (in @ i)
  //  =>
  // ((in @ i)._1) + ((in @ i)._2)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) :=
          Record(x `@` (i*2+j), y `@` (i*2+j))._1 +
            Record(x `@` (i*2+j), y `@` (i*2+j))._2
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
  // (record (x @ i) (y @ i))._1
  //  =>
  // (x @ i)
  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val y = identifier("y", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val p = `for`(2, { i =>
      `for`(2, { j =>
        out `@` (i*2+j) := x `@` (i*2+j) + y `@` (i*2+j)
      })
    })

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }

  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(1, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (out.name -> makeArrayData(0))

    val f = λ( (x1, x2) => x1 + x2 )

    val p = out := reduce(f, 0, x)

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }

  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(4, int)))
    val out = identifier("out", AccType(ArrayType(4, int)))
    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
    store = store + (out.name -> makeArrayData(0, 0, 0, 0))

    val plusOne = λ( x => x + 1 )
    val g = λ( x => map(plusOne, x) )

    val p = out := iterate(2, g, x)

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }

  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(2, ArrayType(4, int))))
    val out = identifier("out", AccType(ArrayType(2, ArrayType(4, int))))
    store = store + (x.name -> makeArrayData(makeArrayData(1, 2, 3, 4), makeArrayData(5, 6, 7, 8)))
    store = store + (out.name -> makeArrayData(makeArrayData(0, 0, 0, 0), makeArrayData(0, 0, 0, 0)))

    val plusOne = λ( x => x + 1 )

    val p = out := map(map(plusOne), x)

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }

  {
    var store = HashMap[String, Data]()
    val x = identifier("x", ExpType(ArrayType(2, ArrayType(2, int))))
    val y = identifier("y", ExpType(ArrayType(2, ArrayType(2, int))))
    val out = identifier("out", AccType(ArrayType(2, ArrayType(2, int))))
    store = store + (x.name -> makeArrayData(makeArrayData(1, 2), makeArrayData(3, 4)))
    store = store + (y.name -> makeArrayData(makeArrayData(5, 6), makeArrayData(7, 8)))
    store = store + (out.name -> makeArrayData(makeArrayData(0, 0), makeArrayData(0, 0)))

    val add = λ( (x, y) => x + y )
    val mult = λ( x => x._1 * x._2 )

    val p = out := map(λ( row =>
        join(map(λ( col =>
          reduce(add, 0, map(mult, zip(row, col)))
        ), y))
      ), x)

    println( p )

    println( TypeChecker(p) )

    println( OperationalSemantics.eval(store, p) )
  }
}
