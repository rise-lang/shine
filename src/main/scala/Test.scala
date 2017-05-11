
import idealised._
import idealised.Core._
import idealised.DSL.untyped._
import idealised.Compiling.{RewriteToImperative, SubstituteImplementations}
import idealised.OpenCL.Core.ToOpenCL
import idealised.OpenCL.DSL._
import lift.arithmetic._
import opencl.generator.OpenCLPrinter

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object Test extends App {

  {
    val n: ArithExpr = 1024
    val dt1: DataType = int

    println(t"exp[ $n . $n . $dt1 x $dt1 ]")

    println(exp"[$n.$dt1]")

    println(t"exp[$dt1] -> exp[$dt1]")

    println(t"exp[${Cst(1048576)}.($int x $int)]")
  }

  type Data = OperationalSemantics.Data
//  val makeData       = OperationalSemantics.makeData
  val makeArrayData  = OperationalSemantics.makeArrayData
//  val makeMatrixData = OperationalSemantics.makeMatrixData

  // first test
  {
    var store = HashMap[String, Data]()

    val out = identifier("out", AccType(int))
    store = store + (out.name -> 0)

    val p_ = `new`(int, OpenCL.PrivateMemory, v =>
      (π2(v) := Literal(42, int) + Literal(1, int)) `;`
        `new`(int, OpenCL.PrivateMemory, v2 =>
          (π2(v2) := π1(v) + 1) `;`
            (π2(v) := π1(v2))
        ) `;`
        `if`(π1(v) % 2,
          thenP = π2(v) := π1(v) + 1,
          elseP = π2(v) := π1(v) + 10) `;`
        (out := π1(v))
    )

    val p = TypeInference(p_)

    println(TypeChecker(p))

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
//  {
//    var store = HashMap[String, Data]()
//
//    val out = identifier("out", AccType(int))
//    store = store + (out.name -> 0)
//
//    val p = `new`(int, PrivateMemory, v =>
//      (π2(v) := 42 + 1) `;`
//      `for`(Cst(10), { i =>
//        π2(v) := i + π1(v)
//      })  `;` skip `;`
//      `if`(π1(v) % 2,
//        thenP = π2(v) := π1(v) + 1,
//        elseP = π2(v) := π1(v) + 10 ) `;`
//      (out := π1(v))
//    )
//
//    println(p)
//
//    println(TypeChecker(p))
//
//    println(OperationalSemantics.eval(store, p))
//
//    // same program in scala
//    {
//      var out = 0;
//      {
//        var v = 42 + 1
//        for (i <- 0 until 10) {
//          v = i + v
//        }
//        ;
//        ;
//        if (v % 2 != 0) {
//          v = v + 1
//        } else {
//          v = v + 10
//        }
//        out = v
//      }
//      println(out)
//    }
//  }

//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val p = `for`(Cst(2), { i =>
//      `for`(Cst(2), { j =>
//        out `@` (i*2+j) := x `@` (i*2+j) + y `@` (i*2+j)
//      })
//    })
//    println(TypeChecker(p))
//
//    println(OperationalSemantics.eval(store, p))
//  }

//  {
//    var store = HashMap[String, Data]()
//
//    val in1 = identifier("in1", ExpType(ArrayType(5, int)))
//    val in2 = identifier("in2", ExpType(ArrayType(5, int)))
//    val out = identifier("out", AccType(ArrayType(5, int)))
//    store = store + (in1.name -> makeArrayData(1, 2, 3, 4, 5))
//    store = store + (in2.name -> makeArrayData(2, 3, 4, 5, 6))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0, 0))
////    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
////    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2 )
//
//    // this should not type check
//    val p_ = out := map(f, zip(in1, in2))
//
//    val p = TypeInference(p_)
//
//    println(p)
//
//    println(TypeChecker(p))
//
//    println(OperationalSemantics.eval(store, p))
//  }
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
//    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
//    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
//    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)

    val f = λ( x => x._1 + x._2 )

    val p_ = `for`(5, { i =>
      λ(AccType(int) x ExpType(RecordType(int, int))) {
        p => π1(p) := f(π2(p))
      }(Pair(out `@` i, zip(in1, in2) `@` i))
    })

    val p = TypeInference(p_)

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
//    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
//    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
//    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)

    val f = λ( x => x._1 + x._2 )

    val p_ = `for`(5, { i =>
      out `@` i := f(zip(in1, in2) `@` i)
    })

    val p = TypeInference(p_)

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
//    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
//    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
//    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)

    val p_ = `for`(5, { i =>
      out `@` i := (zip(in1, in2) `@` i)._1 + (zip(in1, in2) `@` i)._2
    })

    val p = TypeInference(p_)

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
//    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
//    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
//    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)

    val p_ = `for`(5, { i =>
      (out `@` i) := record(in1 `@` i, in2 `@` i)._1 + record(in1 `@` i, in2 `@` i)._2
    })

    val p = TypeInference(p_)

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
//    store = makeArrayData(store, in1.name, 1, 2, 3, 4, 5)
//    store = makeArrayData(store, in2.name, 2, 3, 4, 5, 6)
//    store = makeArrayData(store, out.name, 0, 0, 0, 0, 0)

    val p_ = `for`(5, { i =>
      (out `@` i) := in1 `@` i + in2 `@` i
    })

    val p = TypeInference(p_)

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
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//    val g = λ( x => map(f, x) )
//
//    val p_ = `for`(Cst(2), { i =>
//      `for`(Cst(2), { j =>
//        λ( AccType(int) x ExpType(ArrayType(2, int)) ) { p =>
//          π1(p) := π2(p) `@` j
//        }(PairPhrase(out `@` (i*2+j), map(g, split(2, zip(x, y))) `@` i))
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // (λj. (λa e. a := e @ j) (out @ i1) (in @ i2))
  //  =>
  // λj. (out @ i1) := ((in @ i2) @ j)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//    val g = λ( x => map(f, x) )
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := (map(g, split(2, zip(x, y))) `@` i) `@` j
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // map(f, in) @ i
  //  =>
  // f (in @ i)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//    val g = λ( x => map(f, x) )
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := g(split(2, zip(x, y)) `@` i) `@` j
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // beta reduction of g
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := map(f, split(2, zip(x, y)) `@` i) `@` j
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // map(f, in) @ i
  //  =>
  // f (in @ i)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := f((split(2, zip(x, y)) `@` i) `@` j)
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // ((split n in) @ i) @ j
  //  =>
  // in @ (i*n+j)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := f( zip(x, y) `@` (i*2+j) )
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // (zip x y) @ i
  //  =>
  // record (x @ i) (y @ i)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val f = λ( x => x._1 + x._2)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := f( record(x `@` (i*2+j), y `@` (i*2+j)) )
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // f = λp. p._1 + p._2
  // f (in @ i)
  //  =>
  // ((in @ i)._1) + ((in @ i)._2)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) :=
//          record(x `@` (i*2+j), y `@` (i*2+j))._1 +
//            record(x `@` (i*2+j), y `@` (i*2+j))._2
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
  // (record (x @ i) (y @ i))._1
  //  =>
  // (x @ i)
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val y = identifier("y", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (y.name -> makeArrayData(2, 3, 4, 5))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, y.name, 2, 3, 4, 5)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//
//    val p_ = `for`(2, { i =>
//      `for`(2, { j =>
//        out `@` (i*2+j) := x `@` (i*2+j) + y `@` (i*2+j)
//      })
//    })
//
//    val p = TypeInference(p_)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }
//
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(int))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeData(store, out.name, 0)
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (out.name -> 0)
//
////    val f = λ( (x1, x2) => x1 + x2 )
//
//    val f = λ( x1 => λ( x2 => x1 + x2 ) )
//
//    // this will not type check
//    val p = out := reduce(f, 0, x)
//
//    println( p )
//
//    println( TypeChecker(p) )
//
//    println( OperationalSemantics.eval(store, p) )
//  }

//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(4, int)))
//    val out = identifier("out", AccType(ArrayType(4, int)))
////    store = makeArrayData(store, x.name, 1, 2, 3, 4)
////    store = makeArrayData(store, out.name, 0, 0, 0, 0)
//    store = store + (x.name -> makeArrayData(1, 2, 3, 4))
//    store = store + (out.name -> makeArrayData(0, 0, 0, 0))
//
//    val plusOne = λ( x => x + 1 )
//    val g = λ( x => map(plusOne, x) )
//      // this will not type check
////    val p = out := iterate(2, g, x)
////
////    println( p )
////
////    println( TypeChecker(p) )
////
////    println( OperationalSemantics.eval(store, p) )
//  }
//
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(2, ArrayType(4, int))))
//    val out = identifier("out", AccType(ArrayType(2, ArrayType(4, int))))
////    store = makeMatrixData(store, x.name, Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8)))
////    store = makeMatrixData(store, out.name, Vector(Vector(0, 0, 0, 0), Vector(0, 0, 0, 0)))
//    store = store + (x.name -> makeArrayData(makeArrayData(1, 2, 3, 4), makeArrayData(5, 6, 7, 8)))
//    store = store + (out.name -> makeArrayData(makeArrayData(0, 0, 0, 0), makeArrayData(0, 0, 0, 0)))
//
//    val plusOne = λ( x => x + 1 )
//
////    val p = out := map(map(plusOne), x)
////
////    println( p )
////
////    println( TypeChecker(p) )
////
////    println( OperationalSemantics.eval(store, p) )
//  }
//
//  {
//    var store = HashMap[String, Data]()
//    val x = identifier("x", ExpType(ArrayType(2, ArrayType(2, int))))
//    val y = identifier("y", ExpType(ArrayType(2, ArrayType(2, int))))
//    val out = identifier("out", AccType(ArrayType(2, ArrayType(2, int))))
////    store = makeMatrixData(store, x.name, Vector(Vector(1, 2), Vector(3, 4)))
////    store = makeMatrixData(store, x.name, Vector(Vector(5, 6), Vector(7, 8)))
////    store = makeMatrixData(store, out.name, Vector(Vector(0, 0), Vector(0, 0)))
//    store = store + (x.name -> makeArrayData(makeArrayData(1, 2), makeArrayData(3, 4)))
//    store = store + (y.name -> makeArrayData(makeArrayData(5, 6), makeArrayData(7, 8)))
//    store = store + (out.name -> makeArrayData(makeArrayData(0, 0), makeArrayData(0, 0)))
//
////    val add = λ( (x, y) => x + y )
//    val add = λ( x => λ( y => x + y ) )
//    val mult = λ( x => x._1 * x._2 )
//
////    val p = out := map(λ( row =>
////        map(λ( col =>
////          reduce(add, 0, map(mult, zip(row, col)))
////        ), y)
////      ), x)
////
////    println( p )
////
////    println( TypeChecker(p) )
////
////    println( OperationalSemantics.eval(store, p) )
//  }
//
  {
    val add = λ( x1 => λ( x2 => x1 + x2 ) )
    val p_ = λ(ExpType(ArrayType(4, int)))(inp =>
      reduce(add, 0, map(reduce(add, 0), split(2, inp)))
    )
    val p = TypeInference(p_).asInstanceOf[Lambda[ExpType, ExpType]]
    println("=====")
    println(p)
    println("=====")

    val p2 = RewriteToImperative(p(identifier("input", p.param.t)))
    println("=====")
    println(p2)
    println("=====")
    TypeChecker(p2)

    val p3 = SubstituteImplementations(p2, SubstituteImplementations.Environment())
    println("=====")
    println(p3)
    println("=====")
    TypeChecker(p3)

// New(λ(tmp,
//    For( Length( Split(2,input) ),
//      λ(i,
//        New(λ(accum,
//          accum.wr := 0 ;
//          For( Length( Split(2,input)[i] ),
//            λ(j,
//              accum.wr := (Split(2, input))[i][j] + accum.rd ) ) ;
//          (tmp.wr)[i] := accum.rd ) ) ) ) ;
//    New(λ(accum,
//      accum.wr := 0 ;
//      For( Length(tmp.rd),
//        λ(tmp5,
//          accum.wr := (tmp.rd)[tmp5] + accum.rd ) ) ;
//      output := accum.rd ) ) ) )

    println(PrettyPhrasePrinter(p3))

//    new (λ tmp.
//      for (length (split 2 input))
//        (λ v101.
//          new (λ v102.
//            v102._2 := IntData(0) ;
//            for (length (split 2 input)[v101])
//              (λ v103. v102._2 := ((split 2 input)[v101][v103] + v102._1) ) ;
//            tmp._2[v101] := v102._1
//          )
//        ) ;
//      new (λ v104.
//        v104._2 := IntData(0) ;
//        for (length tmp._1)
//          (λ v105. v104._2 := (tmp._1[v105] + v104._1) ) ;
//        output := v104._1
//      )
//    )


  }

  {
    println("== scal ==")
    val a: Phrase[ExpType] = 5
    val t = ExpType(ArrayType(1048576, int))
    val p = λ(t)(inp =>
      map(λ( x => x * a )) $ inp
    )
    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(?, ?))(p, identifier("inp", t))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== assum ==")
    val t = ExpType(ArrayType(1048576, int))
    val p = λ(t)(inp =>
      reduce(λ( x1 => λ( x2 => x1 + x2 ) ), 0, map(λ( x => `if`(x < 0, -x, x) ), inp))
    )
    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(?, ?))(p, identifier("inp", t))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== dot ==")
    val t: ExpType = ExpType(ArrayType(1048576, int))
    val p: Phrase[ExpType -> (ExpType -> ExpType)] =
      λ(t)(xs => λ(t)(ys =>
        reduce(λ(x1 => λ(x2 => x1 + x2 )), 0) o map(λ(p => p._1 * p._2 )) $ zip(xs, ys)
      ) )
    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(?, ?))(p, identifier("xs", t), identifier("ys", t))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== gemv ==")
    val a: Phrase[ExpType] = 5
    val b: Phrase[ExpType] = 2
    val n = 1048576
    val m = n / 2
    val xsVectorT: ExpType = ExpType(ArrayType(n, int))
    val ysVectorT: ExpType = ExpType(ArrayType(m, int))
    val matrixT: ExpType = ExpType(ArrayType(m, ArrayType(n, int)))


    val add = λ(x1 => λ(x2 => x1 + x2 ))
    val mult = λ(p => p._1 * p._2 )

    val scal = λ(a => λ(vec =>  mapSeq(λ(x => x * a )) $ vec ))

    val dot = λ(xs => λ(ys => reduce(add, 0) o mapSeq(mult) $ zip(xs, ys) ))

    val p =
      λ(matrixT)(mat => λ(xsVectorT)(xs => λ(ysVectorT)(ys => {
        val lhs = mapSeq( λ(x => x * a ) o dot(xs) ) $ mat
        val rhs = scal(b) $ ys
        mapSeq(mult) $ zip(lhs, rhs)
      }) ) )

    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val toOpenCL = new ToOpenCL(localSize = 128, globalSize = m)

    val ast = toOpenCL(p, identifier("mat", matrixT), identifier("xs", xsVectorT), identifier("ys", ysVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  { // TODO: check generated code!!!
    println("== join split ==")

    val xsVectorT = ExpType(ArrayType(1048576, int))

    // results in type error after RewriteToImperative
    val p_ = λ(xsVectorT)(inp =>
      join() o split(2048) $ inp
    )
    val p = TypeInference(p_).asInstanceOf[Lambda[ExpType, ExpType]]
    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(?, ?))(p, identifier("xs", xsVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== asum Nvidia ==")
    val xsVectorT: ExpType = ExpType(ArrayType(1048576, int))

    val abs = (x: Phrase[ExpType]) => `if`(x < 0, -x, x)

    val p = λ(xsVectorT)(inp =>

      mapWorkgroup(
        mapLocal( reduce(λ( x1 => λ( x2 => x1 + abs(x2) ) ), 0) )
      ) o split(128) o split(2048) $ inp
    )
    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(128, ?))(p, identifier("xs", xsVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== PACT paper example ==")
    val n: ArithExpr = SizeVar("N")
    val xsVectorT: ExpType = ExpType(ArrayType(n, int))
    val ysVectorT: ExpType = ExpType(ArrayType(n, int))

    val p =
      λ(xsVectorT)(xs => λ(ysVectorT)(ys => {

        mapWorkgroup(
          mapLocal(
            reduceSeq(λ(xy => λ(a => a + ( xy._1 * xy._2 ) )), 0)
          ) o split(2)
        ) o split(128) $ zip(xs, ys)

      }) )

    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(128, ?))(p, identifier("xs", xsVectorT), identifier("ys", ysVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== gemv fused ==")
    val alpha = 5
    val beta = 2
    val a: Phrase[ExpType] = alpha
    val b: Phrase[ExpType] = beta
    val n: ArithExpr = SizeVar("N")
    val m: ArithExpr = n / 2
    val xsVectorT: ExpType = ExpType(ArrayType(n, int))
    val ysVectorT: ExpType = ExpType(ArrayType(m, int))
    val matrixT: ExpType = ExpType(ArrayType(m, ArrayType(n, int)))

    val p =
      λ(matrixT)(mat => λ(xsVectorT)(xs => λ(ysVectorT)(ys => {

        mapWorkgroup(λ(t =>
          toGlobal(mapLocal( λ(x =>
            x + (t._2 * b)
          ) )) o
          toLocal(mapLocal(
            λ(x => x * a ) o
            reduceSeq(λ(y => λ(acc => acc + ( y._1 * y._2 ) )), 0)
          )) o split(n) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      }) ) )

    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(?, ?))(p, identifier("mat", matrixT), identifier("xs", xsVectorT), identifier("ys", ysVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== sgemv hawaii best ==")
    val n = SizeVar("N")
    val m = SizeVar("M")
    val matrixT = ExpType(ArrayType(n, ArrayType(m, int)))
    val xsVectorT = ExpType(ArrayType(m, int))
    val ysVectorT = ExpType(ArrayType(n, int))
    val aT = ExpType(int)
    val bT = ExpType(int)

    val p =
      λ(matrixT)(mat => λ(xsVectorT)(xs => λ(ysVectorT)(ys => λ(aT)(a => λ(bT)(b => {

        mapWorkgroup(λ(t =>

          toGlobal( λ(x =>
            (x * a) + (t._2 * b)
          ) )
          o
          toLocal(reduceSeq(λ(y => λ(acc => acc + y)), 0))
          o
          toLocal(mapLocal(
            reduceSeq(λ(y => λ(acc => acc + ( y._1 * y._2 ) )), 0)
          )) o split(m /^ 128) o gather(reorderWithStridePhrase(128)) $ zip(xs, t._1)

        )) $ zip(mat, ys)

      }) ) ) ) )

    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val toOpenCL = new ToOpenCL(localSize = 128, globalSize = n)

    val ast = toOpenCL(p, identifier("mat", matrixT), identifier("xs", xsVectorT), identifier("ys", ysVectorT), identifier("alpha", aT), identifier("beta", bT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }


  {
    println("== toLocal ==")
    val n: ArithExpr = SizeVar("N")
    val xsVectorT: ExpType = ExpType(ArrayType(n, int))

    val p =
      λ(xsVectorT)(xs =>
        toLocal(λ( x => x), xs)
      )

    println("=====")
    println(PrettyPhrasePrinter(p))

    println("-----")

    val ast = (new ToOpenCL(128, ?))(p, identifier("xs", xsVectorT))
    println(OpenCLPrinter()(ast))

    println("-----")
  }

  {
    println("== test ==")

    {
      val x = NamedVar("x")
      val f: `(nat)->`[ExpType] = x -> ExpType(ArrayType(x, int))
    }

    {
      val dt: DataType = float
      val f_ : Phrase[ `(nat)->`[ExpType -> ExpType] ] = _Λ_( l =>
        λ(ExpType(ArrayType(l, dt)))( in =>
          mapSeq( reduceSeq(λ(y => λ(acc => acc + y)), 0.0f) ) o split(2) $ in
        )
      )

      val f = TypeInference(f_)

      println( f )

      println( f.apply(1024) )
    }

    {
      val n: ArithExpr = NamedVar("n")
      val f_ = _Λ_( (dt: DataTypeIdentifier) =>
        λ(ExpType(ArrayType(n, dt)))( in =>
          join() o mapSeq( λ(x => x) ) o split(2) $ in
        )
      )

      val f = TypeInference(f_)

      println( f )

      Core.xmlPrinter.toFile("/tmp/f1.xml", f)

      println( f(float) )
    }

  }

}
