package rise.core

import arithexpr.arithmetic.{GoesToRange, RangeAdd}
import rise.core.TypeLevelDSL._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.primitives._
import shine.C.SizeInByte
import shine.OpenCL.{GlobalSize, HNilHelper, LocalSize, ScalaFunction, `(`, `)=>`, `,`}
import util.Execute

import scala.util.Random

class dependentTypes extends test_util.TestsWithExecutor {
  test("Infer int addition type") {
    val e =
      depFun((n: Nat) =>
        fun(
          DepArrayType(n, n2dtFun(i => (i + 1) `.` f32)) ->: DepArrayType(
            n,
            n2dtFun(i => (i + 1) `.` f32)
          )
        )(xs => xs |> depMapSeq(depFun((i: Nat) => fun(xs => mapSeq(fun(x => x))(xs::((i+1)`.`f32)))))
      ))
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    println(inferred.t)
  }

  test("Dependent pair construct") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(xs => dpair(n)(mapSeq(fun(x => x))(xs)))
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  ignore("Dependent pair take and untake") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(xs =>
        dmatch(dpair(n)(xs))(
          depFun((_:Nat) => fun(xs => xs |> take(5) |> mapSeq(fun(x => x))))
        )
      )
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("GEN: Dependent pair map increment") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((n:Nat) => fun(xs =>
        dpair(n)(mapSeq(fun(x => x + l(1.0f)))(xs) ::(n`.`f32))
      ))))
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val cFunName = "foo"
    val cFun = util.gen.CProgram(inferred, cFunName)

    val testCode =
      s"""
         |#include<stdlib.h>
         |#include<stdio.h>
         |#include<stdint.h>
         |
         |${cFun.code}
         |
         |int main(int argc, char** argv) {
         |    const uint32_t x = 100;
         |
         |    const size_t data_size = sizeof(uint32_t) + x * sizeof(float);
         |    uint8_t data[data_size];
         |    uint8_t output[data_size];
         |    // Gold has same first, and incremented snd
         |    uint8_t gold[data_size];
         |
         |    ((uint32_t*)data)[0] = x;
         |    ((uint32_t*)gold)[0] = x;
         |
         |    float* floats = (float*)(data + sizeof(uint32_t));
         |    float* gold_floats = (float*)(gold + sizeof(uint32_t));
         |
         |    for (int i = 0; i < x; i++) {
         |        floats[i] = (float)i;
         |        gold_floats[i] = ((float)i) + 1.0;
         |    }
         |
         |    $cFunName(output, data);
         |    for (size_t i = 0; i < x; i++) {
         |        if (output[i] != gold[i]) {
         |            return 1;
         |        }
         |    }
         |    return 0;
         |}""".stripMargin
         Execute(testCode)
  }

  test("GEN: Dependent pair match with reduction") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((_: Nat) => fun(xs =>
        reduceSeq(fun(x => fun(y => x + y)))(l(0.0f))(xs))
      ))
    )
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val cFunName = "foo"
    val cFun = util.gen.CProgram(inferred, cFunName)

    val testCode =
      s"""
        |#include<stdlib.h>
        |#include<stdio.h>
        |#include<stdint.h>
        |
        | ${cFun.code}
        |
        |int main(int argc, char** argv) {
        |    const uint32_t x = 3;
        |
        |    uint8_t data[sizeof(uint32_t) + x*sizeof(float)];
        |
        |    ((uint32_t*)data)[0] = x;
        |
        |    float* floats = (float*)(data + sizeof(uint32_t));
        |
        |    float gold = 0.0;
        |    for (int i = 0; i < x; i++) {
        |        floats[i] = (float)i;
        |        // Solution is just sum
        |        gold += (float)i;
        |    }
        |    float output;
        |
        |    $cFunName(&output, data);
        |    printf("%f", output);
        |    if (output == gold) { return 0; } else { return 1; }
        |}
        |""".stripMargin

    Execute(testCode)
  }

  test("Dependent pair match with taking") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((_:Nat) => fun(xs => mapSeq(fun(x => x))(take(5)(xs)))))
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }


  test("Simple count") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => array |> map(fun(x => x =:= l(0.0f))) |> count)
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("C filter even numbers") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(pred) |> count |> indexAsNat)(depFun((count:Nat) =>
        dpair(count)(which(array |> map(pred))(count)
          |> mapSeq(fun(idx => array `@` idx)))
      ))
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val program = util.gen.CProgram(inferred, "kernel")

    println(program.code)

    val code = raw"""
      |
      |#include<stdlib.h>
      |#include<stdio.h>
      |#include<stdint.h>
      |
      |${program.code}

      |
      |int main(int argc, char** argv) {
      |   const uint32_t input_size = 10000;
      |
      |   int input[input_size];
      |
      |   for (int i = 0; i < input_size; i++) {
      |       input[i] = i;
      |   }
      |
      |   int output[1 + input_size/2];
      |
      |   kernel((uint8_t*)output, input_size, input);
      |
      |   int ret = 0;
      |   for(uint32_t i = 0; i < input_size/2; i++) {
      |       uint32_t value = output[1 + i];
      |       if (value != i*2) {
      |         printf("!!!!!{%d, %d}", i, value);
      |         return 1;
      |       }
      |       printf("(%d,%d)", i, value);
      |
      |   }
      |   return 0;
      |}""".stripMargin

    Execute(code)
  }

  test("OCL count numbers") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Global)
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernel = util.gen.OpenCLKernel(inferred, "count")

    // Testing it
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val n = 1000
    val array = Array.tabulate(n)(i => i)
    val (result, _) = kernelF(n `,` array)
    assert(result(0) == n / 2)
  }

  test("OCL count numbers parallel") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      array |> split(8) |> mapGlobal(0)(
        map(fun(x => (x % l(2)) =:= l(0))) >> (oclCount(AddressSpace.Private))
      )
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernel = util.gen.OpenCLKernel(inferred, "ocl_par_count")

    // Testing it
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(4), GlobalSize(8))
    val power = 10
    val n = Math.pow(2, power).toInt
    val array = Array.tabulate(n)(i => i)
    val (result, _) = kernelF(n `,` array)
    val total = result.sum
    assert(total == n / 2)
  }

  test("OCL filter even numbers with external count") {
    val e = depFun((n: Nat) => depFun((count:Nat) =>  fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      oclWhich(array |> map(pred))(count) |> oclToMem(AddressSpace.Global) |> mapSeq(fun(idx => array `@` idx))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val kernel = util.gen.OpenCLKernel(inferred, "ocl_filter")

    val n = 1000
    val array = Array.tabulate(n)(i => i)
    val even = array.filter(_ % 2 == 0)
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Int `,` Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))

    val (oclEven, time) = kernelF(n `,` even.length `,` array)
    assert(even.length == oclEven.length)
    assert(even.zip(oclEven).forall( { case (x, y) => x == y} ))
  }

  test("OCL filter even number bringing it with me") {
    val e = depFun((n: Nat) =>  fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Global) |> indexAsNat)(depFun((count:Nat) =>
      dpair(count)(
              oclWhich(array |> map(pred))(count) |>  oclToMem(AddressSpace.Global) |> mapSeq(fun(idx => array `@` idx))
      )))
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    {
      val kernelWrap = util.gen.OpenCLKernel(inferred, "ocl_filter")

      val n = 1000
      val array = Array.tabulate(n)(i => i)
      val even = array.filter(_ % 2 == 0)

      val kernel = kernelWrap.copy(kernel = kernelWrap.kernel
        .withFallbackOutputSize(SizeInByte(n * 4)).setIntermediateBufferSize(0, SizeInByte(n * 4))
      )
      val kernelF = kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` (Int, Array[Int])].withSizes(LocalSize(4), GlobalSize(32))

      val (dpair, _) = kernelF(n `,` array)
      val (count, data) = dpair
      assert(count == even.length)
      even.zip(data).foreach({ case (x, y) => assert(x == y) })
    }
  }

  test("OCL filter even numbers in parallel") {
    val NUM_THREADS = 1024
    val e = depFun((n: Nat) =>  fun(n `.` int)(array => array |> split(n/NUM_THREADS) |> mapGlobal(0)(fun(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Private) |> indexAsNat)(
        depFun(GoesToRange((n/NUM_THREADS)+1), (count:Nat) => dpair(count)(
          oclWhich(array |> map(pred))(count) |>  oclToMem(AddressSpace.Global) |> mapSeq(fun(idx => array `@` idx))
        )))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernelWrap = util.gen.OpenCLKernel(e)

    val n = 10_000_000//Math.pow(2, 22).toInt
    val numbers = Array.tabulate(n)(i => i)

    val kernel = kernelWrap.copy(kernel = kernelWrap.kernel.withFallbackOutputSize(SizeInByte((NUM_THREADS + n) * 4)).setIntermediateBufferSize(0, SizeInByte(n * 4)))
    val kernelF = kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` Array[Int]].withSizes(LocalSize(64), GlobalSize(NUM_THREADS))


    val (result, time) = kernelF(n `,` numbers)

    println(result)
    println(time)
  }

  test("OCL filter even numbers in parallel no alloc") {
    val n = Math.pow(2, 23).toInt
    val NUM_THREADS = 4096
    println(s"N = $n, NUM_THREADS = $NUM_THREADS")

    val e = depFun((n: Nat) =>  fun(n `.` int)(array => array |> split(n/NUM_THREADS) |> mapGlobal(0)(fun(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Private) |> indexAsNat)(
        depFun(GoesToRange((n/NUM_THREADS)+1), (count:Nat) => dpair(count)(
          oclWhichMap(array |> map(pred))(fun(idx => array `@` idx))(count)
        )))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernelWrap = util.gen.OpenCLKernel(e)

    val numbers = Array.tabulate(n)(i => i)

    val kernel = kernelWrap.copy(kernel = kernelWrap.kernel.withFallbackOutputSize(SizeInByte((NUM_THREADS + n) * 4)).setIntermediateBufferSize(0, SizeInByte(n * 4)))
    val kernelF = kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` Array[Int]].withSizes(LocalSize(128), GlobalSize(NUM_THREADS))


    val (result, time) = kernelF(n `,` numbers)

    //println(result)
    println(time)
  }

  def benchmarkSelectQuery(numThreads:Int, inputSize:Int, localSize:Int, pred: ToBeTyped[Expr]): String = {
    val e = depFun((n: Nat) =>  fun(n `.` int)(array => array |> split(n/numThreads) |> mapGlobal(0)(fun(array => {
      liftN(array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Private) |> indexAsNat)(
        depFun(GoesToRange((n/numThreads)+1), (count:Nat) => dpair(count)(
          oclWhichMap(array |> map(pred))(fun(idx => array `@` idx))(count)
        )))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernelWrap = util.gen.OpenCLKernel(e)

    val n = inputSize
    val numbers = Array.tabulate(n)(i => i)

    val kernel = kernelWrap.copy(kernel = kernelWrap.kernel.withFallbackOutputSize(SizeInByte((numThreads + n) * 4)).setIntermediateBufferSize(0, SizeInByte(n * 4)))
    val kernelF = kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` Array[Int]].withSizes(LocalSize(localSize), GlobalSize(numThreads))

    val (result, time) = kernelF(n `,` numbers)

    //println(result)
    time.toString
  }

  test("BENCH: Select query even") {
   final case class Operation(name: String, exp: ToBeTyped[Expr])
    val operations = Vector(
      Operation("Is Even", fun(x => (x % l(2)) =:= l(0))),
      Operation("Is GT than 100", fun(x => x > l(100))),
      Operation("Precisely 2704", fun(x => x =:= l(2704))),
      Operation("Never", fun(x => x > x)),
      Operation("Always", fun(x => x =:= x)),
    )


      val exponents = List(18, 19, 20, 21, 22, 23, 24)
      val results = operations.flatMap(operation => exponents.map(exp => {
        val inputSize = Math.pow(2, exp).toInt
        val timing = benchmarkSelectQuery(4096, inputSize, localSize = 128, operation.exp)
        (operation.name, exp, timing)
      }))

      results.foreach { case (operation, exp, timing) =>
        println(s"$operation 2^$exp (${Math.pow(2, exp).toInt}: $timing")
      }
  }

  test("OCL inner join query") {
    val NUM_THREADS = 1024

    val e = depFun((n: Nat) => depFun((m: Nat) =>
      fun(n `.` (int x int))(tableA =>
        fun(m `.` (int x int))(tableB =>
          tableA |> mapGlobal(0)(fun(a =>
            liftN(tableB |> map(fun(b => a._1 =:= b._1)) |> oclCount(AddressSpace.Private) |> indexAsNat)(
              depFun(GoesToRange(m), (numMatches: Nat) =>
                dpair(numMatches)(
                  oclWhichMap(tableB |> map(fun(b => a._1 =:= b._1)))(fun(idx => pair(a._2)(tableB `@` idx)._2))(numMatches)
                )
              )
            )))))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernelWrap = util.gen.OpenCLKernel(e)

    val numItems = Math.pow(2, 14).toInt

    val n = numItems / 2
    val m = numItems / 2
    val table1 = Array.tabulate(n)(i => (i, i))
    val table2 = Array.tabulate(m)(i => (i * 2, 52 + i))

    val kernel = kernelWrap.copy(kernel = kernelWrap.kernel
      .withFallbackOutputSize(SizeInByte(n * m * 8)))

    val kernelF = kernel.as[ScalaFunction `(` Int `,` `Int` `,` Array[(Int, Int)] `,` Array[(Int, Int)] `)=>` Array[Int]].withSizes(LocalSize(256), GlobalSize(NUM_THREADS))


    val (result, time) = kernelF(n `,` m `,` table1 `,` table2)

    println(result)
    println(time)
  }

  def benchmarkInnerJoin(n: Int, m: Int, threads: Int, localSize:Int): String = {
    val NUM_THREADS = threads

    val e = depFun((n: Nat) => depFun((m: Nat) =>
      fun(n `.` (int x int))(tableA =>
        fun(m `.` (int x int))(tableB =>
          tableA |> mapGlobal(0)(fun(a =>
            liftN(tableB |> map(fun(b => a._1 =:= b._1)) |> oclCount(AddressSpace.Private) |> indexAsNat)(
              depFun(GoesToRange(m), (numMatches: Nat) =>
                dpair(numMatches)(
                  oclWhichMap(tableB |> map(fun(b => a._1 =:= b._1)))(fun(idx => pair(a._2)(tableB `@` idx)._2))(numMatches)
                )
              )
            )))))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernelWrap = util.gen.OpenCLKernel(e)

    val table1 = Array.tabulate(n)(i => (i, i))
    val table2 = Array.tabulate(m)(i => (i * 2, 52 + i))

    val kernel = kernelWrap.copy(kernel = kernelWrap.kernel
      .withFallbackOutputSize(SizeInByte(n * m * 8)))

    val kernelF = kernel.as[ScalaFunction `(` Int `,` `Int` `,` Array[(Int, Int)] `,` Array[(Int, Int)] `)=>` Array[Int]].withSizes(LocalSize(localSize), GlobalSize(NUM_THREADS))


    val (_, time) = kernelF(n `,` m `,` table1 `,` table2)

    time.toString
  }

  test("BENCH: Inner join") {
    val exponents = List(14)
    val results = exponents.map(exp => {
      val inputSize = Math.pow(2, exp).toInt
      val n = inputSize/2
      val m = inputSize/2
      val timing = benchmarkInnerJoin(n, m, 4096, 256)
      (n, m) -> timing
    })

    results.foreach { case ((n, m), timing) =>
      println(s"n x m: $n x $m: $timing")
    }
  }

  test("bin-centric histogram")  {
    val e =  depFun((numBins: Nat) => depFun((n:Nat) =>
      fun(numBins`.` (f32 x f32))(bins =>
        fun(n `.` f32)(data => bins |> mapGlobal(0)(fun(range =>
          data |> map(fun(x => (x > range._1) && (x < range._2))) |> oclCount(AddressSpace.Private)
        )))
      )
    ))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernel = util.gen.OpenCLKernel(inferred, "histogram")

    {
      val n = Math.pow(2, 22).toInt
      val numBins = 128

      val kernelF =
        kernel.as[ScalaFunction `(` Int `,` Int `,` Array[(Float, Float)] `,` Array[Float] `)=>` Array[Int]]
          .withSizes(LocalSize(128), GlobalSize(numBins))

      val elem = 1.0f/(numBins + 1)
      val bins = Array.tabulate(numBins)(i => (elem * i, elem * (i + 1)))

      val rng = new Random(4)
      val data = Array.tabulate(n)(_ => rng.nextFloat())

      val (output, time) = kernelF(numBins `,` n `,` bins `,` data)
      val golden = bins.map { case (low, high) => data.count(x => x > low && x < high)}
      assert(golden.length == output.length)
      golden.zip(output).map( { case (x, y) => assert(x == y)})

      println(time)
    }
  }

  test("bfsOcl") {
    val nextFrontier = depFun((n:Nat) => depFun((m: Nat) => fun(n `.` (NatType x NatType))(nodes =>
      fun(m `.` NatType)(edges =>
        depFun((frontierLen: Nat) =>
          fun(frontierLen `.` NatType)(frontierItems => {
            fun(n `.` bool)(notSeen =>
            frontierItems |>
              mapGlobal(0)(fun(parentIdx =>
                let(nodes `@` natAsIndex(n)(parentIdx))(fun(parent => {
                  liftN(parent._1)(depFun(GoesToRange(m), (childOffset: Nat) =>
                    liftN(parent._2)(depFun(GoesToRange(m), (numChildren: Nat) => {
                      def myEdges = edges |> drop(childOffset) |> take(numChildren)
                      def myEdgesCond = myEdges |> map(fun(childIdx => notSeen `@` natAsIndex(n)(childIdx)))
                      liftN(
                        myEdgesCond |> oclCount(AddressSpace.Private) |> indexAsNat
                      )(depFun(GoesToRange(n), (count:Nat) =>
                        dpair(count)(
                           oclWhichMap(myEdgesCond)(fun(edgeIdx => myEdges `@` edgeIdx))(count)
                        )
                      ))}
                    ))))
                })))))
          })))
    )))

    val inferred: Expr = TDSL.infer(nextFrontier)
    println(inferred)
    print(inferred.t)
    val program = util.gen.OpenCLKernel(inferred, "bfs_inlined")
  }

  test("bfsOclScan") {
    val nextFrontier = depFun((n:Nat) => depFun((m: Nat) => fun(n `.` (NatType x NatType))(nodes =>
      fun(m `.` NatType)(edges =>
        depFun((frontierLen: Nat) =>
          fun(frontierLen `.` NatType)(frontierItems => {
            fun((frontierLen+1) `.` NatType)(childOffsets =>
            fun(n `.` bool)(notSeen =>
              liftNats(childOffsets)(depFun((offs:NatCollection) =>
                (oclDpairNats(offs)(depFun((_:Nat) => mapGlobal(0)(fun(x => x))))
                    {
                      frontierItems |> toDepArray |>
                        depMapGlobal(0)(depFun((frontierIdx:Nat) => fun(parentIdx =>
                          let(nodes `@` natAsIndex(n)(parentIdx))(fun(parent => {
                            liftN(parent._1)(depFun(GoesToRange(m), (childOffset: Nat) =>
                              liftN(parent._2)(depFun(GoesToRange(m), (numChildren: Nat) => {
                                def myEdges = edges |> drop(childOffset) |> take(numChildren)
                                def myEdgesCond = myEdges |> map(fun(childIdx => notSeen `@` natAsIndex(n)(childIdx)))
                                oclWhichMap(myEdgesCond)(fun(edgeIdx => myEdges `@` edgeIdx))((offs `@` (frontierIdx + 1) )- (offs `@` frontierIdx))
                              }
                              ))))
                          })))))
                    }) |> dpairJoin
                ))
            ))
          })))
    )))

    val inferred: Expr = TDSL.infer(nextFrontier)
    println(inferred)
    print(inferred.t)
    val program = util.gen.OpenCLKernel(inferred, "bfs_offsets")
  }

}
