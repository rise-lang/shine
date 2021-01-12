package rise.core

import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.primitives._
import rise.core.semantics.NatData
import rise.openCL.primitives.{oclCount, oclWhich}
import util.Execute

class dependentTypes extends test_util.Tests {
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

  test("Dependent pair match with reduction") {
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
      fun(n `.` f32)(array => count(array)(fun(x => x =:= l(0.0f))))
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("Simple which") {
    val e = depFun((n: Nat) => depFun((count:Nat) =>
      fun(n `.` f32)(array => mapSeq(fun(x => x))(which(array)(count)(fun(x => x =:= l(0.0f)))))
    ))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("Filter and sum") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => {
        def pred = fun(x => x =/= l(0.0f))
        val cnt = count(array)(pred)
        liftN(indexAsNat(cnt))(depFun((cnt: Nat) =>
          reduceSeq(fun(x => fun(_ => x)))(lidx(0, n))(which(array)(cnt)(pred))
        ))
      })
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("List of list one row") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => {
        def pred = fun(x => x =/= l(0.0f))
        val cnt = count(array)(pred)
        liftN(indexAsNat(cnt))(depFun((cnt: Nat) =>
           dpair(cnt)(mapSeq(fun(idx => pair(array `@` idx)(idx)))(which(array)(cnt)(pred)))
        ))
      })
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("List of list") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))
      def cnts = toMem(mapSeq(fun(row => map(pred)(row) |> count |> indexAsNat))(array))

      liftNats(cnts)(depFun((lengths:NatCollection) =>
        dpairNats(lengths)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            which(map(pred)(row))(lengths `@` rowIdx)
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }


  test("Compressed sparse row") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))


      def offs = toMem(array |>
        map(fun(row => map(pred)(row) |> count |> indexAsNat))
        |> scanSeq(fun(x => fun(y => x + y)))(Literal(NatData(0)))
      )

      liftNats(offs)(depFun((offs:NatCollection) =>
        dpairNats(offs)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            which(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }


  test("Compressed sparse row OCL") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))


      def offs = toMem(array |>
        map(fun(row => map(pred)(row) |> oclCount(AddressSpace.Global) |> indexAsNat))
        |> scanSeq(fun(x => fun(y => x + y)))(Literal(NatData(0)))
      )

      liftNats(offs)(depFun((offs:NatCollection) =>
        dpairNats(offs)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            oclWhich(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.OpenCLKernel(inferred, "Foo_foo")
  }
}
