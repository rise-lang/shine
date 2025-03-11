package rise.core

import rise.core.DSL._
import Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.core.primitives._
import util.Execute
import util.gen.c.function

class dependentTypes extends test_util.Tests {
  test("Infer int addition type") {
    val e =
      depFun((n: Nat) =>
        fun(
          DepArrayType(n, n2dtFun(i => (i + 1) `.` f32)) ->: DepArrayType(
            n,
            n2dtFun(i => (i + 1) `.` f32)
          )
        )(xs => xs |> depMapSeq(depFun((i: Nat) => fun(xs => mapSeq(fun(x => x))(xs::((i+1)`.`f32))))))
      )
    val inferred: Expr = e.toExpr
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      expl((n: Nat) => (n `*.`n2dtFun(i => (i + 1) `.` f32)) ->: (n `*.`n2dtFun(i => (i + 1) `.` f32)))
    )
  }

  test("Dependent pair construct") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(xs => makeDepPair(n)(mapSeq(fun(x => x))(xs)))
    )
    val inferred: Expr = e.toExpr
    val expected = expl((n: Nat) => (n `.` f32) ->: (Nat `**` (m => m `.` f32)))
    assert(inferred.t =~= expected)
    function.asStringFromExpr(inferred)
  }

  test("GEN: Dependent pair map increment") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((n:Nat) => fun(xs =>
        makeDepPair(n)(mapSeq(fun(x => x + lf32(1.0f)))(xs) ::(n`.`f32))
      ))))
    val inferred: Expr = e.toExpr
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      ((Nat `**` (n => n`.`f32)) ->: (Nat `**` (m => m `.` f32)))
    )

    val cFunName = "foo"
    val cFun = function(cFunName).asStringFromExpr(inferred)

    val testCode =
      s"""
         |#include<stdlib.h>
         |#include<stdio.h>
         |#include<stdint.h>
         |
         |$cFun
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
        reduceSeq(fun(x => fun(y => x + y)))(lf32(0.0f))(xs))
      ))
    )
    val inferred: Expr = e.toExpr
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      ((Nat `**` (n => n`.`f32)) ->: f32)
    )

    val cFunName = "foo"
    val cFun = function(cFunName).asStringFromExpr(inferred)

    val testCode =
      s"""
        |#include<stdlib.h>
        |#include<stdio.h>
        |#include<stdint.h>
        |
        | $cFun
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
    val inferred: Expr = e.toExpr
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      ((Nat `**` (n => n`.`f32)) ->: (5`.`f32))
    )
    function.asStringFromExpr(inferred)
  }

  // Relies on the explicitly dependent work
  ignore("Simple nested") {
    val e = depFun((n: Nat) => fun(n `*.` (i => (i+1) `.` f32))(array =>
        depMapSeq(depFun((_: Nat) => mapSeq(fun(x => x))))(array)
      ))

    val inferred: Expr = infer(e)
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      expl((n: Nat) => (n `*.` n2dtFun(m => (m+1) `.` f32) ) ->: (n `*.` n2dtFun(m => (m+1) `.` f32) ))
    )
    function.asStringFromExpr(inferred)
  }

  // Relies on the explicitly dependent work
  ignore("Simple reduce") {
    val e = depFun((n: Nat) => fun(n `*.` (i => (i+1) `.` f32))(array =>
      depMapSeq(depFun((_: Nat) => reduceSeq(fun(x => fun(y => x + y)))(lf32(0.0f))))(array)
    ))

    val inferred: Expr = infer(e)
    logger.debug(inferred)
    logger.debug(inferred.t)
    assert(inferred.t =~=
      expl((n: Nat) => (n `*.` n2dtFun(m => (m+1) `.` f32) ) ->: (n `*.` n2dtFun(m => f32) ))
    )
    function.asStringFromExpr(inferred)
  }


  ignore("List of list dot product") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(vector =>
      fun(n `.` NatType)(lengths =>
      fun(n `*.` (i => (lengths `#` i) `.` (f32 `x` IndexType(n))))(array => {
        depMapSeq(depFun((_: Nat) => fun(
          row =>
            reduceSeq(
              fun(x => fun(y => x + y))
            )(lf32(0.0f))(mapSeq(fun(entry => {
              val number = entry.`1`
              val index = entry.`2`
              number * (vector `@` index)
            }))(row))
        )
        ))(array)
      }
    ))))

    val inferred: Expr = infer(e)
    logger.debug(inferred)
    logger.debug(inferred.t)
    function.asStringFromExpr(inferred)
  }
}
