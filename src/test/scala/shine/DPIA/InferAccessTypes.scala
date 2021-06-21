package shine.DPIA

import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.{types => rt}
import shine.DPIA.Types._

class InferAccessTypes extends test_util.Tests {

  ignore("(read -> read) is inferred for id over float.") {
    //TODO decide whether an expression should be typing if it doesn't output
    // write
    val id = (fun(rt.f32)(x => x)).toExpr
    val infPt = inferAccess(id).get(id)
    val expecPt = FunType(ExpType(f32, read), ExpType(f32, read))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) is inferred for id over 8.f32.") {
    //TODO decide whether an expression should be typing if it doesn't output
    // write
    val idArr8f32CopyUnspec = (fun(8`.`rt.f32)(x => x)).toExpr

    assertThrows[Exception](inferAccess(idArr8f32CopyUnspec))
  }

  test("(read -> write) is inferred for id over array with mapSeq") {
    val idArr = (fun(8`.`rt.f32)(
      x => x |> mapSeq(fun(x => x)))).toExpr
    val infPt = inferAccess(idArr).get(idArr)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) is inferred for id over array with mapSeq") {
    //TODO answer question: can we generate code for this case?
    val idArr = (fun(8`.`rt.f32)(x => x |> map(fun(x => x)))).toExpr
    val infPt = inferAccess(idArr).get(idArr)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), read))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with map(id) after mapSeq") {
    val idWithMapSeqAndMap = (fun(8`.`rt.f32)(
        x => x |> mapSeq(fun(x => x)) |> map(fun(x => x)))).toExpr
    val infPt = inferAccess(idWithMapSeqAndMap).get(idWithMapSeqAndMap)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), write))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with transpose after mapSeq(mapSeq)") {
    val transpMapSeqOutput = (fun(8`.`4`.`rt.f32)(
        x => x |> mapSeq(mapSeq(fun(x => x))) |> transpose)).toExpr
    val infPt = inferAccess(transpMapSeqOutput).get(transpMapSeqOutput)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(4, f32)), read),
      ExpType(ArrayType(4, ArrayType(8, f32)), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with map(transpose) on input") {
    val mapTransp = (fun(8`.`8`.`4`.`rt.f32)(x => x |> map(transpose))).toExpr
    val infPt = inferAccess(mapTransp).get(mapTransp)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(8, ArrayType(4, f32))), read),
      ExpType(ArrayType(8, ArrayType(4, ArrayType(8, f32))), read))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with map(transpose) after mapSeq(mapSeq)") {
    val mapTranspAfterMapSeqs = (fun(8`.`8`.`4`.`rt.f32)(
      x => x |> mapSeq(mapSeq(mapSeq(fun(x => x)))) |> map(transpose))).toExpr
    val infPt = inferAccess(mapTranspAfterMapSeqs).get(mapTranspAfterMapSeqs)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(8, ArrayType(4, f32))), read),
      ExpType(ArrayType(8, ArrayType(4, ArrayType(8, f32))), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with toMem after mapSeq") {
    val copyArrIntoIntermediary = (fun(8`.`rt.f32)(
      x => x |> mapSeq(fun(x => x)) |> toMem)).toExpr
    val infPt = inferAccess(copyArrIntoIntermediary).get(copyArrIntoIntermediary)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), read))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with split on input") {
    val splitArray = (depFun((n: Nat) => fun(8`.`rt.f32)(arr =>
      arr |> split(n)))).toExpr
    val infPt = inferAccess(splitArray).get(splitArray).asInstanceOf[
      DepFunType[NatIdentifier, Kind.INat, FunType[ExpType, ExpType]]
    ]
    assertResult(read)(infPt.t.outT.accessType)
  }
}
