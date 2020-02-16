package shine.DPIA

import arithexpr.arithmetic.ArithExpr
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.{core => r}
import rise.core.{types => rt}
import shine.C.AST.DefaultImplementations.ArithmeticExpr
import shine.DPIA.Types._
import shine.DPIA.{fromRise => fr}

class InferAccessTypes extends test_util.Tests {
  //TODO decide whether an expression should be typing if it doesn't output
  // write
  ignore("(read -> read) is inferred for id over float.") {
    val id = rt.infer(fun(rt.f32)(x => x))
    val infPt = inferAccess(id)(id)
    val expecPt = FunType(ExpType(f32, read), ExpType(f32, read))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) is inferred for id over 8.f32.") {
    //TODO decide whether an expression should be typing if it doesn't output
    // write
    val idArr8f32CopyUnspec = rt.infer(fun(8`.`rt.f32)(x => x))

    assertThrows[Exception](inferAccess(idArr8f32CopyUnspec))
  }

  test("(read -> write) is inferred for id over array with mapSeq") {
    val idArr = rt.infer(fun(8`.`rt.f32)(
      x => x |> mapSeq(fun(x => x))))
    val infPt = inferAccess(idArr)(idArr)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) is inferred for id over array with mapSeq") {
    //TODO answer question: can we generate code for this case?
    val idArr = rt.infer(fun(8`.`rt.f32)(x => x |> map(fun(x => x))))
    val infPt = inferAccess(idArr)(idArr)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), read))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with map(id) after mapSeq") {
    val idWithMapSeqAndMap = rt.infer(fun(8`.`rt.f32)(
        x => x |> mapSeq(fun(x => x)) |> map(fun(x => x))))
    val infPt = inferAccess(idWithMapSeqAndMap)(idWithMapSeqAndMap)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), write))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with transpose after mapSeq(mapSeq)") {
    val transpMapSeqOutput = rt.infer(fun(8`.`4`.`rt.f32)(
        x => x |> mapSeq(mapSeq(fun(x => x))) |> transpose))
    val infPt = inferAccess(transpMapSeqOutput)(transpMapSeqOutput)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(4, f32)), read),
      ExpType(ArrayType(4, ArrayType(8, f32)), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with map(transpose) on input") {
    val mapTransp = rt.infer(fun(8`.`8`.`4`.`rt.f32)(x => x |> map(transpose)))
    val infPt = inferAccess(mapTransp)(mapTransp)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(8, ArrayType(4, f32))), read),
      ExpType(ArrayType(8, ArrayType(4, ArrayType(8, f32))), read))

    assertResult(expecPt)(infPt)
  }

  test("(read -> write) with map(transpose) after mapSeq(mapSeq)") {
    val mapTranspAfterMapSeqs = rt.infer(fun(8`.`8`.`4`.`rt.f32)(
      x => x |> mapSeq(mapSeq(mapSeq(fun(x => x)))) |> map(transpose)))
    val infPt = inferAccess(mapTranspAfterMapSeqs)(mapTranspAfterMapSeqs)
    val expecPt = FunType(
      ExpType(ArrayType(8, ArrayType(8, ArrayType(4, f32))), read),
      ExpType(ArrayType(8, ArrayType(4, ArrayType(8, f32))), write))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with toMem after mapSeq") {
    val copyArrIntoIntermediary = rt.infer(fun(8`.`rt.f32)(
      x => x |> mapSeq(fun(x => x)) |> toMem))
    val infPt = inferAccess(copyArrIntoIntermediary)(copyArrIntoIntermediary)
    val expecPt = FunType(
      ExpType(ArrayType(8, f32), read), ExpType(ArrayType(8, f32), read))

    assertResult(expecPt)(infPt)
  }

  ignore("(read -> read) with split on input") {
    val splitArray = rt.infer(nFun(n => fun(8`.`rt.f32)(arr =>
      arr |> split(n))))
    val infPt = inferAccess(splitArray)(splitArray).asInstanceOf[
      DepFunType[NatKind, FunType[ExpType, ExpType]]
    ]
    assertResult(read)(infPt.t.outT.accessType)
  }
}
