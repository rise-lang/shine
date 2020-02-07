package shine.DPIA

import rise.core.DSL._
import rise.core.types._
import shine.DPIA.Types.TypeCheck

class ReadWriteAnnotations extends test_util.Tests {

  test ("ExpTypes with different access annotations and the same DT are not equal.") {
    import shine.DPIA.Types._
    assert(ExpType(int, read) != ExpType(int, write))
  }

  test ("ExpTypes with same access annotations and different DTs are not equal.") {
    import shine.DPIA.Types._
    assert(ExpType(int, read) != ExpType(float, write))
  }

  test ("Use of access type subtyping types mapSeq kernel.") {
    val e =
      nFun(n => fun(ArrayType(n, f32))(xs =>
        xs |> mapSeq(fun(x => x))))

    val dpia_e = util.gen.toDPIA(e)
    TypeCheck(dpia_e)
  }

  test ("Use of access type subtyping types two nested mapSeqs kernel.") {
    val e =
      nFun(m => nFun(n => fun(ArrayType(m, ArrayType(n, f32)))(xs =>
        xs |> mapSeq(mapSeq(fun(x => x))))))

    val dpia_e = util.gen.toDPIA(e)
    TypeCheck(dpia_e)
  }

  test ("Use of access type subtyping types three nested mapSeqs kernel.") {
    val e =
      nFun(m => nFun(n => nFun(o => fun(ArrayType(m, ArrayType(n, ArrayType(o, f32))))(xs =>
        xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))))

    val dpia_e = util.gen.toDPIA(e)
    TypeCheck(dpia_e)
  }
}
