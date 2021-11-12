package apps

import apps.separableConvolution2D.mulT
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._

object mv {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = impl{ dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }
  val scalSeq = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }
  val dot = separableConvolution2D.dot
  val dotSeq = separableConvolution2D.dotSeq

  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |> map(mulT) |> reduce(add)(lf32(0.0f))
    ))
  ))

  val mvSequential = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> mapSeq(fun(row =>
      zip(row)(xs) |> mapSeq(mulT) |> reduceSeq(add)(lf32(0.0f))
    ))
  ))


  object ocl {
    import rise.openCL.DSL._
    import rise.openCL.primitives.{mapGlobal => _, mapLocal => _, mapWorkGroup => _, _}

    val mv = depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      mat |> mapGlobal(fun(row =>
        zip(row)(xs) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
      ))
    ))

    val mvFused = depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      mat |> mapWorkGroup(fun(row =>
        zip(row)(xs) |>
//          split(n) |> // why the split here? is this fast?  is the result even correct?
          // to local Fun()
//          mapLocal(
            oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
//          )
      ))
//      )) |> join
    ))

    val mvFusedSplit = depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      mat |> mapWorkGroup(fun(row =>
        zip(row)(xs) |>
          split(n) |> // why the split here? is this fast?  is the result even correct?
          // to local Fun()
          mapLocal(
            oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
          )
      )) |> join
    ))

    val mvBlastN = mvBlastNParam(64)

    val mvBlastT = mvBlastTParam(64)

    val mvFusedAMD = mvFusedAMDParam(128)

    val mvKeplerBest = mvKeplerBestParam(128)

    def mvBlastNParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
      )((mat, xs) =>
        join o mapWorkGroup(fun(matChunk => // matChunk: s0.(n.f32 x f32)
          mapLocal(fun(x => x)) o
            // TODO: check address space
            oclReduceSeq(AddressSpace.Private)(fun((acc, next) => // next: s0.s0.f32 x s0.f32
              let(toLocal(mapLocal(fun(x => x))(snd(next))))
                be (localX => // localX: s0.f32
                mapLocal(fun(x => // x: f32 x s0.f32
                  // TODO: check address space
                  oclReduceSeq(AddressSpace.Private)(fun((acc2, next2) => // next2: (f32 x f32)
                    acc2 + fst(next2) * snd(next2)
                  ))(fst(x)) $ zip(snd(x))(localX)
                )) $ zip(acc)(fst(next)))
            ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (s0 `.` f32))) $
            zip(transpose o map(split(s0)) $ matChunk)(split(s0) $ xs)
        )) o split(s0) $ mat
      ))
    }

    def mvBlastTParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (n `.` m `.` f32) ->: (n `.` f32) ->: (m `.` f32)
      )((mat, xs) =>
        mvBlastNParam(s0)(n)(m)(transpose(mat))(xs)
      ))
    }

    def mvFusedAMDParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
      )((mat, xs) =>
        mat
          |> mapWorkGroup(fun(row =>
          zip(row)(xs)
            |> reorderWithStride(s0)
            |> split(n /^ s0)
            |> mapLocal(
            oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
          )
            |> toLocal
            |> split(s0)
            |> mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
        ))
          |> join
      ))
    }

    def mvKeplerBestParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
      )((mat, xs) =>
        mat |>
          mapWorkGroup(fun(row =>
            zip(row)(xs) |>
              reorderWithStride(s0) |>
              split(n /^ s0) |>
              // toLocalFun() fails 'could not solve constraints'
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
              )
              |> toLocal // vs. toLocalFun()
              // toLocalFun() here
              |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
          ))
      ))
    }
  }
}
