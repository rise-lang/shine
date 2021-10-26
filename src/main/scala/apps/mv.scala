package apps

import apps.gemv.{add, mult}
import apps.separableConvolution2D.mulT
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.primitives.oclReduceSeq

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

//  val gemvHighLevel = depFun((n: Nat, m: Nat) => fun(
//    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
//      (m`.`f32)
//  )((mat, xs, ys, alpha, beta) =>
//    zip(map(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta)) |>
//    map(fun(x => x._1 + x._2))
//  ))

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

    def mvBlastNParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
          (m`.`f32)
      )((mat, xs) =>
        join o mapWorkGroup(fun(matChunk => // matChunk: 64.(n.f32 x f32)
          // TODO: check address space
          oclReduceSeq(AddressSpace.Private)(fun((acc, next) => // next: 64.64.f32 x 64.f32
            let (toLocal(mapLocal(fun(x => x))(snd(next))))
              be (localX => // localX: 64.f32
              mapLocal(fun(x => // x: f32 x 64.f32
                // TODO: check address space
                oclReduceSeq(AddressSpace.Private)(fun((acc2, next2) => // next2: (f32 x f32)
                  acc2 + fst(next2) * snd(next2)
                ))(fst(x)) $ zip(snd(x))(localX)
              )) $ zip(acc)(fst(next)))
          ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (s0`.`f32))) $
            zip(transpose o map(split(s0)) $ matChunk)(split(s0) $ xs)
        )) o split(s0) $ mat
      ))
    }

    val mvBlastN = mvBlastNParam(64)

    val mvBlastT = depFun((n: Nat, m: Nat) => fun(
      (n`.`m`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      mvBlastN(n)(m)(transpose(mat))(xs)(ys)(alpha)(beta)
    ))

    def mvBlastTParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (n`.`m`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
          (m`.`f32)
      )((mat, xs, ys, alpha, beta) =>
        mvBlastNParam(s0)(n)(m)(transpose(mat))(xs)(ys)(alpha)(beta)
      ))
    }

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
      mat |> mapGlobal(fun(row =>
        zip(row)(xs) |>
          // to local Fun()
          oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
      ))
    ))

    val gemvFused: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
        mapWorkGroup(fun(t =>
          zip(xs)(t._1) |>
            split(n) |>
            toLocalFun(mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
            )) |>
            mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
        )) |> join
    ))

    def mvFusedAMDParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->: (m`.`f32)
      )((mat, xs) =>
        mat |>
          mapWorkGroup(fun(t =>
            zip(xs)(t) |>
              reorderWithStride(s0) |>
              split(n /^ s0) |>
              toLocalFun(mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
              )) |>
              split(s0) |>
              toLocalFun(mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))))
            //              mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
          )) |> join
      ))
    }

    val mvFusedAMD = mvFusedAMDParam(128)

    def mvKeplerBestHighLevel(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32)
      )((mat, xs) =>
        mat |>
          map(fun(row =>
            zip(xs)(row) |>
              //              reorderWithStride(s0) |>
              split(n /^ s0) |>
              map(
                // alternative implementation
                 map(mulT) |> reduce(add)(lf32(0.0f))

                // copy result somewhere?
//                reduce(fun(a => fun(x => mult(x) + a)))(lf32(0.0f)) // reduce inside chunk
              )
              |>
              // copy result somewhere?
              reduce(add)(lf32(0.0f)) // reduce chunks
          ))
      ))
    }


    def mvKeplerBestParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32)
      )((mat, xs) =>
        mat |>
          mapWorkGroup(fun(row =>
            zip(xs)(row) |>
              //              reorderWithStride(s0) |>
              split(n /^ s0) |>
              mapLocal(
                // alternative implementation
                map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))

                // copy result somewhere?
                //                reduce(fun(a => fun(x => mult(x) + a)))(lf32(0.0f)) // reduce inside chunk
              )
              |>
              // copy result somewhere?
              oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)) // reduce chunks
          ))
      ))
    }

    def gemvKeplerBestParam(s0: Nat): ToBeTyped[Expr] = {
      depFun((n: Nat, m: Nat) => fun(
        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
          (m`.`f32)
      )((mat, xs, ys, alpha, beta) =>
        zip(mat)(ys) |>
          mapWorkGroup(fun(t =>
            zip(xs)(t._1) |>
              //              reorderWithStride(s0) |>
              split(n /^ s0) |>
              toLocalFun(mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
              )) |>
              toLocalFun(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))) |>
              fun(x => (alpha * x) + (t._2 * beta))
          ))
      ))
    }

    val mvKeplerBest = mvKeplerBestParam(128)
  }
}
