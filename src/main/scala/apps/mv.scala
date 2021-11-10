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
          split(n) |> // why the split here? is this fast?  is the result even correct?
          // to local Fun()
          mapLocal(
          oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
          )
      )) |> join
    ))

//    def mvBlastNParam(s0: Nat): ToBeTyped[Expr] = {

      val mvBlastN =
      depFun((n: Nat, m: Nat) => fun(
        (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
      )((mat, xs) =>
        join o mapWorkGroup(fun(matChunk => // matChunk: 64.(n.f32 x f32)
          mapLocal(fun( x => x)) o
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
          ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (64`.`f32))) $
            zip(transpose o map(split(64)) $ matChunk)(split(64) $ xs)
        )) o split(64) $ mat
      ))


    // something is broken here
//      val mvBlastN: ToBeTyped[Expr] =
//      depFun((n: Nat, m: Nat) => fun(
//        (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
//          (m`.`f32)
//      )((mat, xs) =>
//        join o mapWorkGroup(fun(matChunk => // matChunk: 64.(n.f32 x f32)
//          // TODO: check address space
//          oclReduceSeq(AddressSpace.Private)(fun((acc, next) => // next: 64.64.f32 x 64.f32
//            let (toLocal(mapLocal(fun(x => x))(snd(next))))
//              be (localX => // localX: 64.f32
//              mapLocal(fun(x => // x: f32 x 64.f32
//                // TODO: check address space
//                oclReduceSeq(AddressSpace.Private)(fun((acc2, next2) => // next2: (f32 x f32)
//                  acc2 + fst(next2) * snd(next2)
//                ))(fst(x)) $ zip(snd(x))(localX)
//              )) $ zip(acc)(fst(next)))
//          ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (64`.`f32))) $
//            zip(transpose o map(split(64)) $ matChunk)(split(64) $ xs)
//        )) o split(64) $ mat
//      ))

//    println("mvBlastN: \n" + mvBlastN)
//
//    println("try to generate code")
//    val code = util.gen.opencl.kernel.asStringFromExpr(mvBlastN)
//    println("code: \n" + code)

//    val mvBlastN = mvBlastNParam(64)
//
//    val mvBlastT = depFun((n: Nat, m: Nat) => fun(
//      (n`.`m`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
//        (m`.`f32)
//    )((mat, xs, ys, alpha, beta) =>
//      mvBlastN(n)(m)(transpose(mat))(xs)(ys)(alpha)(beta)
//    ))

//    def mvBlastTParam(s0: Nat): ToBeTyped[Expr] = {
//      depFun((n: Nat, m: Nat) => fun(
//        (n`.`m`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
//          (m`.`f32)
//      )((mat, xs, ys, alpha, beta) =>
//        mvBlastNParam(s0)(n)(m)(transpose(mat))(xs)(ys)(alpha)(beta)
//      ))
//    }


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

    val mvFusedAMD = mvFusedAMDParam(128)

    val mvKeplerBest = mvKeplerBestParam(128)

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
