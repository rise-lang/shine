package apps

import apps.mmTensor._
import rise.Cuda.DSL.{mapBlock, mapThreads, _}
import rise.Cuda.primitives.{asFragment, asMatrix, mapFragment}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.{AddressSpace, _}
import rise.core.types.DataType._
import rise.openCL.DSL.toPrivate
import rise.openCL.primitives.oclReduceSeq

//General Matrix Multiply (gemm) with tensor cores
//Multiply a m.k a-matrix with a k.n b-matrix and accumulate a m.n c-matrix
object gemmTensor {

  // Tiles m.n.dt matrix into a (m/mTile)*(n/nTile).mTile.nTile.dt
  def tiling2D(mTile: Nat, nTile: Nat): Expr =
    fun(c =>
      c |>
      split(mTile) |>
      map(fun(x =>
        x |>
        transpose |>
        split(nTile) |>
        map(fun(y =>
          y |>
          transpose)))) |>
      join)

  //Simple gemm with any matrix dimensions divisible by fragment sizes
  //Can be executed with any number of blocks and warps
  val simpleGemm: ToBeTyped[Expr] = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    //b-matrix is transposed
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      f32 ->: f32 ->: (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32) ->: (m `.` n `.` f32)
    )((alpha, beta, a, bT, c) =>
      zip
      (a |> split(mTileFrag))
      (c |> split(mTileFrag)) |> // m/mTileFrag.(mTileFrag.k.f16, mTileFrag.n.f32)
        mapBlock(0)(fun(aRowsC =>

          zip
          (bT |> split(nTileFrag))
          (aRowsC.`2` |> transpose |> split(nTileFrag)) |> // n/nTileFrag.(nTileFrag.k.f16, nTileFrag.mTileFrag.f32)
            mapWarp(0)(fun(bColumnsTCT =>

              zip
              (transpose(aRowsC.`1`) |> split(kTileFrag))
              (transpose(bColumnsTCT.`1`) |> split(kTileFrag)) |> // k/kTileFrag.(kTile.mTileFrag.f16 x kTile.nTile.f16)

                oclReduceSeq(AddressSpace.Private)(fun((cTile, abTiles) =>
                  tensorMMA(
                    abTiles.`1` |> transpose |> asFragment |> toPrivate,
                    abTiles.`2` |> asFragment |> toPrivate,
                    cTile)))

                (bColumnsTCT.`2` |>
                  transpose |>
                  asFragment |> toPrivate |>
                  mapFragment(fun(x => x * (beta / alpha)))) |>

                mapFragment(fun(x => x * alpha)) |> toPrivate |>
                asMatrix |> // mTileFrag.nTileFrag.f32

                transpose)) |> // n/nTileFrag.nTileFrag.mTileFrag.f32
            join |> // n.mTileFrag.f32
            transpose)) |> // m/mTileFrag.mTileFrag.n.f32
        join)) // m.n.f32
  }



  //Every Warp load 4 aMatrixFragments and 2 bMatrixFragments and calculate 8 MMAs per iteration to reduce number
  //of global memory accesses
  //Distrbuted over y dimension for m-dimension of matrix multiplication and x-dimension for n-dimension of mm
  //On Block level: Use mapWarp in x-dimension, but mapThreads in y-dimension!
  //Using x-dim = 2*32 and y-dim = 1*32 would start 2*32*32 threads = 64 warps...
  //Every block calculate a 128x128-tile (per iteration)
  //Every warp calculates a 64x32-tile to reduce number of global memory accesses
  //Matrix dimensions must be divisible by block-tile-size (128x128)
  val gemmMultipleFragmentsPerWarp: ToBeTyped[Expr] = {
    val mTileBlock = 128
    val nTileBlock = 128

    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    val mTileWarp = 4*mTileFrag
    val nTileWarp = 2*nTileFrag

    //b-matrix is transposed
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      f32 ->: f32 ->: (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32) ->: (m `.` n `.` f32)
    )((alpha, beta, a, bT, c) =>
      zip
      (a |> split(mTileBlock))
      (c |> split(mTileBlock)) |>
        mapBlock(1)(fun(aRowsBlockC => // (mTileBlock.k.f16, mTileBlock.n.f32)

          zip
          (bT |> split(nTileBlock))
          (aRowsBlockC.`2` |> transpose |> split(nTileBlock)) |>
            mapBlock(0)(fun(bColumnsTBlockCT => // (nTileBlock.k.f16, nTileBlock.mTileBlock.f32)

              zip
              (aRowsBlockC.`1` |> split(mTileWarp))
              (bColumnsTBlockCT.`2` |> transpose |> split(mTileWarp)) |>
                mapThreads(1)(fun(aRowsWarpC => // (mTileWarp.k.f16, mTileWarp.nTileBlock.f32)

                  zip
                  (bColumnsTBlockCT.`1` |> split(nTileWarp))
                  (aRowsWarpC.`2` |> transpose |> split(nTileWarp)) |>
                    mapWarp(0)(fun(bColumnsTWarpCT => // (nTileWarp.k.f16, nTileWarp.mTileWarp.f32)

                      zip
                      (aRowsWarpC.`1` |> transpose |> split(kTileFrag))
                      (bColumnsTWarpCT.`1` |> transpose |> split(kTileFrag)) |>
                        // k/kTileFrag.(kTileFrag.mTileWarp.f16, kTileFrag.nTileWarp.f16)

                        oclReduceSeq(AddressSpace.Private)(fun((cFrags, abTilesWarp) =>

                          //Load tiles of a-matrix into fragments
                          let(toPrivate(
                            abTilesWarp.`1` |> transpose |> split(mTileFrag) |>
                              mapSeqUnroll(fun(aFragTile =>
                                aFragTile |> asFragment))))
                            be(aFrags => // mTileWarp/mTileFrag.WmmaAMatrix

                            //Load tiles of b-matrix into fragments
                            let(toPrivate(
                              abTilesWarp.`2` |> transpose |> split(nTileFrag) |>
                                mapSeqUnroll(fun(bFragTileT =>
                                  bFragTileT |> transpose |> asFragment))))
                              be(bFrags => // nTileWarp/nTileFrag.WmmaBMatrix

                              //Do matrix multiplication and accumulate with tensor cores
                              zip(aFrags)(cFrags) |>
                                mapSeqUnroll(fun(acFrags =>
                                  zip(bFrags)(acFrags.`2`) |>
                                    mapSeqUnroll(fun(bcFrags =>
                                      tensorMMA(acFrags.`1`, bcFrags.`1`, bcFrags.`2`)))))))))

                        (bColumnsTWarpCT.`2` |> transpose |> split(mTileFrag) |>
                          mapSeq(fun(cTiles =>
                            cTiles |> transpose |> split(nTileFrag) |>
                              mapSeq(fun(cTileFragT =>
                                cTileFragT |> transpose |>
                                  asFragment |> toPrivate |>
                                  mapFragment(fun(x => x * (beta / alpha)))))))) |> // mTileWarp.nTileWarp.WmmaAcc

                        //Write result from fragments to output
                        mapSeqUnroll(fun(dTiles =>
                          dTiles |>
                            mapSeqUnroll(fun(dTiles =>
                              dTiles |>
                                mapFragment(fun(x => x * alpha)) |> toPrivate |>
                                asMatrix |>      // mTileFrag.nTileFrag.f32
                                transpose)) |>          // nTileWarp/nTileFrag.nTileFrag.mTileFrag.f32
                            join |>                   // nTileWarp.mTileFrag.f32
                            transpose)) |>            // mTileWarp/mTileFrag.mTileFrag.nTileWarp.f32
                        join |>                     // mTileWarp.nTileWarp.f32

                        transpose)) |>              // nTileBlock/nTileWarp.nTileWarp.mTileWarp.f32
                    join |>                       // nTileBlock.mTileWarp.f32
                    transpose)) |>                // mTileBlock/mTileWarp.mTileWarp.nTileBlock.f32
                join |>                         // mTileBlock.nTileBlock.f32
                transpose)) |>                  // n/nTileBlock.nTileBlock.mTileBlock.f32
            join |>                           // n.mTileBlock.f32
            transpose)) |>                    // m/mTileBlock.mTileBlock.n.f32
        join))                              // m.n.f32
  }



  //  var config: mmConfig = _

  //Example illustration how the kernel works
  //Example dimensions:
  //Every block calculates a 128*128 result-tile (per iteration)
  //Every warp calculate a 64*32 result-tile (per iteration)
  //Use exactly 8 warps so whole block-tile can be calculated
  //
  //
  //          Block-level (blockMM)                                                   Warp-level (warpMMA)
  //
  //               ________                                                                ________
  //              |        |                                                              |        |
  // Warp 0-3 ->  |        |                                                              |        |
  //              |        |                                                              |        |
  //              |________|                                                              |________|
  //              |        |
  // Warp 4-7 ->  |        |                                                        WarpTileAMatrix (in fragments)
  //              |        |
  //              |________|
  //
  //   BlockTileAMatrix (in shared memory)
  //
  //
  //              *                                                                         *
  //
  //
  //      Warp 0,4   Warp 2,6
  //         |          |
  //         | Warp 1,5 |  Warp 3,7
  //         |     |    |    |
  //       _____________________                                                          ______
  //       |    |    |    |    |                                                          |    |
  //       |    |    |    |    |                                                          |    |
  //       |    |    |    |    |                                                          |    |
  //       |____|____|____|____|                                                          |____|
  //
  //   BlockTileBMatrix (in shared memory)                                         WarpTileBMatrix (in fragments)
  //
  //
  //                 +=                                                                     +=
  //
  //
  //                   Warp 0    Warp 2
  //                     |         |
  // caluclated by       |  Warp 1 |  Warp 3
  //                     |    |    |    |
  //                   _____________________                                              ______
  //                   |    |    |    |    |                                              |    |
  //                   |    |    |    |    |                                              |    |
  //                   |    |    |    |    |                                              |    |
  //                   |____|____|____|____|                                              |____|
  //                   |    |    |    |    |
  //                   |    |    |    |    |                                      WarpResultTile (in fragments)
  //                   |    |    |    |    |
  //                   |____|____|____|____|                                        (same as in mm-Kernel)
  //
  //                     |    |    |    |
  // caluclated by       |  Warp 5 |  Warp 7
  //                     |         |
  //                   Warp 4    Warp 6
  //
  //            initial: (beta / alpha) * cMatrix loaded by 'loadMatrixFromCIntoFragments'
  //            from global memory into fragments
  //            resultTile (cMatrix is overwritten by intermediate result)
  //            (in fragments distributed over differtent warps!)
  //
  //                            |
  //                            |  epilog
  //                            V
  //
  //            alpha * resultTile stored in global memory

  //Block-level (executed by a single thread block)
  //MMA-instruction with mTileBlock rows of a-matrix, nTileBlock columns of b-matrix and a mTileBlock.nTileBlock tile
  //of c-matrix
  //Calculate gemm as ((beta/alpha) * cTileBlock + aTileBlock * bTileBlock) * alpha (larger rounding errors possible)
  //loadMatrixFromCIntoFragments: Load matrix elements from c-matrix (global memory) into fragments and scale matrix
  //elements with factor 'scalar'
  //epilog: Load matrix elements from fragments which are distributed over different warps (into global memory)
  //Result: mTileBlock.nTileBlock.f32
  def blockMMA(loadMatrixFromCIntoFragments: ToBeTyped[Expr], epilog: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    fun((alpha, beta, aRowsBlock, bColumnsBlock, cTileBlock) =>
      zip
      (aRowsBlock |> transpose |> split(config.kTileBlock))
      (bColumnsBlock |> split(config.kTileBlock)) |> // k/kTileBlock.(kTileBlock.mTileBlock.f16, kTileBlock.nTileBlock.f16)

        oclReduceSeq(AddressSpace.Private)(fun((cFragsBlock, aTbTileBlock) =>

          //Load aTile and bTile to shared memory
          let(aTbTileBlock.`1` |>
            transpose |>
            copyMatrix(config.mTileBlock, config.kTileBlock, 8) |>
            toSharedWithPadding(config.kTileBlock, 8))
            be(aTile =>

            //Load bTile transposed to shared memory
            let(aTbTileBlock.`2` |>
              transpose |>
              copyMatrix(config.nTileBlock, config.kTileBlock, 8) |>
              toSharedWithPadding(config.kTileBlock, 8))
              be(bTileT =>

              zip
              (crossProductOfMatrixTiles(config.mTileWarp, config.nTileWarp)(aTile, bTileT |> transpose))
              (cFragsBlock) |>

                mapWarp(fun(abWarpC =>
                  warpMMA(
                    abWarpC.`1`.`1`,
                    abWarpC.`1`.`2`,
                    abWarpC.`2` |> split(config.nNumberOfFragsWarp)) |>

                    mapSeq(mapSeq(fun(x => x))))) |>
                join |>
                join |>
                split(config.mNumberOfFragsWarp * config.nNumberOfFragsWarp)))))

        (loadMatrixFromCIntoFragments(beta / alpha)(cTileBlock)) |>

        epilog(alpha))
  }

  //Load matrix elements from c-matrix (global memory) into fragments and scale matrix elments with factor 'scalar'
  //Simply use toFragment-primitiv and mapFragmentElemnts to load from global memory into fragments
  private def loadMatrixFromCIntoFragments: ToBeTyped[Expr] =
    fun((factor, cTileBlock) =>
      cTileBlock |>
        tiling2D(config.mTileWarp, config.nTileWarp) |>
        mapWarp(fun(warpTile =>

          warpTile |>
            tiling2D(config.mTileFrag, config.nTileFrag) |>
            mapSeqUnroll(fun(cTileFrag =>

              cTileFrag |>
                asFragment |>
                mapFragment(fun(x => x * factor)))))))


  //Load matrix elements from c-matrix (global memory) into fragments and scale matrix elments with factor 'scalar'
  //avoiding bank conflicts and allow more flexible tiling sizes
  private def loadMatrixFromCIntoFragmentsV2: ToBeTyped[Expr] = {
    //Number of fragments that fit into shared memory and is divisible by numberOfWarps
    val fragmentsPerIteration = 32

    //Number of fragments that store a single warp into shared memory
    val fragmentsPerIterationPerWarp = fragmentsPerIteration / config.numberOfWarps.eval
    //the fragments within a single warp is in a 2D formation
    val mNumberOfFragmentsPerIteration = fragmentsPerIterationPerWarp / config.nNumberOfFragsWarp
    //number of rows that can be stored in global memory in a single iteration
    val matrixMDimensionPerIteration = mNumberOfFragmentsPerIteration * config.mTileFrag * config.mNumberOfWarps

    fun((factor, cTileBlock) =>
      cTileBlock |>
        split(config.mTileBlock /^ config.mNumberOfWarps) |>
        map(fun(x =>
          x |>
            split(mNumberOfFragmentsPerIteration * config.mTileFrag))) |>
        transpose |>

        mapSeqUnroll(fun(tilePerIteration =>
          tilePerIteration |>
            join |>
            copyMatrix(matrixMDimensionPerIteration, config.nTileBlock, 4) |>
            toSharedWithPadding(config.nTileBlock, 4) |>

            tiling2D(config.mTileFrag * mNumberOfFragmentsPerIteration, config.nTileWarp) |>
            mapWarp(fun(warpTilePerIteration =>
              warpTilePerIteration |>
                tiling2D(config.mTileFrag, config.nTileFrag) |>
                mapSeqUnroll(fun(frag =>
                  frag |>
                    asFragment |>
                    mapFragment(fun(x => x * factor)))))) |>
            transpose)) |>

        join |>
        transpose)
  }


  //Load matrix elements from fragments which are distributed over different warps (into global memory)
  //Simply use fromFragment-primitiv to load from fragments into global memory
  //Before store data in global memory: scale matrix with alpha using mapFragmentElements
  private def epilog: ToBeTyped[Expr] =
    fun((alpha, resultFragsBlock) =>
      resultFragsBlock |>
        mapWarp(fun(resultFragsWarp =>

          //Result from a single warp
          resultFragsWarp |>
            mapSeqUnroll(fun(resultFrag =>
              resultFrag |>
                mapFragment(fun(x => x * alpha)) |> toPrivate |>
                asMatrix |>             // mTileFrag.nTileFrag.f32
                transpose)) |>              // (mTileWarp/mTileFrag)*(nTileWarp/nTileFrag).nTileFrag.mTileFrag.f32
            join |>                       // (mTileWarp/mTileFrag)*nTileWarp.mTileFrag.f32
            split(config.nTileWarp) |>
            map(fun(x =>
              x |> transpose)) |>
            join |>
            transpose)) |>                 // (mTileBlock/mTileWarp)*(nTileBlock/nTileWarp).nTileWarp.mTileWarp.f32

        join |>                          // (mTileBlock/mTileWarp)*nTileBlock.mTileWarp.f32
        split(config.nTileBlock) |>      // (mTileBlock/mTileWarp).nTileBlock.mTileWarp.f32
        map(fun(x =>
          x |> transpose)) |>            // (mTileBlock/mTileWarp).mTileWarp.nTileBlock.f32
        join)                            // mTilBlock.nTileBlock.f32


  //First store date to shared memory avoiding bank conflicts and then coalesced to global memory
  //Complete result must NOT fit into shared memory (multple iterations possible)
  //Before store data in global memory: scale matrix with alpha using mapFragmentElements
  private def epilogV2: ToBeTyped[Expr] = {
    //Number of fragments that fit into shared memory and is divisible by numberOfWarps
    val fragmentsPerIteration = 32

    //Number of fragments that store a single warp into shared memory
    val fragmentsPerIterationPerWarp = fragmentsPerIteration / config.numberOfWarps.eval
    //the fragments within a single warp is in a 2D formation
    val mNumberOfFragmentsPerIteration = fragmentsPerIterationPerWarp / config.nNumberOfFragsWarp
    //number of rows that can be stored in global memory in a single iteration
    val matrixMDimensionPerIteration = mNumberOfFragmentsPerIteration * config.mTileFrag * config.mNumberOfWarps

    fun((alpha, resultFragsBlock) =>
      let(resultFragsBlock)
        //First write result into fragments, so the scope of the allocated shared memory for aBlockTile and bBlockTile ends
        be(resultFragsBlock =>

        resultFragsBlock |>
          transpose |>
          split(fragmentsPerIterationPerWarp) |>
          mapSeqUnroll(fun(resultFragsT =>
            resultFragsT |>
              transpose |>
              mapWarp(fun(fragsTile =>
                //Result from a single warp
                fragsTile |>
                  mapSeqUnroll(fun(resultFrag =>
                    resultFrag |>
                      mapFragment(fun(x => x * alpha)) |> toPrivate |>
                      asMatrix |>
                      transpose)) |>
                  join |>
                  split(config.nTileWarp) |>
                  map(fun(x =>
                    x |> transpose)) |>
                  join |>
                  transpose)) |>

              join |>
              split(config.nTileBlock) |>
              map(fun(x =>
                x |> transpose)) |>
              join |>

              //Write this result to shared memory
              toSharedWithPadding(config.nTileBlock, 4) |>

              //And then coalesced to global memory
              copyMatrix(matrixMDimensionPerIteration, config.nTileBlock, 4) |>

              split(config.mTileFrag * mNumberOfFragmentsPerIteration))) |>
          transpose |>
          join |>
          join))
  }


  //Calculate alpha * (m.k.f16 a-matrix * k.n.f16 b-matrix) and accumulate beta * m.n.f32 matrix
  //Result: m.n.f32 matrix
  //b-matrix is transposed
  private def deviceGEMM(blockGEMM: ToBeTyped[Expr]): ToBeTyped[Expr] =
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      f32 ->: f32 ->: (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32) ->: (m `.` n `.` f32)
    )((alpha, beta, a, bT, c) =>
      zip
      (crossProductOfMatrixTiles(config.mTileBlock, config.nTileBlock)(a, bT |> transpose))
      (c |> tiling2D(config.mTileBlock, config.nTileBlock)) |>
      mapBlock(fun(aRowsBlockBColumnBlockCTileBlock =>

        blockGEMM(
          alpha, beta,
          aRowsBlockBColumnBlockCTileBlock.`1`.`1`, //aRowsBlockBColumnBlockCTileBlock.`1`.`1`
          aRowsBlockBColumnBlockCTileBlock.`1`.`2`, //aRowsBlockBColumnBlockCTileBlock.`1`.`2`
          aRowsBlockBColumnBlockCTileBlock.`2`) |>     //mTileBlock.nTilcblock.f32
            transpose)) |>                                //m/mTileBlock*n/nTileBlock.nTileBlock.mTilcblock.f32
        join |>                                         //m/mTileBlock*n.mTilcblock.f32
        split(n) |>                                     //m/mTileBlock.n.mTilcblock.f32
        map(fun(x =>
          x |> transpose)) |>                           //m/mTileBlock.mTilcblock.n.f32
        join))                                          //m.n.f32


  //Kernel for gemm using shared memory
  def gemmSharedMemory(config: mmConfig): ToBeTyped[Expr] = {
    mmTensor.config = config
    deviceGEMM(blockMMA(loadMatrixFromCIntoFragments, epilog))
  }

  //More optimzed kernel for gemm using shared memory
  def gemmSharedMemoryV2(config: mmConfig): ToBeTyped[Expr] = {
    mmTensor.config = config
    deviceGEMM(blockMMA(loadMatrixFromCIntoFragmentsV2, epilogV2))
  }
}
