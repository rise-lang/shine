package apps

import rise.Cuda.DSL.{mapBlock, mapThreads, _}
import rise.Cuda.primitives.{asFragment, asMatrix, generateFragment}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.{AddressSpace, _}
import rise.core.types.DataType._
import rise.openCL.DSL.{toLocal, toPrivate}
import rise.openCL.primitives.{oclReduceSeq, oclReduceSeqUnroll}

//Matrixmultiplications (mm) with tensor cores
//Multiply a m.k a-matrix with a k.n b-matrix
object mmTensor {

  def id: ToBeTyped[Expr] = fun(x => x)
  def generate2D: ToBeTyped[Expr] = generate(fun(_ => generate(fun(_ => lf32(0f)))))

  val simpleMatMulWithoutTensorCores: Expr = {
    val dotproduct =
      fun((aRow, bColumn) =>
        zip(aRow)(bColumn) |>
          oclReduceSeq(AddressSpace.Private)(fun((n, abPair) =>
            abPair.`1` * abPair.`2` + n))
          (lf32(0.0f)))

    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f32) ->: (k `.` n `.` f32) ->: (m `.` n `.` f32)
    )((a, b) =>
      a |>
        mapBlock(fun(aRow =>

          b |>
            transpose |>
            mapThreads(fun(bColumn =>

              dotproduct(aRow, bColumn)))))))
  }

  //Matrix multiplication with a single fragment (e.g. 16x16-tile)
  //Can only executed by a single warp calculates a single tensor-Core-MMA-instuction
  val simpleMatMulTile: Expr = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    fun(
      (mTileFrag `.` kTileFrag `.` f16) ->: (kTileFrag `.` nTileFrag `.` f16) ->: (mTileFrag `.` nTileFrag `.` f32)
    )((a, b) =>
      tensorMMA(
        a |> asFragment |> toPrivate,
        b |> asFragment |> toPrivate,
        lf32(0f) |> generateFragment |> toPrivate)
        |> toPrivate

        |> asMatrix
    )
  }


  //Simple matrix multiplication with any matrix dimensions divisible by fragment sizes
  //Can be executed with any number of blocks and warps
  val simpleMatMul: Expr = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (k `.` n `.` f16) ->: (m `.` n `.` f32)
    )((a, b) =>
      a |> split(mTileFrag) |> // m/mTileFrag.mTileFrag.k.f16
      mapBlock(0)(fun(aRows =>

        b |> transpose |> split(nTileFrag) |> // n/nTileFrag.nTileFrag.k.f16
        mapWarp(0)(fun(bColumnsT =>

          zip
           (transpose(aRows) |> split(kTileFrag))
           (transpose(bColumnsT) |> split(kTileFrag)) |> // k/kTileFrag.(kTileFrag.mTileFrag.f16 x kTileFrag.nTileFrag.f16)

            //Multiply mTileFrag rows of a-matrix with nTileFrag columns of b-matrix
            oclReduceSeq(AddressSpace.Private)(fun((cTile, abTiles) =>
              tensorMMA(
                abTiles.`1` |> transpose |> asFragment |> toPrivate,
                abTiles.`2` |> asFragment |> toPrivate,
                cTile)))
            (generateFragment(lf32(0f))) |>

            asMatrix |> // mTileFrag.nTileFrag.f32

            transpose)) |> // n/nTileFrag.nTileFrag.mTileFrag.f32
          join |> // n.mTileFrag.f32
          transpose)) |> // m/mTileFrag.mTileFrag.n.f32
        join)) // m.n.f32
  }


  //Same as simpleMatMul just pass matrix b transposed to this kernel for coalesced global memory access
  val simpleMatMulBMatrixTransposed: Expr = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    //b-matrix is transposed
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32)
    )((a, bT) =>
      a |> split(mTileFrag) |> // m/mTileFrag.mTileFrag.k.f16
        mapBlock(0)(fun(aRows =>

          bT |> split(nTileFrag) |> // n/nTileFrag.nTileFrag.k.f16
            mapWarp(0)(fun(bColumnsT =>

              zip
                (transpose(aRows) |> split(kTileFrag))
                (transpose(bColumnsT) |> split(kTileFrag)) |> // k/kTileFrag.(kTileFrag.mTileFrag.f16 x kTileFrag.nTileFrag.f16)

              //Multiply mTileFrag rows of a-matrix with nTileFrag columns of b-matrix
              oclReduceSeq(AddressSpace.Private)(fun((cTile, abTiles) =>
                tensorMMA(
                  abTiles.`1` |> transpose |> asFragment |> toPrivate,
                  abTiles.`2` |> asFragment |> toPrivate, // leading dimension is k cause the b matrix is transposed!
                  cTile)))
              (generateFragment(lf32(0f))) |>

              asMatrix |> // mTileFrag.nTileFrag.f32

              transpose)) |> // n/nTileFrag.nTileFrag.mTileFrag.f32
            join |> // n.mTileFrag.f32
            transpose)) |> // m/mTileFrag.mTileFrag.n.f32
        join)) // m.n.f32
  }


  //Same as simpleMatMul just other loop order for coalesced global memory access
  val simpleMatMulLoopsSwaped: Expr = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (k `.` n `.` f16) ->: (m `.` n `.` f32)
    )((a, b) =>
      b |> transpose |> split(nTileFrag) |> // n/nTileFrag.nTileFrag.k.f16
        mapBlock(0)(fun(bColumnsT =>

          a |> split(mTileFrag) |> // m/mTileFrag.mTileFrag.k.f16
            mapWarp(0)(fun(aRows =>

              zip
                (transpose(aRows) |> split(kTileFrag))
                (transpose(bColumnsT) |> split(kTileFrag)) |> // k/kTileFrag.(kTileFrag.mTileFrag.f16 x kTileFrag.nTileFrag.f16)

              //Multiply mTileFrag rows of a-matrix with nTileFrag columns of b-matrix
              oclReduceSeq(AddressSpace.Private)(fun((cTile, abTiles) =>
                tensorMMA(
                  abTiles.`1` |> transpose |> asFragment |> toPrivate,
                  abTiles.`2` |> asFragment |> toPrivate,
                  cTile)))
              (generateFragment(lf32(0f))) |>

              asMatrix)) |> // m/mTileFrag.mTileFrag.nTileFrag.f32

            join |> // m.mTileFrag.f32
            transpose)) |> // n/nTileFrag.nTileFrag.m.f32
        join |> //n.m.f32
        transpose)) // m.n.f32
  }


  //Every Warp load 4 aMatrixFragments and 2 bMatrixFragments and calculate 8 MMAs per iteration to reduce number
  //of global memory accesses
  //Distrbuted over y dimension for m-dimension of matrix multiplication and x-dimension for n-dimension of mm
  //On Block level: Use mapWarp in x-dimension, but mapThreads in y-dimension!
  //Using x-dim = 2*32 and y-dim = 1*32 would start 2*32*32 threads = 64 warps...
  //Every block calculate a 128x128-tile (per iteration)
  //Every warp calculates a 64x32-tile to reduce number of global memory accesses
  //Matrix dimensions must be divisible by block-tile-size (128x128)
  def matMulMultipleFragmentsPerWarp(mTileWarp: Int = 64, nTileWarp: Int = 32, mTileBlock: Int = 128, nTileBlock: Int = 128): Expr = {
    assert(mTileBlock % mTileWarp == 0)
    assert(nTileBlock % nTileWarp == 0)

    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16


    //b-matrix is transposed
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32)
    )((a, bT) =>
      a |> split(mTileBlock) |>
      mapBlock(1)(fun(aRowsBlock => // mTileBlock.k.f16

        bT |> split(nTileBlock) |>
        mapBlock(0)(fun(bColumnsTBlock => // nTileBlock.k.f16

          aRowsBlock |> split(mTileWarp) |>
          mapThreads(1)(fun(aRowsWarp => // mTileWarp.k.f16

            bColumnsTBlock |> split(nTileWarp) |>
            mapWarp(0)(fun(bColumnsTWarp => // nTileWarp.k.f16

              zip
                (aRowsWarp |> transpose |> split(kTileFrag))
                (bColumnsTWarp |> transpose |> split(kTileFrag)) |> // k/kTileFrag.(kTileFrag.mTileWarp.f16, kTileFrag.nTileWarp.f16)

              //Multiply mTileWarp rows of a-matrix with nTileWarp columns of b-matrix
              oclReduceSeq(AddressSpace.Private)(fun((cFrags, abTilesWarp) =>

                //Load tile of a-matrix into multiple fragments
                let(toPrivate(
                  abTilesWarp.`1` |> transpose |> split(mTileFrag) |>
                    mapSeqUnroll(fun(aFragTile =>
                      aFragTile |> asFragment))))
                be(aFrags => // mTileWarp/mTileFrag.WmmaAMatrix

                  //Load tile of b-matrix into multiple fragments
                  let(toPrivate(
                    abTilesWarp.`2` |> transpose |> split(nTileFrag) |>
                      mapSeqUnroll(fun(bFragTileT =>
                        bFragTileT |> transpose |> asFragment))))
                  be(bFrags => // nTileWarp/nTileFrag.WmmaBMatrix

                    //Do MMA instructions with tensor cores
                    zip(aFrags)(cFrags) |>
                    mapSeqUnroll(fun(acFrags =>
                      zip(bFrags)(acFrags.`2`) |>
                      mapSeqUnroll(fun(bcFrags =>
                        tensorMMA(acFrags.`1`, bcFrags.`1`, bcFrags.`2`)))))))))

              (generate2D |>
                mapSeq(
                  mapSeq(fun(_ =>
                    generateFragment(lf32(0f)))))) |> // mTileWarp/mTileFrag.nTileWarp/nTileFrag.WmmaAccumulator

              //Write result from fragments to output
              mapSeqUnroll(fun(cTiles =>
                cTiles |>
                mapSeqUnroll(fun(cTile =>
                  cTile |>
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


  //First attempt to use shared memory
  //Similar to matMulMultipleFragmentsPerWarp, but before load data into fragments load block tiles into shared memory to
  //reduce number of global memory accesses
  //The reduce over k-dimension is very inefficient!!!
  //After every multiplication of mTileBlock rows of a-matrix with nTileBlock columns of b-matrix the result
  //is stored in local memory. This is very inefficient. The result should be stayed in fragments while
  // //reducing over k-dimension
  //Matrix dimensions must be divisible by block-tile-size
  //Block-tile-size must be divisible by warp-tile-size (64x32)
  def matMulShared0(mTileBlock: Int, nTileBlock: Int, kTileBlock: Int): Expr = {
    val mTileFrag = 16
    val nTileFrag = 16
    val kTileFrag = 16

    val mTileWarp = 4*mTileFrag
    val nTileWarp = 2*nTileFrag

    assert(mTileBlock % mTileWarp == 0)
    assert(nTileBlock % nTileWarp == 0)

    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (k `.` n `.` f16) ->: (m `.` n `.` f32)
    )((a, b) =>
      a |> split(mTileBlock) |>
      mapBlock(1)(fun(aRowsBlock => // mTileBlock.k.f16

        b |> transpose |> split(nTileBlock) |>
        mapBlock(0)(fun(bColumnsTBlock => // nTileBlock.k.f16

          zip
            (aRowsBlock |> transpose |> split(kTileBlock))
            (bColumnsTBlock |> transpose |> split(kTileBlock)) |>
            // k/kTileBlock.(kTileBlock.mTileBlock.f16, kTileBlock.nTileBlock.f16)

          //Block-level
          //Multiply a mTileBlock rows of a with nTileBlock columns of b
          //Result: mTileBlock.nTileBlock.f32 in shared memory
          oclReduceSeq(AddressSpace.Local)(fun((resultTile, aTbTileBlock) =>

            //Load aTile to shared memory
            let(toLocal(
              aTbTileBlock.`1` |> transpose |>
              mapThreads(1)(mapThreads(id)))) //mTileBlock.kTileBlock.f16
            be(aTile =>

              //Load bTile to shared memory
              let(toLocal(
                aTbTileBlock.`2` |>
                mapThreads(1)(mapThreads(id)))) //kTileBlock.mTileBlock.f16
              be(bTile =>

                zip
                  (aTile |> split(mTileWarp))
                  (resultTile |> split(mTileWarp)) |>

                mapThreads(1)(fun(aTilesWarpC => // (mTileWarp.kTileBlock.f16, mTileWarp.nTileBlock.f32)

                  zip
                    (bTile |> transpose |> split(nTileWarp))
                    (aTilesWarpC.`2` |> transpose |> split(nTileWarp)) |>
                  mapWarp(0)(fun(bTilesWarpTCT => // (nTileWarp.kTileBlock.f16, nTileWarp.mTileWarp.f32)

                    //Warp-level
                    //Multiply a mTileWarp.kTileBlock.f16-Tile with a kTileBlock.nTileWarp.f16-Tile and accumulate present result

                    zip
                      (aTilesWarpC.`1` |> transpose |> split(kTileFrag))
                      (bTilesWarpTCT.`1` |> transpose |> split(kTileFrag)) |>
                      //kTileBlock/kTileFrag.(kTileFrag.mTileWarp.f16, kTileFrag.nTileWarp.f16)

                    oclReduceSeqUnroll(AddressSpace.Private)(fun((cTiles, aTbTilesWarp) =>

                      //Load tile of a matrix to fragment
                      let(toPrivate(
                        aTbTilesWarp.`1` |> transpose |> split(mTileFrag) |>
                          mapSeqUnroll(fun(aFragTileT =>
                            aFragTileT |> asFragment))))
                      be(aFrags => // mTileWarp/mTileFrag.WmmaAMatrix

                        //Load tile of b matrix to fragment
                        let(toPrivate(
                          aTbTilesWarp.`2` |> transpose |> split(nTileFrag) |>
                            mapSeqUnroll(fun(bFragTileT =>
                              bFragTileT |> transpose |> asFragment))))
                        be(bFrags => // nTileWarp/nTileFrag.WmmaBMatrix

                          //Do matrix multiplication and accumulate with tensor cores
                          zip(aFrags)(cTiles) |>
                          mapSeqUnroll(fun(acFrags =>
                            zip(bFrags)(acFrags.`2`) |>
                            mapSeqUnroll(fun(bcFrags =>
                              tensorMMA(acFrags.`1`, bcFrags.`1`, bcFrags.`2`)))))))))

                    //This load from shared memory into fragments is unnecessary when reduce directly over fragments
                    (bTilesWarpTCT.`2` |> transpose |> split(mTileFrag) |>
                      mapSeqUnroll(fun(cTile =>
                        cTile |> transpose |> split(nTileFrag) |>
                        mapSeqUnroll(fun(cTileFragT =>
                          cTileFragT |> transpose |> asFragment)))))
                     |> // mTileWarp/mTileFrag.nTileWarp/nTileFrag.WmmaAccumulator

                    //This store into shared memory into fragments is unnecessary when reduce directly over fragments
                    mapSeqUnroll(fun(cTiles =>
                      cTiles |>
                      mapSeqUnroll(fun(cTile =>
                        cTile |>
                        asMatrix |>   // mTileFrag.nTileFrag.f32
                        transpose)) |>                // nTileWarp/nTileFrag.nTileFrag.mTileFrag.f32
                      join |>                         // nTileWarp.mTileFrag.f32
                      transpose)) |>                  // mTileWarp/mTileFrag.mTileFrag.nTileWarp.f32
                    join |>                           // mTileWarp.nTileWarp.f32

                    transpose)) |>                    // nTileBlock/nTileWarp.nTileWarp.mTileWarp.f32
                  join |>                             // nTileBlock.mTileWarp.f32
                  transpose)) |>                      // mTileBlock/mTileWarp.mTileWarp.nTileBlock.f32
                join))))                              // mTileBlock.nTileBlock.f32

          (generate2D |>
            mapThreads(1)(mapThreads(id))) |> // mTileBlock.nTileBlock.f32

          mapThreads(1)(mapThreads(id)) |>

          transpose)) |>                // n/nTileBlock.nTileBlock.mTileBlock.f32
        join |>                         // n.mTileBlock.f32
        transpose)) |>                  // m/mTileBlock.mTileBlock.n.f32
      join))                            // m.n.f32
    }



  //The following kernels can be run with different configs:
  //  --dimension of block tile (size of the tile caluculated by a single thread block in one iteration)
  //    this tile will be loaded into shared memory
  //  --dimensions of warp tile (size of the tile caluculated by a single warp in one iteration)
  //    this tile will be loaded into fragments (of a single warp)
  //  --dimensions for a single fragment
  case class mmConfig(mTileBlock: Nat, nTileBlock: Nat, kTileBlock: Nat, //dimension of a block tile
                      mTileWarp: Nat, nTileWarp: Nat, //dimensions of a warp tile (same k dimension as block tile)
                      mTileFrag: Nat, nTileFrag: Nat, kTileFrag: Nat //dimensions for a fragments
                      ) {
    //the warps within a single thread block are in a 2D formation
    def mNumberOfFragsWarp: Nat = mTileWarp /^ mTileFrag
    def nNumberOfFragsWarp: Nat = nTileWarp /^ nTileFrag
    //number of warps within a single thread block
    def mNumberOfWarps: Nat = mTileBlock /^ mTileWarp
    def nNumberOfWarps: Nat = nTileBlock /^ nTileWarp
    //total number of warps within a single thread block
    def numberOfWarps: Nat = mNumberOfWarps * nNumberOfWarps
  }

  object mmConfig {
    def apply() : mmConfig =
      apply(128, 128, 64)

    def apply(mTileBlock: Int, nTileBlock: Int, kTileBlock: Int) : mmConfig =
      apply(mTileBlock, nTileBlock, kTileBlock, 64, 32)

    def apply(mTileBlock: Int, nTileBlock: Int, kTileBlock: Int, mTileWarp: Int, nTileWarp: Int) : mmConfig =
      apply(mTileBlock, nTileBlock, kTileBlock, mTileWarp, nTileWarp, 16, 16, 16)

    def apply(mTileBlock: Int, nTileBlock: Int, kTileBlock: Int, mTileWarp: Int, nTileWarp: Int,
              mTileFrag: Int, nTileFrag: Int, kTileFrag: Int) : mmConfig = {
      assert(mTileBlock % mTileWarp == 0)
      assert(nTileBlock % nTileWarp == 0)

      assert(mTileWarp % mTileFrag == 0)
      assert(nTileWarp % nTileFrag == 0)
      assert(kTileBlock % kTileFrag == 0)

      mmConfig(mTileBlock: Nat, nTileBlock: Nat, kTileBlock: Nat, mTileWarp: Nat, nTileWarp: Nat,
        mTileFrag: Nat, nTileFrag: Nat, kTileFrag: Nat)
    }
  }

  //config of the kernels
  var config: mmConfig = _


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
  //                 =                                                                     +=
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
  //                   |____|____|____|____|
  //
  //                     |    |    |    |
  // caluclated by       |  Warp 5 |  Warp 7
  //                     |         |
  //                   Warp 4    Warp 6
  //
  //              resultTile (in fragments distributed over differtent warps!)
  //
  //                            |
  //                            |  epilog
  //                            V
  //
  //              resultTile stored in global memory


  //Generates Array with pairs of mTile row-blocks of matrix a and nTile column-blocks of matrix b
  //       _____________________        ______               _________________________________________
  //       |                   |        |    |               |    |    |    |    |    |    |    |    |
  //       |                   |        |    |               |    |    |    |    |    |    |    |    |
  //       |                   |        |    |               |    |    |    |    |    |    |    |    |
  //       |                   |        |____|               |____|____|____|____|____|____|____|____|
  //       |                   |
  //       |                   |       matrixTile     -->                    result
  //       |                   |
  //       |___________________|
  //
  //            matrix
  //
  def crossProductOfMatrixTiles(mTile: Nat, nTile: Nat): ToBeTyped[Expr] =
    fun((a, b) =>
      a |> split(mTile) |>
      map(fun(aRowBlock =>
        b |> transpose |> split(nTile) |>
        map(fun(bColumnTBlock =>
          makePair(aRowBlock)(bColumnTBlock |> transpose))))) |>
      join)


  //Parallel copy-function for a matrix distributed over warps and located therein threads
  //Copy a m.n.dt matrix with an vector size of vSize elements and full loop unrolling
  //Result: m.n.dt matrix
  def copyMatrix(m: Nat, n: Nat, vSize: Nat) : ToBeTyped[Expr] =
    fun(matrix =>
      matrix |>
      join |>
      split(m*n /^ config.numberOfWarps) |>
      mapWarp(fun(warpTile =>
        warpTile |>
        split(32*vSize) |>
        mapSeqUnroll(fun(x =>
          x |>
          asVectorAligned(vSize) |>
          mapLane(id) |>
          asScalar)) |>
        join)) |>
      join |>
      split(n))


  //Write a matrix to shared memory with pad elements spacing between two consecutive rows
  def toSharedWithPadding(numberOfColumns: Nat, pad: Nat): ToBeTyped[Expr] =
    fun(matrix =>
      matrix |>
      map(fun(row =>
        row |> padEmpty(pad))) |>
      toLocal |>
      map(fun(row =>
        row |> take(numberOfColumns)))
    )


  //Warp-level (executed by a single warp)
  //Multiply a mTileWarp.kTileBlock.f16-Tile with a kTileBlock.nTileWarp.f16-Tile and accumulate current result
  //Result: nTileWarp/nTileFrag.mTileWarp/mTileFrag.WmmaAccumulator
  def warpMMA: ToBeTyped[Expr] =
    fun((aTileWarp, bTileWarp, cFragsWarp) =>
        zip
          (aTileWarp |> transpose |> split(config.kTileFrag))
          (bTileWarp |> split(config.kTileFrag)) |>
          //kTileBlock/kTileFrag.(kTileBlock.mTileWarp.f16, kTileBlock.nTileWarp.f16)

        //This reduce should not allocate memory!!!
        oclReduceSeq(AddressSpace.Private)(fun((cFrags, aTbTilesFragments) =>

          //Load tile of a-matrix into multiple fragments
          let(toPrivate(
            aTbTilesFragments.`1` |>
            transpose |>
            split(config.mTileFrag) |>
            mapSeqUnroll(fun(aFragTile =>
              aFragTile |>
              asFragment))))
          be(aFrags => // numberOfAFragments.WmmaAMatrix

            //Load tile of b-matrix into multiple fragments
            let(toPrivate(
              aTbTilesFragments.`2` |>
              transpose |>
              split(config.nTileFrag) |>
              mapSeqUnroll(fun(bFragTile =>
                bFragTile |> transpose |>
                asFragment))))
            be(bFrags => // numberOfBFragments.WmmaBMatrix

              //Do matrix multiplication and accumulate with tensor cores
              zip(aFrags)(cFrags) |>

              mapSeqUnroll(fun(acFrags =>
                zip(bFrags)(acFrags.`2`) |>

                mapSeqUnroll(fun(bcFrags =>
                  tensorMMA(acFrags.`1`, bcFrags.`1`, bcFrags.`2`)))))))))

        (cFragsWarp |> mapSeq(mapSeq(id))))


  //Block-level (executed by a single thread block)
  //Multiply a mTileBlock rows of a-matrix with nTileBlock columns of b-matrix
  //epilog: Load matrix elements from fragments which are distributed over different warps (into global memory)
  //Result: mTileBlock.nTileBlock.f32
  private def blockMM(epilog: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    fun((aRowsBlock, bColumnsBlock) =>
      zip
        (aRowsBlock |> transpose |> split(config.kTileBlock))
        (bColumnsBlock |> split(config.kTileBlock)) |> // k/kTileBlock.(kTileBlock.mTileBlock.f16, kTileBlock.nTileBlock.f16)

      oclReduceSeq(AddressSpace.Private)(fun((cFragsBlock, aTbTileBlock) =>

        //Load aTile to shared memory
        let(toLocal(
          aTbTileBlock.`1` |> transpose |> join |> asVectorAligned(8) |>
          mapThreads(id) |> asScalar |>
          split(config.kTileBlock))) //mTileBlock.kTileBlock.f16
        be(aTile =>

          //Load bTile transposed (like it is in global memory) to shared memory
          let(toLocal(
            aTbTileBlock.`2` |> transpose |> join |> asVectorAligned(8) |>
            mapThreads(id) |> asScalar |>
            split(config.kTileBlock))) //kTileBlock.nTileBlock.f16
          be(bTileT =>

            zip
              (crossProductOfMatrixTiles(config.mTileWarp, config.nTileWarp)(aTile, bTileT |> transpose))
              (cFragsBlock) |>

            mapWarp(fun(abWarpC =>
              warpMMA(
                abWarpC.`1`.`1`,
                abWarpC.`1`.`2`,
                abWarpC.`2` |> split(config.nNumberOfFragsWarp)) |>

              mapSeq(mapSeq(id)))) |>
            join |>
            join |>
            split(config.mNumberOfFragsWarp * config.nNumberOfFragsWarp)))))

      (generate2D |>
        mapWarp(
          mapSeqUnroll(fun(z =>
            generateFragment(z))))) |>

      epilog)
  }


  //Same as function before but avoid bank conflicts during load tiles into shared memory and use
  //better copy function to copy from global memory to shared memory (unroll all loops)
  private def blockMMV2(epilog: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    fun((aRowsBlock, bColumnsBlock) =>
      zip
        (aRowsBlock |> transpose |> split(config.kTileBlock))
        (bColumnsBlock |> split(config.kTileBlock)) |> // k/kTileBlock.(kTileBlock.mTileBlock.f16, kTileBlock.nTileBlock.f16)

      oclReduceSeq(AddressSpace.Private)(fun((cFragsBlock, aTbTileBlock) =>

        //Load aTile to shared memory
        let(aTbTileBlock.`1` |>
          transpose |>
          copyMatrix(config.mTileBlock, config.kTileBlock, 8) |>
          toSharedWithPadding(config.kTileBlock, 8))
        be(aTile =>

          //Load bTile transposed (like it is in global memory) to shared memory
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

                  mapSeq(mapSeq(id)))) |>
              join |>
              join |>
              split(config.mNumberOfFragsWarp * config.nNumberOfFragsWarp)))))

      (generate2D |>
        mapWarp(
          mapSeqUnroll(fun(z =>
            generateFragment(z))))) |>

    epilog)
  }

  //Store matrix elements from fragments which are distributed over different warps (into global memory)
  //Simply use from-fragment-primitiv to load from fragments into global memory
  private def epilog: ToBeTyped[Expr] =
    fun(resultFragsBlock =>
      resultFragsBlock |>
      mapWarp(fun(resultFragsWarp =>

        //Result from a single warp
        resultFragsWarp |>
        mapSeqUnroll(fun(resultFrag =>
          resultFrag |>
          asMatrix |>                // mTileFrag.nTileFrag.f32
          transpose)) |>             // (mTileWarp/mTileFrag)*(nTileWarp/nTileFrag).nTileFrag.mTileFrag.f32
        join |>                      // (mTileWarp/mTileFrag)*nTileWarp.mTileFrag.f32
        split(config.nTileWarp) |>
        map(fun(x =>
          x |> transpose)) |>
        join |>
        transpose)) |>              // (mTileBlock/mTileWarp)*(nTileBlock/nTileWarp).nTileWarp.mTileWarp.f32

      join |>                       // (mTileBlock/mTileWarp)*nTileBlock.mTileWarp.f32
      split(config.nTileBlock) |>   // (mTileBlock/mTileWarp).nTileBlock.mTileWarp.f32
      map(fun(x =>
        x |> transpose)) |>         // (mTileBlock/mTileWarp).mTileWarp.nTileBlock.f32
      join)                         // mTilBlock.nTileBlock.f32


  //First store data to shared memory and then coalesced to global memory
  //Complete result must fit into shared memory!
  private def epilogV2: ToBeTyped[Expr] =
    fun(resultFragsBlock =>
      //First calculate result
      //This is necessary so the shared memory it can be reused, because of this the code for the epilog
      //is generated out of the scope of previous shared memory variables
      let(resultFragsBlock)
      //First write result into fragments, so the scope of the allocated shared memory for aBlockTile and bBlockTile ends
      be(resultFragsBlock =>

        resultFragsBlock |>
        mapWarp(fun(resultFragsWarp =>

          //Result from a single warp
          resultFragsWarp |>
          mapSeqUnroll(fun(resultFrag =>
            resultFrag |>
            asMatrix |>                         // mTileFrag.nTileFrag.f32
            transpose)) |>                      // (mTileWarp/mTileFrag)*(nTileWarp/nTileFrag).nTileFrag.mTileFrag.f32
          join |>                               // (mTileWarp/mTileFrag)*nTileWarp.mTileFrag.f32
          split(config.nTileWarp) |>
          map(fun(x =>
            x |> transpose)) |>
          join |>
          transpose)) |>                   // (mTileBlock/mTileWarp)*(nTileBlock/nTileWarp).nTileWarp.mTileWarp.f32

        join |>                            // (mTileBlock/mTileWarp)*nTileBlock.mTileWarp.f32
        split(config.nTileBlock) |>        // (mTileBlock/mTileWarp).nTileBlock.mTileWarp.f32
        map(fun(x =>
          x |> transpose)) |>              // (mTileBlock/mTileWarp).mTileWarp.nTileBlock.f32
        join |>                            // mTileBlock.nTileBlock.f32

        //Write this result to shared memory
        toLocal |>

        //And then coalesced to global memory
        copyMatrix(config.mTileBlock, config.nTileBlock, 4)))


  //First store data to shared memory avoiding bank conflicts and then coalesced to global memory
  //Complete result must NOT fit into shared memory (multple iterations possible)
  private def epilogV3: ToBeTyped[Expr] = {
    //Number of fragments that fit into shared memory and is divisible by numberOfWarps
    val fragmentsPerIteration = 32

    //Number of fragments that store a single warp into shared memory
    val fragmentsPerIterationPerWarp = fragmentsPerIteration / config.numberOfWarps.eval
    //the fragments within a single warp is in a 2D formation
    val mNumberOfFragmentsPerIteration = fragmentsPerIterationPerWarp / config.nNumberOfFragsWarp
    //number of rows that can be stored in global memory in a single iteration
    val matrixMDimensionPerIteration = mNumberOfFragmentsPerIteration * config.mTileFrag * config.mNumberOfWarps

    fun(resultFragsBlock =>
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


  //Multiply m.k.f16 a-matrix with k.n.f16 b-matrix
  //Matrix-dimensions must be divisible by configured block-size!
  //Result: m.n.f32 matrix
  //b-matrix is transposed
  private def deviceMM(blockMM: ToBeTyped[Expr]): ToBeTyped[Expr] =
    depFun((m: Nat, n: Nat, k: Nat) => fun(
      (m `.` k `.` f16) ->: (n `.` k `.` f16) ->: (m `.` n `.` f32)
    )((a, bT) =>
      crossProductOfMatrixTiles(config.mTileBlock, config.nTileBlock)(a, bT |> transpose) |>

      mapBlock(fun(aRowsBlockBColumnBlock =>

        blockMM(
          aRowsBlockBColumnBlock.`1`,
          aRowsBlockBColumnBlock.`2`) |> //mTileBlock.nTileblock.f32

        transpose)) |>                  //m/mTileBlock*n/nTileBlock.nTileBlock.mTilcblock.f32
      join |>                           //m/mTileBlock*n.mTilcblock.f32
      split(n) |>                       //m/mTileBlock.n.mTilcblock.f32
      map(fun(x =>
        x |> transpose)) |>             //m/mTileBlock.mTilcblock.n.f32
      join))                            //m.n.f32


  //Kernel for mm using shared memory
  def matMulSharedMemory(config: mmConfig): Expr = {
    this.config = config
    deviceMM(blockMM(epilog))
  }

  //Kernel for mm using shared memory avoiding bank conflicts while loading blockTiles into shared memory
  def matMulSharedMemoryV2(config: mmConfig): Expr = {
    this.config = config
    deviceMM(blockMMV2(epilog))
  }

  //Kernel for mm using shared memory avoiding bank conflicts while loading blockTiles into shared memory
  //and better epilog
  def matMulSharedMemoryV3(config: mmConfig): Expr = {
    this.config = config
    deviceMM(blockMMV2(epilogV2))
  }

  //Kernel for mm using shared memory avoiding bank conflicts while loading blockTiles into shared memory
  //and even better epilog
  def matMulSharedMemoryV4(config: mmConfig): Expr = {
    this.config = config
    deviceMM(blockMMV2(epilogV3))
  }
}
