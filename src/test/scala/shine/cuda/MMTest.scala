package shine.cuda

import rise.core.types.DataType.*
import rise.core.types.MatrixLayout.*
import rise.core.types.{FunType => _, *}
import shine.DPIA.*
import shine.DPIA.Phrases.*
import shine.DPIA.Types.*
import shine.DPIA.primitives.functional.{Fst, Join, Snd, Split, Transpose, Zip}
import shine.OpenCL.*
import shine.OpenCL.primitives.functional.{ReduceSeq, ToMem}
import shine.cuda.KernelExecutor.KernelNoSizes
import shine.cuda.primitives.functional.*
import test_util.similar
import util.gen

import scala.reflect.Selectable.reflectiveSelectable

class MMTest extends test_util.TestWithCUDA {
  val n: NatIdentifier = NatIdentifier(freshName("n"))
  val m: NatIdentifier = NatIdentifier(freshName("m"))
  val k: NatIdentifier = NatIdentifier(freshName("k"))

  val compilerOptions = List("--gpu-architecture=compute_70")


  //Simple 16x16 matrix multiplication
  test("mmaTest for 16x16 matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixATile = Identifier(freshName("MatrixATile"), ExpType(ArrayType(mTile, ArrayType(kTile, f16)), read))
    val matrixBTile = Identifier(freshName("MatrixBTile"), ExpType(ArrayType(kTile, ArrayType(nTile, f16)), read))

    //Kernel
    val simpleMatMulTile =
      Lambda[ExpType, FunType[ExpType, ExpType]](matrixATile,
        Lambda[ExpType, ExpType](matrixBTile,
          //Write Result in output
          AsMatrix(mTile, nTile, kTile, f32,

            //do matrix multiplication
            ToMem(shine.cuda.AddressSpace.Private, FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None),
              TensorMatMultAdd(mTile, nTile, kTile, MatrixLayoutIdentifier("ml"), MatrixLayoutIdentifier("ml"), f16, f32,

                //load aMatrix into a fragment
                ToMem(shine.cuda.AddressSpace.Private, FragmentType(mTile, kTile, nTile, f16, Fragment.AMatrix, MatrixLayoutIdentifier("ml")),
                  shine.cuda.AsFragment(mTile, kTile, nTile, f16, Fragment.AMatrix, matrixATile)),

                //load bMatrix into a fragment
                ToMem(shine.cuda.AddressSpace.Private, FragmentType(kTile, nTile, mTile, f16, Fragment.BMatrix, MatrixLayoutIdentifier("ml")),
                  shine.cuda.AsFragment(kTile, nTile, mTile, f16, Fragment.BMatrix, matrixBTile)),

                //add fragment with zeros
                ToMem(shine.cuda.AddressSpace.Private, FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None),
                  GenerateFragment(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None, Literal(FloatData(0.0f))))))))
      )

    val kernel = gen.cuda.kernel("matrixMult").fromPhrase(simpleMatMulTile)

    logger.debug("KernelCode:")
    logger.debug(shine.cuda.KernelModule.translationToString(kernel))

    //Check kernel-result
    val matrixATest = generateMatrix(mTile, kTile)
    val matrixBTest = generateMatrix(kTile, nTile)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    if (executeCudaTests) {
      val scalaFun = KernelNoSizes(kernel, compilerOptions).as[Args `(` scala.Array[scala.Array[Float]] `,`
        scala.Array[scala.Array[Float]], scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

      val (result, _) = scalaFun(matrixATest `,` matrixBTest)

      val resultMatrix = result.sliding(mTile, nTile).toArray

      //Check result
      if (!similar(resultTest, resultMatrix)) {
        logger.debug("Expected:")
        logger.debug(resultTest.map(_.mkString(" ")).mkString("\n"))
        logger.debug("\nFound:")
        logger.debug(resultMatrix.map(_.mkString(" ")).mkString("\n"))
        throw new Exception("False Result")
      }
    }
  }

  //Simple 16xk * kx16 matrix multiplication
  test("mmaTest for 16xk * kx16 matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixATile = Identifier(freshName("MatrixATile"), ExpType(ArrayType(mTile, ArrayType(k, f16)), read))
    val matrixBTile = Identifier(freshName("MatrixBTile"), ExpType(ArrayType(k, ArrayType(nTile, f16)), read))
    val matrixCFrag = Identifier(freshName("MatrixCFrag"), ExpType(FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None), read))
    val matrixABTiles = Identifier(freshName("MatrixABTiles"), ExpType(PairType(
      ArrayType(mTile, ArrayType(kTile, f16)),
      ArrayType(kTile, ArrayType(nTile, f16))), read))

    //Kernel
    val simpleMatMulTile =
      DepLambda(NatKind, k)(
        Lambda[ExpType, FunType[ExpType, ExpType]](matrixATile,
          Lambda[ExpType, ExpType](matrixBTile,
            AsMatrix(mTile, nTile, kTile, f32,
              ReduceSeq(unroll = false)(k /^ kTile, shine.cuda.AddressSpace.Private,
                PairType(
                  ArrayType(mTile, ArrayType(kTile, f16)),
                  ArrayType(kTile, ArrayType(nTile, f16))),
                FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None),

                Lambda[ExpType, FunType[ExpType, ExpType]](matrixCFrag,
                  Lambda[ExpType, ExpType](matrixABTiles,
                    TensorMatMultAdd(mTile, nTile, kTile, MatrixLayoutIdentifier("ml"), MatrixLayoutIdentifier("ml"), f16, f32,
                      ToMem(shine.cuda.AddressSpace.Private, FragmentType(mTile, kTile, nTile, f16, Fragment.AMatrix, MatrixLayoutIdentifier("ml")),
                        shine.cuda.AsFragment(mTile, kTile, nTile, f16, Fragment.AMatrix,
                          Transpose(kTile, mTile, f16, read,
                            Fst(
                              ArrayType(kTile, ArrayType(mTile, f16)),
                              ArrayType(kTile, ArrayType(nTile, f16)),
                              matrixABTiles)))),

                      ToMem(shine.cuda.AddressSpace.Private, FragmentType(kTile, nTile, mTile, f16, Fragment.BMatrix, MatrixLayoutIdentifier("ml")),
                        shine.cuda.AsFragment(kTile, nTile, mTile, f16, Fragment.BMatrix,
                          Snd(
                            ArrayType(mTile, ArrayType(kTile, f16)),
                            ArrayType(kTile, ArrayType(nTile, f16)),
                            matrixABTiles))),

                      matrixCFrag))),

                  GenerateFragment(mTile, nTile, kTile, f32, Fragment.Accumulator, Row_Major, Literal(FloatData(0.0f))),

                Zip(k /^ kTile,
                  ArrayType(mTile, ArrayType(kTile, f16)),
                  ArrayType(kTile, ArrayType(nTile, f16)),
                  read,

                  Split(kTile, k /^ kTile, read, ArrayType(mTile, f16),
                    Transpose(mTile, k, f16, read, matrixATile)),

                  Split(kTile, k /^ kTile, read, ArrayType(nTile, f16),
                    matrixBTile)
                ))))))

    val kernel = gen.cuda.kernel("matrixMult").fromPhrase(simpleMatMulTile)

    logger.debug("KernelCode:")
    logger.debug(shine.cuda.KernelModule.translationToString(kernel))


    //Check kernel-result
    val kTest = 16 * 2

    val matrixATest = generateMatrix(mTile, kTest)
    val matrixBTest = generateMatrix(kTest, nTile)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    if (executeCudaTests) {
      val scalaFun = KernelNoSizes(kernel, compilerOptions).as[Args `(` Int `,` scala.Array[scala.Array[Float]] `,`
        scala.Array[scala.Array[Float]], scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

      val (result, _) = scalaFun(kTest `,` matrixATest `,` matrixBTest)

      val resultMatrix = result.sliding(mTile, nTile).toArray

      //Check result
      if (!similar(resultTest, resultMatrix)) {
        logger.debug("Expected:")
        logger.debug(resultTest.map(_.mkString(" ")).mkString("\n"))
        logger.debug("\nFound:")
        logger.debug(resultMatrix.map(_.mkString(" ")).mkString("\n"))
        throw new Exception("False Result")
      }
    }
  }


  //  matrixmultiplication of a mxk and a kx16 matrix
  test("mmaTest for mxk * kxn matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixA = Identifier(freshName("MatrixA"), ExpType(ArrayType(m, ArrayType(k, f16)), read))
    val matrixB = Identifier(freshName("MatrixBColumn"), ExpType(ArrayType(k, ArrayType(n, f16)), read))

    val matrixARow = Identifier(freshName("MatrixARow"), ExpType(ArrayType(mTile, ArrayType(k, f16)), read))
    val matrixBColumnT = Identifier(freshName("MatrixBColumn"), ExpType(ArrayType(nTile, ArrayType(k, f16)), read))

    val matrixCFrag = Identifier(freshName("MatrixCFrag"), ExpType(FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None), read))

    val matrixABTiles = Identifier(freshName("MatrixABTiles"), ExpType(PairType(
      ArrayType(kTile, ArrayType(mTile, f16)),
      ArrayType(kTile, ArrayType(nTile, f16))), read))

    //Kernel
    val simpleMatMul =
      DepLambda(NatKind, m)(
        DepLambda(NatKind, n)(
          DepLambda(NatKind, k)(
            //Input: matrixA
            Lambda[ExpType, FunType[ExpType, ExpType]](matrixA,
              //And matrixB
              Lambda[ExpType, ExpType](matrixB,
                Transpose(n, m, f32, write,
                  Join(n /^ nTile, nTile, write, ArrayType(m, f32),
                  //Map over nTile-column-block of matrixB
                  Map(Local, 1)(n /^ nTile,
                    //A transposed column of matrixB
                    ArrayType(nTile, ArrayType(k, f16)),

                    //Result: transposed tile of cMatrix
                    ArrayType(nTile, ArrayType(m, f32)),

                    Lambda[ExpType, ExpType](matrixBColumnT,
                      //Transpose cTile
                      Transpose(m, nTile, f32, write,
                        Join(m /^ mTile, mTile, write, ArrayType(nTile, f32),
                          //Map over mTile-row-blocks of matrixA
                          Map(Warp, 0)(m /^ mTile,
                            //A row of matrixA
                            ArrayType(mTile, ArrayType(k, f16)),

                            //Result: tile of cMatrix
                            ArrayType(mTile, ArrayType(nTile, f32)),

                            Lambda[ExpType, ExpType](matrixARow,
                              AsMatrix(mTile, nTile, kTile, f32,
                                //Multiply mTile rows of matrixA with kTest columns of matrixB
                                ReduceSeq(unroll = false)(k /^ kTile, shine.cuda.AddressSpace.Private,
                                  //Input: Pair of transposed matrixATile and matrixBTile
                                  PairType(
                                    ArrayType(mTile, ArrayType(kTile, f16)),
                                    ArrayType(kTile, ArrayType(nTile, f16))),

                                  //Result: tile of cMatrix as fragment
                                  FragmentType(mTile, nTile, kTile, f32, Fragment.Accumulator, MatrixLayout.None),

                                  //Multiply matrixATile and matrixBTile
                                  Lambda[ExpType, FunType[ExpType, ExpType]](matrixCFrag,
                                    Lambda[ExpType, ExpType](matrixABTiles,
                                      //matrix multiply and accumulate
                                      TensorMatMultAdd(mTile, nTile, kTile, MatrixLayoutIdentifier("ml"), MatrixLayoutIdentifier("ml"), f16, f32,

                                        //matrixATile as fragment
                                        ToMem(shine.cuda.AddressSpace.Private, FragmentType(mTile, kTile, nTile, f16, Fragment.AMatrix, MatrixLayoutIdentifier("ml")),
                                          shine.cuda.AsFragment(mTile, kTile, nTile, f16, Fragment.AMatrix,
                                            Transpose(kTile, mTile, f16, read,
                                              Fst(
                                                ArrayType(mTile, ArrayType(kTile, f16)),
                                                ArrayType(kTile, ArrayType(nTile, f16)),
                                                matrixABTiles)))),

                                        //matrixBTile as fragment
                                        ToMem(shine.cuda.AddressSpace.Private, FragmentType(kTile, nTile, mTile, f16, Fragment.BMatrix, MatrixLayoutIdentifier("ml")),
                                          shine.cuda.AsFragment(kTile, nTile, mTile, f16, Fragment.BMatrix,
                                            Snd(
                                              ArrayType(mTile, ArrayType(kTile, f16)),
                                              ArrayType(kTile, ArrayType(nTile, f16)),
                                              matrixABTiles))),

                                        matrixCFrag))),

                                  //Neutral Element for Reduce: fragment initialized with zeros
                                  GenerateFragment(mTile, nTile, kTile, f32, Fragment.Accumulator, Row_Major, Literal(FloatData(0.0f))),

                                  //Zip transposed, splited row of matrixA and splited column of matrixB
                                  Zip(k /^ kTile,
                                    ArrayType(kTile, ArrayType(mTile, f16)),
                                    ArrayType(kTile, ArrayType(nTile, f16)),
                                    read,

                                    //Split transposed row of matrixA kTest.mTile.f16-Array -> k/kTest.kTest.mTile.f16-Array
                                    Split(kTile, k /^ kTile, read, ArrayType(mTile, f16),
                                      //Transpose row of matrixA mTile.kTest.f16-Array -> kTest.mTile.f16-Array
                                      Transpose(mTile, k, f16, read, matrixARow)),

                                    //Split transposed column of matrixB kTest.nTest.f16-Array -> k/kTest.kTest.nTest.f16-Array
                                    Split(kTile, k /^ kTile, read, ArrayType(nTile, f16),
                                      Transpose(nTile, k, f16, read, matrixBColumnT))
                                  )))),

                            //Split aMatrix in mTile-row-blocks
                            Split(mTile, m /^ mTile, read, ArrayType(k, f16), matrixA))
                        ))),

                    //Split bMatrix in nTile-column-blocks
                    Split(nTile, n /^ nTile, read, ArrayType(k, f16),
                      Transpose(k, n, f16, read, matrixB))))
              ))))))

    val kernel = gen.cuda.kernel("matrixMult").fromPhrase(simpleMatMul)

    logger.debug("KernelCode:")
    logger.debug(shine.cuda.KernelModule.translationToString(kernel))


    //Check kernel-result
    val mTest = 32
    val nTest = 32
    val kTest = 32

    val matrixATest = generateMatrix(mTest, kTest)
    val matrixBTest = generateMatrix(kTest, nTest)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    if (executeCudaTests) {
      val scalaFun = KernelNoSizes(kernel, compilerOptions).as[Args `(`
        Int `,` Int `,` Int `,` scala.Array[scala.Array[Float]] `,`
        scala.Array[scala.Array[Float]], scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

      val (result, _) = scalaFun(mTest `,` nTest `,` kTest `,` matrixATest `,` matrixBTest)

      val resultMatrix = result.sliding(mTest, nTest).toArray

      //Check result
      if (!similar(resultTest, resultMatrix)) {
        logger.debug("Expected:")
        logger.debug(resultTest.map(_.mkString(" ")).mkString("\n"))
        logger.debug("\nFound:")
        logger.debug(resultMatrix.map(_.mkString(" ")).mkString("\n"))
        throw new Exception("False Result")
      }
    }
  }

  private def generateMatrix(n: Int, m: Int): scala.Array[scala.Array[Float]] = {
    val array = new scala.Array[scala.Array[Float]](n)

    for (i <- 0 until n) {
      array(i) = new scala.Array[Float](m)
      for (j <- 0 until m) {
        array(i)(j) = (i * j).asInstanceOf[Float]
      }
    }

    array
  }

  /**
    * Multiply matrixA with matrixB using scala.
    *
    * @param matrixA first matrix
    * @param matrixB second matrix
    * @return product of matrixA and matrixB
    */
  private def computeGold(matrixA: scala.Array[scala.Array[Float]], matrixB: scala.Array[scala.Array[Float]])
  : scala.Array[scala.Array[Float]] = {
    assert(matrixA.transpose.length == matrixB.length)

    matrixA.map(rowA =>
      matrixB.transpose.map(columnB =>
        (rowA zip columnB
          map Function.tupled(_ * _)).sum))
  }
}
