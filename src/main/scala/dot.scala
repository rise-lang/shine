
import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics.FloatData
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import apart.arithmetic._
import opencl.generator.OpenCLAST.Block
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions
import scala.util.Random

object dot extends App {

  Executor.loadLibrary()
  Executor.init()

  val epsilon = 1.0f

  val check = true
  val N = SizeVar("N")
  val xsT = ExpType(ArrayType(N, float))
  val ysT = ExpType(ArrayType(N, float))

  def printOpenCLKernel1(name: String,
                         untypedLambda: Phrase[ExpType -> (ExpType -> ExpType)]): Unit = {
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Float] :: Array[Float] :: Nil) =:=> Array[Float]](kernel)

    val size = 1024 * 1024

    val xs = Array.fill(size)(Random.nextInt(4).toFloat)
    val ys = Array.fill(size)(Random.nextInt(4).toFloat)

    val (res, time) = fun(xs :: ys)
    println(s"RESULT KERNEL1 NAME: $name TIME: $time")
    if (check) {
      val gold = (xs zip ys).map{ case (x, y) => x * y }.sum
      if (res.sum - gold < epsilon) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
        println(s"res: ${res.sum} vs. gold: $gold")
      }
    }

    println("----------------\n")
  }

  def printOpenCLKernel2(name: String,
                         untypedLambda: Phrase[ExpType -> ExpType]): Unit = {
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Float] :: Nil) =:=> Array[Float]](kernel)

    val size = 512

    val xs = Array.fill(size)(Random.nextInt(10).toFloat)

    val (res, time) = fun(xs :: HNil)
    println(s"RESULT KERNEL2 NAME: $name TIME: $time")
    if (check) {
      val gold = xs.sum
      if (res.sum - gold < epsilon) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
      }
    }

    println("----------------\n")
  }

  val mult = λ(x => x._1 * x._2)
  val add = λ(x => λ(a => x + a))

//  val high_level = λ(xsT)(xs => λ(ysT)(ys =>
//    reduce(add, 0.0f) o map(mult) $ zip(xs, ys)
//  ))
//
//  {
//    val lambda = TypeInference(high_level)
//    println("high_level:\n" + PrettyPrinter(lambda))
//    lambda.typeCheck()
//
//    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
//    val kernel = toOpenCL.makeKernel(lambda)
//    println(OpenCLPrinter()(kernel))
//  }
////  sys.exit(1)
//
//  val dotSimpler = λ(xsT)(xs => λ(ysT)(ys =>
//    mapGlobal(
//      reduceSeq(add, 0.0f) o mapSeq(mult)
//    ) o split(1024) $ zip(xs, ys)
//  ))
//
//  printOpenCLKernel1("dotSimpler", dotSimpler)
//
//  val dotSimple = λ(xsT)(xs => λ(ysT)(ys =>
//    join() o mapWorkgroup(
//      mapLocal(
//        reduceSeq(add, 0.0f) o mapSeq(mult)
//      ) o split(4)
//    ) o split(1024) $ zip(xs, ys)
//  ))
//
//  printOpenCLKernel1("dotSimple", dotSimple)


  val dotCPUVector1 = λ(xsT)(xs => λ(ysT)(ys =>
    asScalar() o join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => mult(x) + a)), vectorize(4, 0.0f))
      ) o split(2048)
    ) o split(2048 * 64) $ zip(asVector(4) $ xs, asVector(4) $ ys)
  ))

  val intelDerivedNoWarpDot1 = λ(xsT)(xs => λ(ysT)(ys =>
    asScalar() o join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => mult(x) + a)), vectorize(4, 0.0f))
      ) o split(8192)
    ) o split(8192) $ zip(asVector(4) $ xs, asVector(4) $ ys)
  ))

  printOpenCLKernel1("intelDerivedNoWarpDot1", dotCPUVector1)

  val dotCPU1 = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)
      ) o split(2048)
    ) o split(2048 * 128) $ zip(xs, ys)
  ))

  printOpenCLKernel1("dotCPU1", dotCPU1)

  val dotCPU2 = λ(xsT)(in =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => x + a)), 0.0f)
      ) o split(128)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotCPU2", dotCPU2)

  val dotProduct1 = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ zip(xs, ys)
  ))

  printOpenCLKernel1("dotProduct1", dotProduct1)

  val dotProduct2 = λ(xsT)(in =>
    join() o mapWorkgroup(
      iterate(6, toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)) o
        toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotProduct2", dotProduct2)

  def dotSimpleDetailed(): Unit = {
    import idealised.HighLevelCombinators._
    import idealised.LowLevelCombinators._
    import idealised.OpenCL.LowLevelCombinators._

    val dt = float

    val xs = IdentPhrase("xs", exp"[$N.$dt]")
    val ys = IdentPhrase("ys", exp"[$N.$dt]")
    val output = IdentPhrase("output", acc"[${N /^ 4}.$dt]")

    val dotSimpleImp: Phrase[CommandType] =
      ParForWorkGroup(N /^ 1024, dt = dt"[${Cst(256)}.$dt]",
        out = JoinAcc(n = N /^ 1024, m = 256, dt, output),
        body = \(exp"[idx(${N /^ 1024})]")(v84 =>
          \(acc"[${Cst(256)}.$dt]")(v85 =>
            ParForLocal(n = 256, dt,
              out = v85,
              body = \(exp"[idx(${Cst(256)})]")(v86 =>
                \(acc"[$dt]")(v87 =>
                  New(dt"[${Cst(4)}.$dt]", OpenCL.GlobalMemory,
                    \(VarType(dt"[${Cst(4)}.$dt]"))(v74 =>
                      Seq(
                        For(n = 4, \(exp"[idx(${Cst(4)})]")(v88 =>
                          Assign(dt,
                            lhs = IdxAcc(n = 4, dt,
                              index = v88,
                              array = Proj2Phrase(v74)
                            ),
                            rhs = BinOpPhrase(BinOpPhrase.Op.MUL,
                              lhs = Fst(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              rhs = Snd(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )),
                        New(dt, OpenCL.PrivateMemory,
                          \(VarType(dt))(v89 =>
                            Seq(
                              Seq(
                                Assign(dt,
                                  lhs = Proj2Phrase(v89),
                                  rhs = LiteralPhrase(FloatData(0.0f), dt)
                                ),
                                For(4, \(exp"[idx(${Cst(4)})]")(v90 =>
                                  Assign(dt,
                                    lhs = Proj2Phrase(v89),
                                    rhs = BinOpPhrase(BinOpPhrase.Op.ADD,
                                      lhs = Idx(4, dt,
                                        index = v90,
                                        array = Proj1Phrase(v74)
                                      ),
                                      rhs = Proj1Phrase(v89)
                                    )
                                  )
                                ))
                              ),
                              Assign(dt,
                                lhs = v87,
                                rhs = Proj1Phrase(v89)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    val (dotSimpleImplAdjustedMem, _) = HoistMemoryAllocations(dotSimpleImp)
    TypeChecker(dotSimpleImplAdjustedMem)
    Core.xmlPrinter.toFile("/tmp/mem.xml", dotSimpleImplAdjustedMem)

    val bodyAdjustedMem = ToOpenCL.cmd(dotSimpleImplAdjustedMem, Block(), ToOpenCL.Environment(128, N))

    println("============")
    println(OpenCLPrinter()(bodyAdjustedMem))
    println("============")

    TypeChecker(dotSimpleImp)

    Core.xmlPrinter.toFile("/tmp/pp.xml", dotSimpleImp)

    val body = ToOpenCL.cmd(dotSimpleImp, Block(), ToOpenCL.Environment(128, N))

    println(OpenCLPrinter()(body))

    val dotSimpleImp2 =
      ParForWorkGroup(N /^ 1024, dt = dt"[${Cst(256)}.$dt]",
        out = JoinAcc(n = N /^ 1024, m = 256, dt, output),
        body = \(exp"[idx(${N /^ 1024})]")(v84 =>
          \(acc"[${Cst(256)}.$dt]")(v85 =>
            New(dt"[${Cst(256)}.${Cst(4)}.$dt]", OpenCL.GlobalMemory,
              \(VarType(dt"[${Cst(256)}.${Cst(4)}.$dt]"))(v74 =>
                ParForLocal(n = 256, dt,
                  out = v85,
                  body = \(exp"[idx(${Cst(256)})]")(v86 =>
                    \(acc"[$dt]")(v87 =>
                      Seq(
                        For(n = 4, \(exp"[idx(${Cst(4)})]")(v88 =>
                          Assign(dt,
                            lhs = IdxAcc(n = 4, dt,
                              index = v88,
                              array = IdxAcc(n = 256, dt"[${Cst(4)}.$dt]",
                                index = v86,
                                array = Proj2Phrase(v74)
                              )
                            ),
                            rhs = BinOpPhrase(BinOpPhrase.Op.MUL,
                              lhs = Fst(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              rhs = Snd(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )),
                        New(dt, OpenCL.PrivateMemory,
                          \(VarType(dt))(v89 =>
                            Seq(
                              Seq(
                                Assign(dt,
                                  lhs = Proj2Phrase(v89),
                                  rhs = LiteralPhrase(FloatData(0.0f), dt)
                                ),
                                For(4, \(exp"[idx(${Cst(4)})]")(v90 =>
                                  Assign(dt,
                                    lhs = Proj2Phrase(v89),
                                    rhs = BinOpPhrase(BinOpPhrase.Op.ADD,
                                      lhs = Idx(4, dt,
                                        index = v90,
                                        array = Idx(n = 256, dt"[${Cst(4)}.$dt]",
                                          index = v86,
                                          array = Proj1Phrase(v74)
                                        )
                                      ),
                                      rhs = Proj1Phrase(v89)
                                    )
                                  )
                                ))
                              ),
                              Assign(dt,
                                lhs = v87,
                                rhs = Proj1Phrase(v89)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    TypeChecker(dotSimpleImp2)

    Core.xmlPrinter.toFile("/tmp/ppp.xml", dotSimpleImp2)

    val body2 = ToOpenCL.cmd(dotSimpleImp2, Block(), ToOpenCL.Environment(128, N))

    println(OpenCLPrinter()(body2))

    val dotSimpleImp3 =
      New(dt"[${N /^ 1024}.${Cst(256)}.${Cst(4)}.$dt]", OpenCL.GlobalMemory,
        \(VarType(dt"[${N /^ 1024}.${Cst(256)}.${Cst(4)}.$dt]"))(v74 =>
          ParForWorkGroup(N /^ 1024, dt = dt"[${Cst(256)}.$dt]",
            out = JoinAcc(n = N /^ 1024, m = 256, dt, output),
            body = \(exp"[idx(${N /^ 1024})]")(v84 =>
              \(acc"[${Cst(256)}.$dt]")(v85 =>
                ParForLocal(n = 256, dt,
                  out = v85,
                  body = \(exp"[idx(${Cst(256)})]")(v86 =>
                    \(acc"[$dt]")(v87 =>
                      Seq(
                        For(n = 4, \(exp"[idx(${Cst(4)})]")(v88 =>
                          Assign(dt,
                            lhs = IdxAcc(n = 4, dt,
                              index = v88,
                              array = IdxAcc(n = 256, dt"[${Cst(4)}.$dt]",
                                index = v86,
                                array = IdxAcc(n = N /^ 1024, dt"[${Cst(256)}.${Cst(4)}.$dt]",
                                  index = v84,
                                  array = Proj2Phrase(v74)
                                )
                              )
                            ),
                            rhs = BinOpPhrase(BinOpPhrase.Op.MUL,
                              lhs = Fst(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              rhs = Snd(dt, dt,
                                Idx(4, dt x dt,
                                  index = v88,
                                  array = Idx(256, dt"[${Cst(4)}.($dt x $dt)]",
                                    index = v86,
                                    array = Split(n = 4, m = 256, dt x dt,
                                      Idx(N /^ 1024, dt"[${Cst(1024)}.($dt x $dt)]",
                                        index = v84,
                                        array = Split(n = 1024, m = N /^ 1024, dt x dt,
                                          Zip(N, dt, dt, lhs = xs, rhs = ys)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )),
                        New(dt, OpenCL.PrivateMemory,
                          \(VarType(dt))(v89 =>
                            Seq(
                              Seq(
                                Assign(dt,
                                  lhs = Proj2Phrase(v89),
                                  rhs = LiteralPhrase(FloatData(0.0f), dt)
                                ),
                                For(4, \(exp"[idx(${Cst(4)})]")(v90 =>
                                  Assign(dt,
                                    lhs = Proj2Phrase(v89),
                                    rhs = BinOpPhrase(BinOpPhrase.Op.ADD,
                                      lhs = Idx(4, dt,
                                        index = v90,
                                        array = Idx(n = 256, dt"[${Cst(4)}.$dt]",
                                          index = v86,
                                          array = Idx(n = N /^ 1024, dt"[${Cst(256)}.${Cst(4)}.$dt]",
                                            index = v84,
                                            array = Proj1Phrase(v74)
                                          )
                                        )
                                      ),
                                      rhs = Proj1Phrase(v89)
                                    )
                                  )
                                ))
                              ),
                              Assign(dt,
                                lhs = v87,
                                rhs = Proj1Phrase(v89)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    TypeChecker(dotSimpleImp3)

    Core.xmlPrinter.toFile("/tmp/pppp.xml", dotSimpleImp3)

    val body3 = ToOpenCL.cmd(dotSimpleImp3, Block(), ToOpenCL.Environment(128, N))

    println(OpenCLPrinter()(body3))
  }

}

