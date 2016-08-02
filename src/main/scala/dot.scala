
import Core.OperationalSemantics.FloatData
import Core._
import DSL.untyped._
import OpenCL.Core.{HoistMemoryAllocations, ToOpenCL}
import OpenCL.DSL._
import apart.arithmetic._
import opencl.generator.OpenCLAST.Block
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions

object dot extends App {

  val N = SizeVar("N")
  val xsT = ExpType(ArrayType(N, float))
  val ysT = ExpType(ArrayType(N, float))

  def printOpenCLKernel1(name: String,
                         untypedLambda: Phrase[ExpType -> (ExpType -> ExpType)]) = {
    val lambda = TypeInference(untypedLambda)
    lambda.typeCheck()
    println(name + ":\n" + PrettyPrinter(lambda))

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
      lambda, identifier("xs", xsT), identifier("ys", ysT))))
    println("----------------")
  }

  def printOpenCLKernel2(name: String,
                         untypedLambda: Phrase[ExpType -> ExpType]) = {
    val lambda = TypeInference(untypedLambda)
    lambda.typeCheck()
    println(name + ":\n" + PrettyPrinter(lambda))

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
      lambda, identifier("in", xsT))))
    println("----------------")
  }

  val mult = λ(x => x._1 * x._2)
  val add = λ(x => λ(a => x + a))

  val high_level = λ(xsT)(xs => λ(ysT)(ys =>
    reduce(add, 0.0f) o map(mult) $ zip(xs, ys)
  ))

  printOpenCLKernel1("High-Level", high_level)

  val dotSimple = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f) o mapSeq(mult)
      ) o split(4)
    ) o split(1024) $ zip(xs, ys)
  ))

  printOpenCLKernel1("dotSimple", dotSimple)

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
      iterate(6,
        toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)) o
        toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotProduct2", dotProduct2)

  def dotSimpleDetailed(): Unit = {
    import HighLevelCombinators._
    import LowLevelCombinators._
    import OpenCL.LowLevelCombinators._

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

