package apps

import idealised.DPIA.Types.ExpType
import idealised.OpenCL.{GlobalSize, LocalSize}
import lift.core._
import lift.core.types._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.HighLevelConstructs.reorderWithStride
import idealised.util.{SyntaxChecker, gen}

import scala.util.Random

//noinspection TypeAnnotation
class asum extends idealised.util.TestsWithExecutor {

  def inputT(n : NatIdentifier) = ArrayType(n, float)
  val abs = dtFun(t => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
  val fabs = abs(float)
  val add = fun(x => fun(a => x + a))

  val high_level = nFun(n => fun(inputT(n))(input =>
    input |> map(fabs) |> reduceSeq(add)(l(0.0f))
  ))

  test("High level asum type inference works") {
    val typed = infer(high_level)

    val N = typed.t.asInstanceOf[NatDepFunType[_ <: Type]].x
    assertResult(DepFunType[NatKind, Type](N, FunType(inputT(N), float))) {
      typed.t
    }
  }

  // C code gen
  test("High level asum compiles to syntactically correct C") {
    gen.CProgram(high_level)
  }

  // OpenMP code gen
  test("Intel derived no warp compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(32768) |>
        mapPar(
          asVector(4) >>
          split(8192) >>
          mapSeq(
            reduceSeq(fun(x => fun(a => abs(float4)(x) + a)))(vectorFromScalar(l(0.0f)))
          ) >> asScalar
        ) |> join
    ))

    gen.OpenMPProgram(intelDerivedNoWarp1)
  }

  test("Second kernel of Intel derived compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val intelDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(2048) |>
        mapPar(
          split(2048) >> mapSeq(reduceSeq(add)(l(0.0f)))
        ) |> join
    ))

    gen.OpenMPProgram(intelDerived2)
  }

  test("AMD/Nvidia second kernel derived compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(8192) |>
        mapPar(
          split(128) >>
            mapSeq(reduceSeq(add)(l(0.0f))) >>
            iterate(6)(nFun(_ =>
              split(2) >>
                mapSeq(reduceSeq(add)(l(0.0f))))
            )
        ) |> join
    ))

    gen.OpenMPProgram(amdNvidiaDerived2)
  }

  { // OpenCL code gen
    import lift.OpenCL.primitives._
    import idealised.OpenCL

    val random = new Random()

    def generateInput(n: Int): Array[Float]         = Array.fill(n)((random.nextInt(2)-1).toFloat)
    def generatePositiveInput(n: Int): Array[Float] = Array.fill(n)((random.nextInt(2)+1).toFloat)
    def computeAsum(input: Array[Float]): Float     = input.map(_.abs).sum

    def run(kernel: Expr)(localSize: LocalSize, globalSize: GlobalSize)(n: Int, input: Array[Float]): Array[Float] = {
      import idealised.OpenCL._
      val runKernel = gen.OpenCLKernel(kernel).as[ScalaFunction `(` Int `,` Array[Float] `)=>` Array[Float]]
      val (output, _) = runKernel(localSize, globalSize)(n `,` input)
      output
    }

    val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(32768) |>
        mapWorkGroup(
          asVector(4) >>
            split(8192) >>
            mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(x => fun(a => abs(float4)(x) + a)))(vectorFromScalar(l(0.0f)))
            ) >> asScalar
        ) |> join
    ))

    test("Intel derived no warp compiles to syntactically correct OpenCL code") {
      val phrase = idealised.DPIA.fromLift(infer(intelDerivedNoWarp1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->:`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(LocalSize(128), GlobalSize(N))(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    ignore("Intel derived no warp executes correctly") {
      val n = 16777216
      val input = generateInput(n)
      val gold = computeAsum(input)

      val output = run(intelDerivedNoWarp1)(LocalSize(128), GlobalSize(n))(n, input)

      assert(computeAsum(output) == gold)
    }

    val intelDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(2048) |>
        mapWorkGroup(
          split(2048) >>
            mapLocal(oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)))
        ) |> join
    ))
    test("Second kernel of Intel derived compiles to syntactically correct OpenCL code") {
      val phrase = idealised.DPIA.fromLift(infer(intelDerived2))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->:`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(LocalSize(128), GlobalSize(N))(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("Second kernel of Intel derived executes correctly") {
      val n = 2048
      val input = generatePositiveInput(n)
      val gold = computeAsum(input)

      val output = run(intelDerived2)(LocalSize(1), GlobalSize(n))(n, input)

      assert(output.length == 1)
      assert(output.head == gold)
    }

    val nvidiaDerived1 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(2048 * 128) |>
        mapWorkGroup(
          reorderWithStride(128) >>
            split(2048) >>
            mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(x => fun(a => abs(float)(x) + a)))(l(0.0f))
            )
        ) |> join
    ))

    test("Nvidia kernel derived compiles to syntactically correct OpenCL code") {
      val phrase = idealised.DPIA.fromLift(infer(nvidiaDerived1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->:`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(LocalSize(128), GlobalSize(N))(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("Nvidia kernel derived executes correctly") {
      val n = 16777216
      val input = generateInput(n)
      val gold = computeAsum(input)

      val output = run(nvidiaDerived1)(LocalSize(128), GlobalSize(n))(n, input)

      assert(computeAsum(output) == gold)
    }

    val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(8192) |>
        mapWorkGroup(
          split(128) >>
            toLocalFun(mapLocal(oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)))) >>
            toLocalFun(oclIterate(AddressSpace.Local)(6)(nFun(_ =>
              split(2) >> mapLocal(oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)))
            ))) >> mapLocal(fun(x => x))
        ) |> join
    ))

    test("AMD/Nvidia second kernel derived compiles to syntactically correct OpenCL code") {
      val phrase = idealised.DPIA.fromLift(infer(amdNvidiaDerived2))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->:`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(LocalSize(128), GlobalSize(N))(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("AMD/Nvidia second kernel executes correctly") {
      val n = 8192
      val input = generatePositiveInput(n)
      val gold = computeAsum(input)

      val output = run(amdNvidiaDerived2)(LocalSize(128), GlobalSize(n))(n, input)

      assert(output.length == 1)
      assert(output.head == gold)
    }

    val amdDerived1 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(4096 * 128) |>
        mapWorkGroup(
          asVector(2) >>
            reorderWithStride(64) >>
            split(2048) >>
            mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(x => fun(a => abs(float2)(x) + a)))(vectorFromScalar(l(0.0f)))
            ) >> asScalar
        ) |> join
    ))

    test("AMD kernel derived compiles to syntactically correct OpenCL code") {
      val phrase = idealised.DPIA.fromLift(infer(amdDerived1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->:`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(LocalSize(128), GlobalSize(N))(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    ignore("AMD kernel derived executes correctly") {
      val n = 16777216
      val input = generateInput(n)
      val gold = computeAsum(input)

      val output = run(amdDerived1)(LocalSize(128), GlobalSize(n))(n, input)

      assert(computeAsum(output) == gold)
    }
  }
}
