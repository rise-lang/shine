package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class AtomicBinOpAssign extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def xsfT(N: NatIdentifier) = ArrayType(N, f32)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))

  def id: Expr = fun(x => x)

  val n = 32
  val k = 4

  val indices = new Array[Int](n)
  val values = new Array[Int](n)
  val fValues = new Array[Float](n)

  val r = new scala.util.Random
  var index = 0
  var x = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    x = 1 + r.nextInt(5)
    indices(i) = index
    values(i) = x
    fValues(i) = x
  }

  private val atomicTestInt = (func: Expr, init: Int) => {
    nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x int)
        oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(func)(
          generate(fun(IndexType(k))(_ => l(init))) |>
            mapLocal(id) // k.int
        ) |>
        mapLocal(id) // k.int
    ))))
  }

  private val atomicTestFloat = (func: Expr, init: Float) => {
    nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsfT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x float)
        oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(func)(
          generate(fun(IndexType(k))(_ => l(init))) |>
            mapLocal(id) // k.float
        ) |>
        mapLocal(id) // k.float
    ))))
  }

  test("AtomicBinOpAssign works correctly for function (a = a - x)") {
    val func = fun(a => fun(x => a - x))
    val result = new Array[Int](k)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = result(index) - values(i)
    }

    val kernel = gen.OpenCLKernel(atomicTestInt(func, 0))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    assert(kernel.code.contains("atomic_sub"))
    println("\nKernel correctly uses atomic_sub instead of workaround function with atomic_cmpxchg.")

    checkResult(output, result)
  }

  test("AtomicBinOpAssign works correctly for function (a = a * x)") {
    val func = fun(a => fun(x => a * x))
    val result = Array.fill[Int](n)(1)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = result(index) * values(i)
    }

    val kernel = gen.OpenCLKernel(atomicTestInt(func, 1))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    checkResult(output, result)
  }

  test("AtomicBinOpAssign works correctly with floats") {
    val func = fun(a => fun(x => a + x))
    val result = Array.fill[Float](n)(1)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = result(index) + values(i)
    }

    val kernel = gen.OpenCLKernel(atomicTestFloat(func, 1))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, fValues)

    checkResult(output, result)
  }

  test("AtomicBinOpAssign works correctly with commutativity") {
    val result = new Array[Int](k)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = result(index) + values(i)
    }

    val func = fun(a => fun(x => a + x))

    val kernel = gen.OpenCLKernel(atomicTestInt(func, 0))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    checkResult(output, result)

    val func2 = fun(a => fun(x => x + a))

    val kernel2 = gen.OpenCLKernel(atomicTestInt(func2, 0))

    val output2 = runKernel(kernel2)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    assert(kernel.code.contains("atomic_add"))
    assert(kernel2.code.contains("atomic_add"))
    println("\nKernels correctly use atomic_add instead of workaround function with atomic_cmpxchg.")

    checkResult(output2, output)
  }

  test("AtomicBinOpAssign works correctly for function (a = a + 2 * x)") {
    val func = fun(a => fun(x => a + l(2) * x))
    val result = new Array[Int](k)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = result(index) + 2  * values(i)
    }

    val kernel = gen.OpenCLKernel(atomicTestInt(func, 0))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    assert(kernel.code.contains("atomic_add"))
    println("\nKernel correctly uses atomic_add instead of workaround function with atomic_cmpxchg.")

    checkResult(output, result)
  }

  test("AtomicBinOpAssign works correctly for function (a = 2 * a + x)") {
    val func = fun(a => fun(x => l(2) * a + x))
    val result = new Array[Int](k)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = 2 * result(index) + values(i)
    }

    val kernel = gen.OpenCLKernel(atomicTestInt(func, 0))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    checkResult(output, result)
  }

  test("AtomicBinOpAssign works correctly with for function (a = 10.0f + 4.2f * x + 1.5f * a)") {
    val func = fun(a => fun(x => l(10.0f) + l(4.2f) * x + l(1.5f) * a))
    val result = new Array[Float](k)

    for (i <- 0 until n) {
      index = indices(i)
      result(index) = 10.0f + 4.2f * values(i) + 1.5f * result(index)
    }

    val kernel = gen.OpenCLKernel(atomicTestFloat(func, 0))

    val output = runKernel(kernel)(LocalSize(32), GlobalSize(32))(n, k, indices, fValues)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(result(i) + " ")

      assert((output(i) - result(i)).abs < 0.01f)
    }
    println("")
  }

  def checkResult[T](output: Array[T], expected: Array[T]): Unit = {
    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(expected(i) + " ")

      assert(output(i) == expected(i))
    }
    println("")
  }

  def runKernel[T](kernel: KernelNoSizes)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
                    n: Int,
                    k: Int,
                    indices: Array[Int],
                    values: Array[T]
                  ): Array[T] = {
    val runKernel = kernel
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[T]`)=>` Array[T]]
    val (output, _) = runKernel(localSize, globalSize)(n `,` k `,` indices `,` values)
    output
  }
}
