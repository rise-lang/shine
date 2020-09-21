package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLReduceByIndexGlobal extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def f_xsT(N: NatIdentifier) = ArrayType(N, f32)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 1000
  val k = 21

  val indices = new Array[Int](n)

  val values = new Array[Int](n)
  val result = new Array[Int](k)

  val f_values = new Array[Float](n)
  val f_result = new Array[Float](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    values(i) = i
    f_values(i) = i
    result(index) = result(index) + i
    f_result(index) = f_result(index) + i
  }

  test("Reduce By Index Global Test (Single Histogram, Int)") {

    val reduceByIndexGlobalTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        oclReduceByIndexGlobal(rise.core.types.AddressSpace.Global)(add)(
          //TODO: Data races can occur if you either use mapGlobal here or if you use mapLocal
          //      and start multiple workgroups as you can't synchronize globally.
          //      => In that case you could also use reduceByIndexLocal instead so
          //         does a reduceByIndexGlobal primitive even make sense?
          generate(fun(IndexType(k))(_ => l(0))) |>
            mapLocal(id)
        ) |>
        mapGlobal(id)
    ))))

    val output = runKernel(reduceByIndexGlobalTest)(LocalSize(1000), GlobalSize(1000))(n, k, indices, values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(result(i) + " ")

      assert(output(i) == result(i))
    }
  }

  test("Reduce By Index Global Test (Single Histogram, Float)") {

    val reduceByIndexGlobalTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(f_xsT(n))(xs =>
      zip(is)(xs) |>
        oclReduceByIndexGlobal(rise.core.types.AddressSpace.Global)(add)(
          //TODO: Data races can occur if you either use mapGlobal here or if you use mapLocal
          //      and start multiple workgroups as you can't synchronize globally.
          generate(fun(IndexType(k))(_ => l(0.0f))) |>
            mapLocal(id)
        ) |>
        mapGlobal(id)
    ))))

    val output = runKernel(reduceByIndexGlobalTest)(LocalSize(1000), GlobalSize(1000))(n, k, indices, f_values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(f_result(i) + " ")

      assert(output(i) == f_result(i))
    }
  }

  def runKernel[T](kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    indices: Array[Int],
    values: Array[T]
  ): Array[T] = {
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[T]`)=>` Array[T]]
    val (output, _) = runKernel(localSize, globalSize)(n `,` k `,` indices `,` values)
    output
  }

}
