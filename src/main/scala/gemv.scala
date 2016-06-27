
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object gemv extends App {

  val reorderWithStride = (s: ArithExpr) => {
    (i: ArithExpr, t: DataType) => {
      val n = ir.Type.getLength(DataType.toType(t)) /^ s
      (i / n) + s * (i % n)
    }
  }

  val N = SizeVar("N")
  val M = SizeVar("M")
  val dataT = float
  val xsT = ExpType(ArrayType(N, dataT))
  val ysT = ExpType(ArrayType(M, dataT))
  val matT = ExpType(ArrayType(M, ArrayType(N, dataT)))

  def printOpenCLKernel1(name: String,
                         lambda: Phrase[ExpType ->(ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))]) = {
    TypeChecker(lambda)
    println(name + ":\n" + PrettyPrinter(lambda))

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(lambda,
      identifier("mat", matT),
      identifier("xs", xsT),
      identifier("ys", ysT),
      identifier("alpha", ExpType(dataT)),
      identifier("beta", ExpType(dataT))
      )))
    println("----------------")
  }

  val mult = λ( x => x._1 * x._2 )
  val add = λ( x => λ( a => x + a))
  val scal = λ(xs => λ(alpha => map(λ( x => alpha * x ), xs) ) )
  val dot = λ(xs => λ(ys => reduce(add, 0.0f) o map(mult) $ zip(xs, ys) ) )

  val high_level =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        map(λ( x => x._1 + x._2 )) $
        zip(map(λ(row => alpha * dot(row)(xs)), mat), scal(ys)(beta))

    ) ) ) ) )

    printOpenCLKernel1("high_level", high_level)

  val fullMatrixVectorFusedOpenCL =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        join() o mapWorkgroup(λ(t =>
          mapLocal(λ(x => (alpha * x) + (t._2 * beta))) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)))
            o split(N) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      ) ) ) ) )

  printOpenCLKernel1("fullMatrixVectorFusedOpenCL", fullMatrixVectorFusedOpenCL)

  val fullMatrixVectorFusedOpenCLAMD =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        join() o mapWorkgroup(λ(t =>
          mapLocal(λ(x => x + (t._2 * beta))) o
            mapLocal(reduceSeq(add, 0.0f)) o split(128) o
            mapLocal(λ(x => alpha * x)) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)))
            o split(N /^ 128) o gather(reorderWithStride(128)) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      ) ) ) ) )

  TypeChecker(fullMatrixVectorFusedOpenCLAMD)
  println(xmlPrinter.asString(fullMatrixVectorFusedOpenCLAMD))

  printOpenCLKernel1("fullMatrixVectorFusedOpenCLAMD", fullMatrixVectorFusedOpenCLAMD)

}
