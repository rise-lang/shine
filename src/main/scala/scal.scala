
import Core._
import DSL.untyped._
import OpenCL.Core.ToOpenCL
import OpenCL.DSL._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object scal extends App {

  val N = SizeVar("N")
  val dataT = float
  val inputT = ExpType(ArrayType(N, dataT))

  def printOpenCLCode(name: String,
                      untypedLambda: Phrase[ExpType ->(ExpType -> ExpType)]) = {
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
      lambda, identifier("input", inputT), identifier("alpha", ExpType(dataT)))))
    println("----------------")
  }

  val high_level = λ(inputT)(input => λ(ExpType(dataT))(alpha =>
    map(λ( x => alpha * x ), input)
  ) )

  printOpenCLCode("high_level", high_level)

  val scalWgLcl = (fst: ArithExpr, snd: ArithExpr) =>
    λ(inputT)(input => λ(ExpType(dataT))(alpha =>
      join() o mapWorkgroup(
        join() o mapLocal(mapSeq(
          λ(x => alpha * x)
        )) o split(snd)
      ) o split(fst) $ input
    ))

  printOpenCLCode("vectorScal", scalWgLcl(1024, 4))

  printOpenCLCode("scalAMD", scalWgLcl(128, 1))

  printOpenCLCode("scalNvidia", scalWgLcl(2048, 1))

  val scalIntelUntyped = λ(inputT)(input => λ(ExpType(VectorType(4, dataT)))(alpha =>
    join() o mapWorkgroup(
      asScalar() o join() o mapLocal(mapSeq(
        λ(x => alpha * x)
      )) o split(128) o asVector(4)
    ) o split(4 * 128 * 128) $ input
  ))

  val scalIntel = TypeInference(scalIntelUntyped)

  println("scalIntel" + ":\n" + PrettyPrinter(scalIntel))
  println(TypeChecker(scalIntel))

  println(s"-- scalIntel --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
    scalIntel, identifier("input", inputT), identifier("alpha", ExpType(VectorType(4, dataT))))))
  println("----------------")

}
