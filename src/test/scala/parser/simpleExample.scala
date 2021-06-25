import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.openCL.TypedDSL._
import rise.openCL.primitives.oclReduceSeq
import util._

class parseTest extends  test_util.TestsWithExecutor {
  private val v = vec(4, f32)
  private val add = foreignFun("add",
    Seq("a", "b"),
    """{
	  |  float4 res;
    |  res.xyz = a.xyz + b.xyz;
    |  return res;
    |  }
    |""".stripMargin,
    v->:v->:v
  )
  val matrixaddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
    (n`.`n`.`v)->:(n`.`n`.`v)->:(n`.`n`.`v)
  )((matA, matB)=>
    mapGlobal(1)(fun(row=>
      mapLocal(0)(fun(elem=>
          add(fst(elem))(snd(elem))
      )) $ zip(fst(row))(snd(row))
    )) $ zip(matA)(matB)
  ))

  val kernel = gen.OpenCLKernel(matrixaddition)
  println(kernel)
}
