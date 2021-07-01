import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.openCL.TypedDSL._
import rise.openCL.primitives.oclReduceSeq
import util._

class parseTest extends  test_util.TestsWithExecutor {
  test("matrixaddition"){
    val v = vec(4, f32)
    val add = foreignFun("add",
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

  /*
  rise.core.types.InferenceException was thrown.
inference exception: could not solve constraints: List((_n185._n171.<4>f32 -> (_n185._n171.<4>f32 -> _n185._n171.<2>f64))  ~  (n132.n132.<4>f32 -> (n132.n132.<4>f32 -> n132.n132.<4>f32)))
---- trace ----

---------------
   */
  test("matrixaddition: falscher Typ"){
    val v1 = vec(4, f32)
    val v2 = vec(2, f64)
    val add = foreignFun("add",
      Seq("a", "b"),
      """{
	  |  float4 res;
|  res.xyz = a.xyz + b.xyz;
|  return res;
|  }
|""".stripMargin,
      v1->:v1->:v2
    )
    val matrixaddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
      (n`.`n`.`v1)->:(n`.`n`.`v1)->:(n`.`n`.`v1)
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

  test("two_times_square"){
    val two_times_square: ToBeTyped[Expr] = fun(
      f32->:f32
    )(a=>
      mul(l(2.0f))(mul(a)(a))
    )

    val kernel = gen.OpenCLKernel(two_times_square)
    println(kernel)
  }
  /*
  inference exception: could not solve constraints: List((f64 -> f64)  ~  (f32 -> f32))
---- trace ----

---------------
   */
  test("two_times_square: float_error"){
    val two_times_square: ToBeTyped[Expr] = fun(
      f32->:f32
    )(a=>
      mul(l(2.0))(mul(a)(a))
    )

    val thrown = intercept[rise.core.types.InferenceException] {
      gen.OpenCLKernel(two_times_square)
    }
    println(thrown)
  }

  /*
/home/visualjames/Documents/Universitaet/Bachelorarbeit/Repos/IntegrateRISE/shine/src/test/scala/parser/simpleExample.scala:110:10
type mismatch;
 found   : Unit
 required: rise.core.DSL.ToBeTyped[rise.core.Expr]
    )(x=>( )
   */
//  test("noExpressionInBraces"){
//    val f: ToBeTyped[Expr] = fun(
//      f32->:f32
//    )(x=>( )
//    )
//
//    val thrown = intercept[rise.core.types.InferenceException] {
//      gen.OpenCLKernel(f)
//    }
//    println(thrown)
//  }
  /*not found: value row
        mapGlobal(1)(fun(row==>
   */
//  test("matrixaddition"){
//    val v = vec(4, f32)
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      v->:v->:v
//    )
//    val matrixaddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`n`.`v)->:(n`.`n`.`v)->:(n`.`n`.`v)
//    )((matA, matB)=>
//      mapGlobal(1)(fun(row==>
//        mapLocal(0)(fun(elem=>
//          add(fst(elem))(snd(elem))
//        )) $ zip(fst(row))(snd(row))
//      )) $ zip(matA)(matB)
//    ))
//
//    val kernel = gen.OpenCLKernel(matrixaddition)
//    println(kernel)
//  }

}
