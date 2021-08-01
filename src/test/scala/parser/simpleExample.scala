import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types.AddressSpace.Local
import rise.core.types._
import rise.openCL.TypedDSL.toLocal
import rise.openCL.primitives.{mapGlobal, mapLocal, oclReduceSeq}
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
      mapGlobal(1, None)(fun(row=>
        mapLocal(0, None)(fun(elem=>
          add(fst(elem))(snd(elem))
        )) $ zip(fst(row))(snd(row))
      )) $ zip(matA)(matB)
    ))

    val kernel = gen.OpenCLKernel(matrixaddition)
    println(kernel)
  }

//  test("vektoradditionAndReduce"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float res;
//|  res= a.x + b.x;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoradditionAndReduce: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:f32
//    )((vecA, vecB)=>
//        reduceSeq(mul)(l(0f))(mapLocal(0)(fun(elem=>
//          add(fst(elem))(snd(elem))
//      )) $ zip(vecA)(vecB)
//    )))
//
//    val kernel = gen.OpenCLKernel(vektoradditionAndReduce)
//    println(kernel)
//  }
  test("vektoraddition"){
    val add = foreignFun("add",
      Seq("a", "b"),
      """{
	  |  float4 res;
|  res.xyz = a.xyz + b.xyz;
|  return res;
|  }
|""".stripMargin,
      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
    )
    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
    )((vecA, vecB)=>
      mapLocal(0, None)(fun(elem=>
        add(fst(elem))(snd(elem))
      )) $ zip(vecA)(vecB)
    ))

    val kernel = gen.OpenCLKernel(vektoraddition)
    println(kernel)
  }

  test("vektoraddition: high-level"){
    val add = foreignFun("add",
      Seq("a", "b"),
      """{
	  |  float4 res;
|  res.xyz = a.xyz + b.xyz;
|  return res;
|  }
|""".stripMargin,
      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
    )
    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
    )((vecA, vecB)=>
      map(fun(elem=>
        add(fst(elem))(snd(elem))
      )) $ zip(vecA)(vecB)
    ))

    val kernel = gen.OpenCLKernel(vektoraddition)
    println(kernel)
  }

//  test("vektoradditionFehler"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0)(fun(a==>
//        add(fst(a))(snd(a))
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }

//  test("vektoradditionFehler"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0)(fun(a=>
//        add(fst(a))()
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }
  test("vektoradditionFehler: WrongTypes"){
    val add = foreignFun("add",
      Seq("a", "b"),
      """{
	  |  float4 res;
|  res.xyz = a.xyz + b.xyz;
|  return res;
|  }
|""".stripMargin,
      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
    )
    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
    )((vecA, vecB)=>
      mapLocal(0, None)(fun(a=>
        add(fst(a))(a)
      )) $ zip(vecA)(vecB)
    ))

    val kernel = gen.OpenCLKernel(vektoraddition)
    println(kernel)
  }
//  test("vektoradditionFehler: ParenthesesError"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0, None)(fun(a=>
//        add(fst(a)))(snd(a))
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }
//  test("vektoradditionFehler: TooLongArrow"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0, None)(fun(a==>
//        add(fst(a))(snd(a))
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }
//  test("vektoradditionFehler: Empty Parentheses"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0, None)(fun(a=>
//        add(fst(a))()
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }
//  test("vektoradditionFehler: Semikolon not allowed"){
//    val add = foreignFun("add",
//      Seq("a", "b"),
//      """{
//	  |  float4 res;
//|  res.xyz = a.xyz + b.xyz;
//|  return res;
//|  }
//|""".stripMargin,
//      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(4, f32))->:(n`.`vec(4, f32))->:(n`.`vec(4, f32))
//    )((vecA, vecB)=>
//      mapLocal(0, None)(fun(a=>
//        add(fst(a))(snd(a));
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }

//  test("vektoraddition*2"){
//    val addMul = foreignFun("add",
//      Seq("a", "b", "ß"),
//      """{
//	  |  return a.x+b.x*ß;
//|  }
//|""".stripMargin,
//      vec(2, f32)->:vec(2, f32)->:f32
//    )
//    val vektoraddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`vec(2, f32))->:(n`.`vec(2, f32))->:(n`.`vec(2, f32))
//    )((vecA, vecB)=>
//      mapLocal(0)(fun(elem=>
//        addMul(fst(elem))(snd(elem))
//      )) $ zip(vecA)(vecB)
//    ))
//
//    val kernel = gen.OpenCLKernel(vektoraddition)
//    println(kernel)
//  }
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
      mapGlobal(1, None)(fun(row=>
        mapLocal(0, None)(fun(elem=>
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

//  test("two_times_square Fehler"){
//    val two_times_square: ToBeTyped[Expr] = fun(
//      f32->:f32
//    )(a=>
//      mul(l(2.0f)))(mul(a)(a))
//    )
//
//    val kernel = gen.OpenCLKernel(two_times_square)
//    println(kernel)
//  }
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

//  test("two_times_square: a has no type"){
//    val two_times_square: ToBeTyped[Expr] = mul(l(2.0))(mul(a)(a))
//
//    val thrown = intercept[rise.core.types.InferenceException] {
//      gen.OpenCLKernel(two_times_square)
//    }
//    println(thrown)
//  }
//  test("two_times_square: empty parentheses"){
//    val two_times_square: ToBeTyped[Expr] = mul(l(2.0))()
//
//    val thrown = intercept[rise.core.types.InferenceException] {
//      gen.OpenCLKernel(two_times_square)
//    }
//    println(thrown)
//  }
  test("reduceAdd: high-level"){
    val addArray: ToBeTyped[Expr] = depFun((n: Nat)=>fun((n`.`f32)->:f32)(
      arr=> reduce(add)(l(0f))$ arr
    ))
    val kernel = gen.CProgram(addArray)
    println(kernel)
  }
  test("reduceAdd"){
    val addArray: ToBeTyped[Expr] = depFun((n: Nat)=>fun((n`.`f32)->:f32)(
      arr=> reduceSeq(add)(l(0f))$ arr
    ))
    val kernel = gen.CProgram(addArray)
    println(kernel)
  }
  test("matMul"){
    val matMul: ToBeTyped[Expr] = depFun((r:Nat)=>depFun((m:Nat)=>depFun((n: Nat)=>fun((n`.`m`.`f32)->:
      (m`.`r`.`f32)->:(n`.`r`.`f32))(
      (matrixA,matrixB)=> mapLocal(1, None)(fun(rowA=>
        mapLocal(0, None)(
        fun(columnB=>
          oclReduceSeq(Local)(fun((acc,arg)=>add(acc)(arg)))
          (l(0f))(toLocal(mapSeq(fun(x=>mul(fst(x))(snd(x))))$ zip(rowA)(columnB)))
      ))(transpose(matrixB))
    )) $ matrixA))))
    val kernel = gen.CProgram(matMul)
    println(kernel)
//    val matrixaddition: ToBeTyped[Expr] = depFun((n: Nat)=>fun(
//      (n`.`n`.`v)->:(n`.`n`.`v)->:(n`.`n`.`v)
//    )((matA, matB)=>
//      mapGlobal(1, None)(fun(row=>
//        mapLocal(0, None)(fun(elem=>
//          add(fst(elem))(snd(elem))
//        )) $ zip(fst(row))(snd(row))
//      )) $ zip(matA)(matB)
//    ))
  }

  test("syntax ugly2"){
    val addVec: ToBeTyped[Expr] = foreignFun("addVec",
      Seq("a", "b"),
      """{
	  |  float4 res;
    |  res.xyz = a.xyz + b.xyz;
    |  return res;
    |  }
    |""".stripMargin,
      vec(4, f32)->:vec(4, f32)->:vec(4, f32)
    )
    val kernel = gen.OpenCLKernel(addVec)
    println(kernel)
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
