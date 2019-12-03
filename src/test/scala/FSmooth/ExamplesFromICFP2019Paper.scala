package FSmooth

import FSmooth.DSL._
import FSmooth.MSmooth._
import elevate.core.strategies.basic.repeat
import elevate.core.strategies.traversal.oncetd
import elevate.core.Strategy
import elevate.fsmooth.FSmooth
import elevate.fsmooth.rules._
import elevate.fsmooth.traversal._

class ExamplesFromICFP2019Paper extends test_util.Tests {

  test("Type inference MSmooth vector constructs") {
    println("vectorRange = " + TypeInference.infer(vectorRange))

    println("vectorFill = " + TypeInference.infer(vectorFill))

    println("vectorHot = " + TypeInference.infer(vectorHot))

    println("vectorMap = " + TypeInference.infer(vectorMap))

    println("vectorMap2 = " + TypeInference.infer(vectorMap2))

    println("vectorZip = " + TypeInference.infer(vectorZip))

    println("vectorAdd = " + TypeInference.infer(vectorAdd))

    println("vectorEMul = " + TypeInference.infer(vectorEMul))

    println("vectorSMul = " + TypeInference.infer(vectorSMul))

    println("vectorSum = " + TypeInference.infer(vectorSum))

    println("vectorDot = " + TypeInference.infer(vectorDot))

    println("vectorNorm = " + TypeInference.infer(vectorNorm))

//    println("vectorSlice = " + TypeInference.infer(vectorSlice))

    println("vectorToMatrix = " + TypeInference.infer(vectorToMatrix))

    println("vectorOutProd = " + TypeInference.infer(vectorOutProd))
  }

  test("Type inference MSmooth matrix constructs") {
    println("matrixRows = " + TypeInference.infer(matrixRows))

    println("matrixCols = " + TypeInference.infer(matrixCols))

    println("matrixZeros = " + TypeInference.infer(matrixZeros))

    println("matrixOnes = " + TypeInference.infer(matrixOnes))

    println("matrixEye = " + TypeInference.infer(matrixEye))

    println("matrixHot = " + TypeInference.infer(matrixHot))

    println("matrixMap = " + TypeInference.infer(matrixMap))

    println("matrixMap2 = " + TypeInference.infer(matrixMap2))

    println("matrixAdd = " + TypeInference.infer(matrixAdd))

    println("matrixTranspose = " + TypeInference.infer(matrixTranspose))

    println("matrixMult = " + TypeInference.infer(matrixMult))

    println("matrixTrace = " + TypeInference.infer(matrixTrace))
  }

  test("Example1: uMv^T") {
    val f = fun( (u, M, v) =>
      let( vectorToMatrix(u) ).beIn(um =>
      let( matrixTranspose(vectorToMatrix(v)) ).beIn(vt =>
      let( matrixMult(um, matrixMult(M, vt)) ).beIn(m =>
      m.get(idx(0)).get(idx(0))
      )))
    )
    println(f)
    println("f = " + TypeInference.infer(f))
  }

  // todo generalize
  def normalize: Strategy[FSmooth] => Strategy[FSmooth] =
    s => repeat(oncetd(s))

  test("Example5: Matrix Transpose") {
    val e = fun(M => matrixTranspose(matrixTranspose(M)))

    println(normalize(buildGet <+ lengthBuild <+ letPartialEvaluation <+ conditionalPartialEvalution <+ conditionApplication <+ letApplication <+ funToLet <+ letFission <+ letInitDuplication).apply(e))
    println("--")
    println(e)
  }

  test("matrixEye typechecks") {
    val expr = matrixEye
    println(expr)
    TypeInference.infer(expr)
  }

  test("Example6: Simplification") {
    val input = fun((v, I) => Let(I, Application(matrixEye, Seq(Application(len, Seq(v)))),
      Application(build, Seq(
        Application(len, Seq(v)),
        fun(i => Application(ifold, Seq(
          Application(len, Seq(v)), ScalarValue(0), fun((a, j) =>
            a + Application(get, Seq(v, j)) * Application(get, Seq(Application(get, Seq(I, j)), i))
        )))))
    )))

    // the order of ifold arguments is incorrect in the paper
    val inputFixed = fun((v, I) => Let(I, Application(matrixEye, Seq(Application(len, Seq(v)))),
      Application(build, Seq(
        Application(len, Seq(v)),
        fun(i => Application(ifold, Seq(
          fun((a, j) =>
            a + Application(get, Seq(v, j)) * Application(get, Seq(Application(get, Seq(I, j)), i))
          ),
          ScalarValue(0),
          Application(len, Seq(v))
        ))))
      )))

    println(inputFixed)
    TypeInference.infer(inputFixed)

    println("\n-- step1: inlining + fusing")
    // funToLet2 required because of matrixEye + vectorHot definition
    val strategy0 = normalize(funToLet <+ funToLet2 <+ letPartialEvaluation <+ buildGet <+ lengthBuild)
    //                       | ---- INLINING ------------------------------- | ----- FUSION ---------- |
    val step1 = strategy0.apply(inputFixed)
    println(step1)

    println("\n-- step2: conditional rules + ring structure rules")
    // conditionApplication2 handles binary functions
    val strategy1 = normalize(conditionApplication2 <+ multiplicationZero <+ multiplicationOne <+ additionZero)
    val step2 = strategy1.apply(step1.get)
    println(step2)

    println("\n-- step3: loop normalisation")
    // this rule only applies if the ifold arguments are ordered correctly
    val strategy2 = oncetd(foldConditional)
    val step3 = strategy2.apply(step2.get)
    println(step3)

    println("\n-- step4: partial evaluation + simplification")
    val strategy3 = normalize(letPartialEvaluation <+ additionZero)
    val step4 = strategy3.apply(step3.get)
    println(step4)
  }

  test("Free variables") {
    val y = Variable(freshName("y"))
    val z = Variable(freshName("z"))
    assert( Differentiation.fvs( fun(x => x) ) == Seq() )
    assert( Differentiation.fvs( fun(x => x + y) ) == Seq(y) )
    assert( Differentiation.fvs( fun(x => z + x + y) ) == Seq(z, y) )
  }
}
