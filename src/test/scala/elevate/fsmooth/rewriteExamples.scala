package elevate.fsmooth

import FSmooth.{Application, Let, ScalarValue, TypeInference}
import FSmooth.DSL._
import FSmooth.MSmooth.{matrixEye, matrixTranspose}
import elevate.core.strategies.basic.normalize
import elevate.core.strategies.traversal.oncetd
import elevate.fsmooth.rules._
import elevate.fsmooth.traversal._

class rewriteExamples extends test_util.Tests {

  test("Example5: Matrix Transpose") {
    val e = fun(M => matrixTranspose(matrixTranspose(M)))

    println(normalize.apply(buildGet <+ lengthBuild <+ letPartialEvaluation <+ conditionalPartialEvalution <+ conditionApplication <+ letApplication <+ funToLet <+ letFission <+ letInitDuplication).apply(e))
    println("--")
    println(e)
  }

  test("Example6: Simplification") {
    // val input = fun((v, I) => Let(I, Application(matrixEye, Seq(Application(len, Seq(v)))),
    //   Application(build, Seq(
    //     Application(len, Seq(v)),
    //     fun(i => Application(ifold, Seq(
    //       Application(len, Seq(v)), ScalarValue(0), fun((a, j) =>
    //         a + Application(get, Seq(v, j)) * Application(get, Seq(Application(get, Seq(I, j)), i))
    //       )))))
    //   )))

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
    val strategy0 = normalize.apply(funToLet <+ funToLet2 <+ letPartialEvaluation <+ buildGet <+ lengthBuild)
    //                       | ---- INLINING ------------------------------- | ----- FUSION ---------- |
    val step1 = strategy0.apply(inputFixed)
    println(step1)

    println("\n-- step2: conditional rules + ring structure rules")
    // conditionApplication2 handles binary functions
    val strategy1 = normalize.apply(conditionApplication2 <+ multiplicationZero <+ multiplicationOne <+ additionZero)
    val step2 = strategy1.apply(step1.get)
    println(step2)

    println("\n-- step3: loop normalisation")
    // this rule only applies if the ifold arguments are ordered correctly
    val strategy2 = oncetd(foldConditional)
    val step3 = strategy2.apply(step2.get)
    println(step3)

    println("\n-- step4: partial evaluation + simplification")
    val strategy3 = normalize.apply(letPartialEvaluation <+ additionZero)
    val step4 = strategy3.apply(step3.get)
    println(step4)
  }
}
