package rise.autotune

import arithexpr.arithmetic.{ArithExpr}
import rise.autotune
import rise.core.{DepLambda, Expr}
import rise.core.types.{Nat, NatIdentifier, NatKind, TuningParameter}

class TestConstraints extends test_util.Tests {

  test("collect constraints") {
    val e: Expr = util.expressions.convolution.convolutionOclGsLsWrap
    autotune.constraints.collectConstraints(e,
      autotune.constraints.collectParameters(e))
      .foreach(println)
  }

  test("mm kernel constraints") {

    val e: Expr = util.expressions.mm.mmOclGsLsWrap

    val (nIdent, mIdent, oIdent) = e match {
      case DepLambda(NatKind, n: NatIdentifier, DepLambda(NatKind, m: NatIdentifier, DepLambda(NatKind, o: NatIdentifier, _))) =>
        (n, m, o)
      case _ => ???
    }

    val params = autotune.constraints.collectParameters(e)
    val constraints = autotune.constraints.collectConstraints(e, params)
    // note: v5 multiple of 4, contiguous memory constraint is missing

    // n, m, o, v3, v4, v5, v6, v7, v8, ls0, ls1, gs0, gs1
    val badParameters = Seq[(Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)](
      (64, 128, 128, 8, 1, 1, 1, 16, 1, 32, 4, 32, 16),
      (64, 128, 128, 1, 1, 1, 32, 1, 128, 64, 2, 64, 128),
      (64, 128, 128, 2, 1, 1, 2, 32, 32, 1, 1, 4, 64),
      (64, 128, 128, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1),
      (64, 128, 128, 1, 1, 1, 1, 8, 4, 4, 1, 64, 1),
      (64, 128, 128, 1, 1, 1, 1, 1, 128, 2, 2, 2, 8),
      (64, 128, 128, 1, 1, 1, 2, 1, 2, 2, 2, 4, 128),
      (64, 128, 128, 2, 1, 1, 1, 2, 4, 4, 4, 16, 64),
      (64, 128, 128, 2, 1, 1, 2, 8, 2, 4, 2, 4, 2),
      (64, 128, 128, 8, 1, 1, 1, 8, 4, 8, 64, 8, 64),
      (64, 128, 128, 16, 2, 2, 1, 16, 2, 64, 4, 64, 8),
      (64, 128, 128, 1, 1, 2, 2, 128, 4, 16, 16, 64, 64),
      (64, 128, 128, 1, 1, 2, 4, 1, 16, 4, 1, 4, 32),
      (64, 128, 128, 1, 1, 2, 1, 4, 128, 4, 2, 64, 4),
      (64, 128, 128, 2, 1, 2, 4, 2, 64, 2, 1, 128, 1),
      (64, 128, 128, 16, 2, 2, 2, 32, 8, 2, 1, 128, 1),
      (64, 128, 128, 2, 2, 2, 2, 16, 8, 4, 1, 4, 32),
      (64, 128, 128, 1, 2, 2, 1, 1, 1, 4, 1, 4, 4),
      (64, 128, 128, 8, 2, 2, 1, 8, 32, 8, 2, 32, 2),
      (64, 128, 128, 32, 2, 2, 2, 64, 16, 1, 2, 16, 32),
      (64, 128, 128, 2, 4, 4, 16, 8, 64, 8, 1, 8, 8),
      (64, 128, 128, 1, 1, 4, 32, 8, 64, 2, 4, 128, 4),
      (64, 128, 128, 1, 2, 4, 4, 1, 1, 16, 1, 16, 1),
      (64, 128, 128, 1, 2, 4, 128, 8, 128, 2, 1, 2, 32),
      (64, 128, 128, 1, 1, 4, 16, 16, 32, 1, 128, 32, 128),
      (64, 128, 128, 1, 1, 4, 32, 8, 8, 16, 8, 64, 8),
      (64, 128, 128, 2, 1, 4, 64, 8, 32, 2, 8, 4, 64),
      (64, 128, 128, 4, 1, 4, 2, 32, 2, 4, 8, 8, 64),
      (64, 128, 128, 2, 1, 8, 2, 4, 128, 2, 2, 2, 16),
      (64, 128, 128, 1, 4, 8, 8, 2, 16, 4, 2, 8, 4),
      (64, 128, 128, 2, 8, 8, 8, 128, 2, 2, 1, 4, 2),
      (64, 128, 128, 1, 4, 8, 16, 4, 4, 1, 2, 128, 2),
      (64, 128, 128, 1, 1, 8, 1, 4, 1, 1, 16, 1, 32),
      (64, 128, 128, 2, 8, 8, 8, 8, 1, 8, 4, 8, 16),
      (64, 128, 128, 2, 1, 16, 16, 8, 32, 1, 2, 1, 128),
      (64, 128, 128, 2, 2, 16, 4, 2, 4, 2, 1, 128, 2),
      (64, 128, 128, 2, 1, 16, 16, 8, 8, 16, 2, 16, 2),
      (64, 128, 128, 1, 4, 16, 32, 8, 128, 1, 1, 2, 8),
      (64, 128, 128, 1, 4, 16, 128, 32, 32, 1, 4, 1, 16),
      (64, 128, 128, 1, 1, 16, 4, 1, 1, 1, 4, 32, 32),
      (64, 128, 128, 2, 1, 32, 1, 4, 8, 8, 2, 128, 2),
      (64, 128, 128, 1, 4, 32, 1, 4, 64, 1, 8, 1, 8),
      (64, 128, 128, 1, 1, 32, 2, 1, 8, 8, 1, 64, 1),
      (64, 128, 128, 1, 8, 32, 32, 1, 64, 1, 1, 1, 64),
      (64, 128, 128, 1, 32, 32, 16, 64, 32, 1, 1, 8, 16),
      (64, 128, 128, 2, 32, 32, 4, 4, 2, 16, 8, 32, 64),
      (64, 128, 128, 32, 8, 32, 2, 32, 1, 2, 8, 2, 8),
      (64, 128, 128, 2, 8, 64, 4, 4, 64, 1, 8, 1, 32),
      (64, 128, 128, 2, 1, 64, 16, 2, 4, 1, 1, 4, 64),
      (64, 128, 128, 1, 4, 64, 8, 1, 128, 1, 2, 16, 4),
      (64, 128, 128, 2, 16, 64, 2, 2, 8, 2, 2, 2, 2),
      (64, 128, 128, 1, 8, 64, 1, 1, 1, 1, 1, 1, 2),
      (64, 128, 128, 1, 1, 64, 1, 2, 8, 32, 64, 64, 128),
      (64, 128, 128, 1, 64, 64, 64, 1, 4, 1, 2, 1, 16),
      (64, 128, 128, 2, 64, 64, 4, 2, 1, 2, 8, 2, 32),
      (64, 128, 128, 64, 32, 64, 1, 64, 64, 32, 1, 128, 64),
      (64, 128, 128, 32, 32, 128, 1, 64, 4, 64, 1, 64, 1),
      (64, 128, 128, 4, 2, 128, 32, 4, 8, 1, 1, 8, 4),
      (64, 128, 128, 4, 128, 128, 1, 4, 32, 2, 16, 2, 64),
      (64, 128, 128, 1, 8, 128, 128, 1, 32, 2, 2, 4, 16),
      (64, 128, 128, 1, 32, 128, 2, 1, 128, 4, 1, 64, 8),
      (64, 128, 128, 1, 8, 128, 8, 4, 128, 4, 8, 8, 16),
      (64, 128, 128, 8, 128, 128, 128, 16, 16, 8, 64, 16, 64),
      (64, 128, 128, 16, 64, 128, 1, 128, 128, 1, 2, 1, 4),
      (64, 128, 128, 16, 128, 128, 8, 32, 8, 1, 2, 1, 2),
      (64, 128, 128, 2, 1, 1, 8, 4, 16, 2, 2, 2, 2),
    )
    val goodParameters = Seq[(Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)](
      (64, 128, 128, 4, 1, 1, 4, 4, 8, 8, 4, 8, 32),
      (64, 128, 128, 4, 1, 1, 8, 64, 16, 1, 16, 4, 32),
      (64, 128, 128, 4, 1, 1, 8, 128, 64, 1, 8, 8, 8),
      (64, 128, 128, 4, 1, 2, 4, 64, 128, 4, 2, 4, 4),
      (64, 128, 128, 64, 2, 2, 32, 64, 32, 1, 1, 2, 32),
      (64, 128, 128, 4, 1, 2, 8, 8, 64, 32, 2, 32, 16),
      (64, 128, 128, 4, 1, 2, 32, 4, 32, 4, 4, 4, 4),
    )
    for ((params, isGood) <- Seq((badParameters, false), (goodParameters, true))) {
      params.foreach { case cfg@(n, m, o, v3, v4, v5, v6, v7, v8, ls0, ls1, gs0, gs1) =>
        val map = Map(
          nIdent -> n,
          mIdent -> m,
          oIdent -> o,
          TuningParameter("v3") -> v3,
          TuningParameter("v4") -> v4,
          TuningParameter("v5") -> v5,
          TuningParameter("v6") -> v6,
          TuningParameter("v7") -> v7,
          TuningParameter("v8") -> v8,
          TuningParameter("ls0") -> ls0,
          TuningParameter("ls1") -> ls1,
          TuningParameter("gs0") -> gs0,
          TuningParameter("gs1") -> gs1,
        )
        if (autotune.constraints.checkConstraints(constraints, map) != isGood) {
          val (sat, notSat) = constraints.partition(c =>
            c.substitute(map.toMap[ArithExpr, ArithExpr]).isSatisfied())
          println("satisfied:")
          sat.foreach(println)
          println("not satisfied:")
          notSat.foreach(println)
          throw new Exception(s"$cfg should${if (isGood) "" else " not"} pass constraint checking")
        }
      }
    }
  }
}
