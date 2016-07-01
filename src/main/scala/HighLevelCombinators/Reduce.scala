package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL._
import MidLevelCombinators.{ReduceIAcc, ReduceIExp}
import apart.arithmetic.ArithExpr

import scala.xml.Elem

abstract class AbstractReduce(n: ArithExpr,
                              dt1: DataType,
                              dt2: DataType,
                              f: Phrase[ExpType -> (ExpType -> ExpType)],
                              init: Phrase[ExpType],
                              array: Phrase[ExpType],
                              makeReduce: (ArithExpr, DataType, DataType, Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) => AbstractReduce,
                              makeReduceIAcc: (ArithExpr, DataType, DataType, Phrase[AccType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIAcc,
                              makeReduceIExp: (ArithExpr, DataType, DataType, Phrase[ExpType -> CommandType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIExp)
  extends HighLevelCombinator {

  override lazy val `type` = exp"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    f checkType t"exp[$dt1] -> exp[$dt2] -> exp[$dt2]"
    init checkType exp"[$dt2]"
    array checkType exp"[$n.$dt1]"
  }

  override def inferTypes: AbstractReduce = {
    import TypeInference._
    val array_ = TypeInference(array)
    val init_ = TypeInference(init)
    (init_.t, array_.t) match {
      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_))) =>
        val f_ = TypeInference.setParamsAndInferTypes(f, exp"[$dt1_]", exp"[$dt2_]")
        f_.t match {
          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
            if (dt1_ == t1 && dt2_ == t2 && dt2_ == t3) {
              makeReduce(n_, dt1_, dt2_, f_, init_, array_)
            } else {
              error(dt1_.toString + ", " + t1.toString + " as well as " +
                dt2_.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "(ExpType, ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    makeReduce(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s, fE(LiteralPhrase(x))(LiteralPhrase(y)))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    val F = f
    val I = init
    val E = array

    exp(E)(λ(ExpType(ArrayType(n, dt1)))(x =>
      exp(I)(λ(ExpType(dt2))(y =>
        makeReduceIAcc(n, dt1, dt2, A,
          λ(AccType(dt2))(o =>
            λ(ExpType(dt1))(x =>
              λ(ExpType(dt2))(y => acc(F(x)(y))(o)))),
          y,
          x
        )
      ))
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    exp(array)(λ(ExpType(ArrayType(n, dt1))) { x =>
      exp(init)(λ(ExpType(dt2)) { y =>
        makeReduceIExp(n, dt1, dt2, C,
          λ(AccType(dt2)) { o =>
            λ(ExpType(dt1)) { x =>
              λ(ExpType(dt2)) { y => acc(f(x)(y))(o) }
            }
          },
          y,
          x
        )
      })
    })
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> (ExpType(dt2) -> ExpType(dt2)))}>
        {Core.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2))}>
        {Core.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Core.xmlPrinter(array)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

case class Reduce(n: ArithExpr,
                  dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array, Reduce, ReduceIAcc, ReduceIExp)
