package ExpPatterns

import CommandPatterns.{ReduceIAcc, ReduceIExp}
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Compiling.RewriteToImperative
import apart.arithmetic.ArithExpr

import scala.xml.Elem

abstract class AbstractReduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                              init: Phrase[ExpType],
                              array: Phrase[ExpType],
                              makeReduce: (Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) => AbstractReduce,
                              makeReduceIAcc: (ArithExpr, DataType, DataType, Phrase[AccType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIAcc,
                              makeReduceIExp: (ArithExpr, DataType, DataType, Phrase[ExpType -> CommandType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIExp)
  extends ExpPattern {

  protected var n: ArithExpr = null
  protected var dt1: DataType = null
  protected var dt2: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(init), TypeChecker(array)) match {
      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_))) =>
        n = n_; dt1 = dt1_; dt2 = dt2_
        setParamType(f, ExpType(dt1))
        setSecondParamType(f, ExpType(dt2))
        TypeChecker(f) match {
          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
            if (dt1 == t1 && dt2 == t2 && dt2 == t3) ExpType(dt2)
            else {
              error(dt1.toString + ", " + t1.toString + " as well as " +
                dt2.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "(ExpType, ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    val r = makeReduce(VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
    r.n = fun(n)
    r.dt1 = fun(dt1)
    r.dt2 = fun(dt2)
    r
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

    exp(array)(λ( ExpType(ArrayType(n, dt1)) ) { x =>
      exp(init)(λ( ExpType(dt2) ) { y =>
        makeReduceIAcc(n, dt1, dt2, A,
          λ( AccType(dt2) ) { o =>
            λ( ExpType(dt1) ) { x =>
              λ( ExpType(dt2) ) { y => acc( f(x)(y) )( o ) } } },
          y,
          x
        )
      })
    })
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    exp(array)(λ( ExpType(ArrayType(n, dt1)) ) { x =>
      exp(init)(λ( ExpType(dt2) ) { y =>
        makeReduceIExp(n, dt1, dt2, C,
          λ( AccType(dt2) ) { o =>
            λ( ExpType(dt1) ) { x =>
              λ( ExpType(dt2) ) { y => acc( f(x)(y) )( o ) } } },
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

case class Reduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array, Reduce, ReduceIAcc, ReduceIExp)
