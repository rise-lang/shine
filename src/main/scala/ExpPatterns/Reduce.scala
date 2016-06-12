package ExpPatterns

import CommandPatterns.{ReduceIAcc, ReduceIExp}
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Rewriting.RewriteToImperative
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

abstract class AbstractReduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                              init: Phrase[ExpType],
                              array: Phrase[ExpType],
                              makeReduce: (Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) => AbstractReduce,
                              makeReduceIAcc: (Phrase[AccType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIAcc,
                              makeReduceIExp: (Phrase[ExpType -> CommandType], Phrase[AccType -> (ExpType -> (ExpType -> CommandType))], Phrase[ExpType], Phrase[ExpType]) => ReduceIExp)
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
          case t => error(t.toString, "FunctionType")
        }
      case t => error(t.toString, "(ExpType, ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    val r = makeReduce(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, array))
    r.t = t
    r.n = n
    r.dt1 = dt1
    r.dt2 = dt2
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

  override def toOpenCL: Expression = ???

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = ???

  override def prettyPrint: String = s"(${this.getClass.getSimpleName} ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)

    RewriteToImperative.exp(array, λ( ExpType(ArrayType(n, dt1)) ) { x =>
      RewriteToImperative.exp(init, λ( ExpType(dt2) ) { y =>
        makeReduceIAcc(A,
          λ( AccType(dt2) ) { o =>
            λ( ExpType(dt1) ) { x =>
              λ( ExpType(dt2) ) { y => RewriteToImperative.acc(f(x)(y), o) } } },
          y,
          x
        )
      })
    })
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)

    RewriteToImperative.exp(array, λ( ExpType(ArrayType(n, dt1)) ) { x =>
      RewriteToImperative.exp(init, λ( ExpType(dt2) ) { y =>
        makeReduceIExp(C,
          λ( AccType(dt2) ) { o =>
            λ( ExpType(dt1) ) { x =>
              λ( ExpType(dt2) ) { y => RewriteToImperative.acc(f(x)(y), o) } } },
          y,
          x
        )
      })
    })
  }

}

case class Reduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array, Reduce, ReduceIAcc, ReduceIExp)
