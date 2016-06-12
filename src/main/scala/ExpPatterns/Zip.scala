package ExpPatterns

import CommandPatterns.MapI
import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import DSL._
import Rewriting.RewriteToImperative
import apart.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLAST.Expression

case class Zip(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(lhs), TypeChecker(rhs)) match {
      case (ExpType(ArrayType(n, dt1)), ExpType(ArrayType(m, dt2))) if n == m =>
        ExpType(ArrayType(n, RecordType(dt1, dt2)))
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Zip(OperationalSemantics.substitute(phrase, `for`, lhs), OperationalSemantics.substitute(phrase, `for`, rhs))
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          RecordData(p._1, p._2)
        })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def toOpenCL: Expression = ???

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    val i = tupleAccess.head
    val rest = tupleAccess.tail

    if (i == Cst(1)) {
      return ToOpenCL.exp(lhs, arrayAccess, rest)
    }

    if (i == Cst(2)) {
      return ToOpenCL.exp(rhs, arrayAccess, rest)
    }

    throw new Exception("This should not happen")
  }

  override def prettyPrint: String = s"(zip ${PrettyPrinter(lhs)} ${PrettyPrinter(rhs)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.exp(lhs, λ(lhs.t) { x =>
      RewriteToImperative.exp(rhs, λ(rhs.t) { y =>
        MapI(A,
          λ(A.t) { o => λ(ExpType(RecordType(lhs.t.dataType, rhs.t.dataType))) { x => RewriteToImperative.acc(x, o) } },
          Zip(x, y)
        )
      })
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(lhs, λ(lhs.t) { x =>
      RewriteToImperative.exp(rhs, λ(rhs.t) { y =>
        C(Zip(x, y))
      })
    })
  }
}