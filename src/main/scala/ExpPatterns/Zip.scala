package ExpPatterns

import CommandPatterns.MapI
import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import DSL._
import Compiling.RewriteToImperative
import apart.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Zip(lhs: Phrase[ExpType],
               rhs: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  private var n: ArithExpr = null
  private var dt1: DataType = null
  private var dt2: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(lhs), TypeChecker(rhs)) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_)))
        if n_ == m_ =>
        n = n_; dt1 = dt1_; dt2 = dt2_

        ExpType(ArrayType(n, RecordType(dt1, dt2)))
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Zip(VisitAndRebuild(lhs, f), VisitAndRebuild(rhs, f))
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

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val i = tupleAccess.head
    val rest = tupleAccess.tail

    if (i == Cst(1)) {
      return ToOpenCL.exp(lhs, env, arrayAccess, rest, dt)
    }

    if (i == Cst(2)) {
      return ToOpenCL.exp(rhs, env, arrayAccess, rest, dt)
    }

    throw new Exception("This should not happen")
  }

  override def prettyPrint: String =
    s"(zip ${PrettyPrinter(lhs)} ${PrettyPrinter(rhs)})"

  override def xmlPrinter: Elem =
    <zip n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <lhs type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Core.xmlPrinter(lhs)}
      </lhs>
      <rhs type={ToString(ExpType(ArrayType(n, dt2)))}>
        {Core.xmlPrinter(rhs)}
      </rhs>
    </zip>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    val E1 = lhs
    val E2 = rhs

    exp(E1)(λ(ExpType(ArrayType(n, dt1)))(x =>
      exp(E2)(λ(ExpType(ArrayType(n, dt2)))(y =>
        MapI(n, dt1 x dt2, dt1 x dt2, A,
          λ(A.t) { o =>
            λ(ExpType(RecordType(lhs.t.dataType, rhs.t.dataType))) { x =>
              acc(x)(o)
            }
          },
          Zip(x, y)
        )
      ))
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(lhs)(λ(lhs.t) { x =>
      exp(rhs)(λ(rhs.t) { y =>
        C(Zip(x, y))
      })
    })
  }
}