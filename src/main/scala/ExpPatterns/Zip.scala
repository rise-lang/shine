package ExpPatterns

import CommandPatterns.MapI
import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import DSL._
import Compiling.RewriteToImperative
import Core.PrettyPrinter.Indent
import apart.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLAST.Expression

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

  override def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    val i = tupleAccess.head
    val rest = tupleAccess.tail

    if (i == Cst(1)) {
      return ToOpenCL.exp(lhs, ocl, arrayAccess, rest)
    }

    if (i == Cst(2)) {
      return ToOpenCL.exp(rhs, ocl, arrayAccess, rest)
    }

    throw new Exception("This should not happen")
  }

  override def prettyPrint(indent: Indent): String =
    indent + s"(zip\n" +
      s"${PrettyPrinter(lhs, indent.more)} : exp[$n.$dt1]\n" +
      s"${PrettyPrinter(rhs, indent.more)} : exp[$n.$dt2]\n" +
      indent + s") : exp[$n.($dt1 x $dt2)]"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(lhs)(λ(lhs.t) { x =>
      exp(rhs)(λ(rhs.t) { y =>
        MapI(n, RecordType(dt1, dt2), RecordType(dt1, dt2), A,
          λ(A.t) { o =>
            λ(ExpType(RecordType(lhs.t.dataType, rhs.t.dataType))) { x =>
              acc(x)(o) } },
          Zip(x, y)
        )
      })
    })
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