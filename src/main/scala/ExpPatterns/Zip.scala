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

case class Zip(n: ArithExpr,
               dt1: DataType,
               dt2: DataType,
               lhs: Phrase[ExpType],
               rhs: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    lhs.t =?= exp"[$n.$dt1]"
    rhs.t =?= exp"[$n.$dt2]"
    exp"[$n.($dt1 x $dt2)]"
  }

  override def inferTypes(): Zip = {
    import TypeInference._
    val lhs_ = TypeInference(lhs)
    val rhs_ = TypeInference(rhs)
    (lhs_.t, rhs_.t) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_)))
        if n_ == m_ =>
        Zip(n_, dt1_, dt2_, lhs_, rhs_)
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Zip(f(n), f(dt1), f(dt2), VisitAndRebuild(lhs, f), VisitAndRebuild(rhs, f))
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

    exp(E1)(λ(exp"[$n.$dt2]")(x =>
      exp(E2)(λ(exp"[$n.$dt2]")(y =>
        MapI(n, dt1 x dt2, dt1 x dt2, A,
          λ(A.t) { o =>
            λ(ExpType(RecordType(lhs.t.dataType, rhs.t.dataType))) { x =>
              acc(x)(o)
            }
          },
          Zip(n, dt1, dt2, x, y)
        )
      ))
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(lhs)(λ(lhs.t) { x =>
      exp(rhs)(λ(rhs.t) { y =>
        C(Zip(n, dt1, dt2, x, y))
      })
    })
  }
}