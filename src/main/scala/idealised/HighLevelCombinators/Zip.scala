package idealised.HighLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._
import idealised.MidLevelCombinators.MapI

import scala.xml.Elem

final case class Zip(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     lhs: Phrase[ExpType],
                     rhs: Phrase[ExpType])
  extends HighLevelCombinator {

  override lazy val `type` = exp"[$n.($dt1 x $dt2)]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (lhs :: exp"[$n.$dt1]") ->
      (rhs :: exp"[$n.$dt2]") ->
      `type`
  }

  override def inferTypes: Zip = {
    import TypeInference._
    val lhs_ = TypeInference(lhs)
    val rhs_ = TypeInference(rhs)
    (lhs_.t, rhs_.t) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_))) =>
        if (n_ == m_)
          Zip(n_, dt1_, dt2_, lhs_, rhs_)
        else
          error(s"Length $n_ and $m_ does not match")
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
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

    exp(lhs)(λ(exp"[$n.$dt1]")(x =>
      exp(rhs)(λ(exp"[$n.$dt2]")(y =>
        MapI(n, dt1 x dt2, dt1 x dt2, A,
          λ(acc"[$dt1 x $dt2]")(o => λ(exp"[$dt1 x $dt2]")(x =>
            acc(x)(o))),
          Zip(n, dt1, dt2, x, y)
        )
      ))
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    exp(lhs)(λ(exp"[$n.$dt1]")(x =>
      exp(rhs)(λ(exp"[$n.$dt2]")(y =>
        C(Zip(n, dt1, dt2, x, y))
      ))
    ))
  }
}