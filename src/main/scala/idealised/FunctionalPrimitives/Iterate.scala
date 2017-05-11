package idealised.FunctionalPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.{IterateIAcc, IterateIExp}
import lift.arithmetic._

import scala.xml.Elem

final case class Iterate(n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->`[ExpType -> ExpType]],
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = {
    if (m != null && n != null) {
      exp"[${m /^ n.pow(k)}.$dt]"
    } else {
      exp"[${null}.$dt]"
    }
  }

  override def typeCheck(): Unit = {
    import TypeChecker._
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (f :: t"($l : nat) -> exp[$l.$dt] -> exp[${l /^ n}.$dt]") ->
          (array :: exp"[$m.$dt]") ->
          `type`
      case _ => throw new Exception("This should not happen")
    }
  }

  override def inferTypes: Iterate = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(m_, dt_)) =>
        f match {
          case NatDependentLambda(l, body) =>
            val b = TypeInference.setParamAndInferType(body, exp"[$l.$dt_]")
            val f_ = NatDependentLambda(l, b) //f.copy(body=b)
            f_.t match {
              case NatDependentFunctionType(_,
              FunctionType(ExpType(ArrayType(l_, dt1_)),
              ExpType(ArrayType(l_n, dt2_)))) =>
                if (l == l_ && dt1_ == dt_ && dt2_ == dt_) {

                  val n_ = l_n match {
                    case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
                    case _ => error(s"$l_n", s"${l_ / n}")
                  }

                  Iterate(n_, m_, k, dt_, f_, array_)
                } else {
                  error(s"expected $l == $l_ && $dt1_ == $dt_ && $dt2_ == $dt_")
                }
              case ft => error(ft.toString, NatDependentFunctionType.toString)
            }
          case _ => error(f.toString, NatDependentLambda.toString)
        }
      case t_ => error(t_.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Iterate(fun(n), fun(m), k, fun(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
//    val fE = OperationalSemantics.eval(s, f)
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(xs) =>
//        var a = array
//        for (i <- 0 until k.eval) {
//          a = fE(a)
//        }
//        OperationalSemantics.eval(s, a)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l -> (ExpType(ArrayType(l, dt)) -> ExpType(ArrayType(if (l != null && n != null) l /^ n else null, dt))))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt)))}>
        {Core.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import idealised.Compiling.RewriteToImperative._

    assert(n != null && m != null && k != null && dt != null)

    con(array)(λ(exp"[$m.$dt]")(x =>
      IterateIAcc(n, m = m /^ n.pow(k), k, dt, A,
        _Λ_(l => λ(acc"[${l /^ n}.$dt]")(o => λ(exp"[$l.$dt]")(x =>
          acc(f(l)(x))(o)))),
        x
      )
    ))
  }

  override def rewriteToImperativeCon(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import idealised.Compiling.RewriteToImperative._

    assert(n != null && m != null && k != null && dt != null)

    con(array)(λ(exp"[$m.$dt]")(x =>
      IterateIExp(n, m = m /^ n.pow(k), k, dt, C,
        _Λ_(l => λ(acc"[${l /^ n}.$dt]")(o => λ(exp"[$l.$dt]")(x =>
          acc(f(l)(x))(o)))),
        x
      )
    ))
  }
}
