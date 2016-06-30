package ExpPatterns

import CommandPatterns.{IterateIAcc, IterateIExp}
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import apart.arithmetic._

import scala.xml.Elem

case class Iterate(n: ArithExpr,
                   m: ArithExpr,
                   k: ArithExpr,
                   dt: DataType,
                   f: Phrase[`(nat)->`[ExpType -> ExpType]],
                   array: Phrase[ExpType])
  extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    f match {
      case NatDependentLambdaPhrase(l, _) =>
        f.t =?= t"($l : nat) -> exp[$l.$dt] -> exp[${l /^ n}.$dt]"
      case _ => throw new Exception("This should not happen")
    }
    array.t =?= exp"[$m.$dt]"
    exp"[${m /^ n.pow(k)}.$dt]"
  }

  override def inferTypes(): Iterate = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(m_, dt_)) =>
        f match {
          case NatDependentLambdaPhrase(l, body) =>
            val f_ = f.copy(body=TypeInference.setParamType(body, exp"[$l.$dt_]"))
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
          case _ => error(f.toString, NatDependentLambdaPhrase.toString)
        }
      case t_ => error(t_.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Iterate(fun(n), fun(m), k, fun(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    //    val fE = OperationalSemantics.eval(s, f)
    //    OperationalSemantics.eval(s, array) match {
    //      case ArrayData(xs) =>
    //        var a = array
    //        for (_ <- 0 until k.eval) {
    //          a = fE(a)
    //        }
    //        OperationalSemantics.eval(s, a)
    //      case _ => throw new Exception("This should not happen")
    //    }
    ???
  }

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambdaPhrase(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l -> (ExpType(ArrayType(l, dt)) -> ExpType(ArrayType(l /^ n, dt))))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt)))}>
        {Core.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    assert(n != null && m != null && k != null && dt != null)

    exp(array)(λ(ExpType(ArrayType(m, dt))) { x =>
      IterateIAcc(n, m = m /^ n.pow(k), k, dt, A,
        _Λ_(l =>
          λ(null.asInstanceOf[AccType]) { o =>
            λ(null.asInstanceOf[ExpType]) { x =>
              acc(f(l)(x))(o)
            }
          }
        ),
        x
      )
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    assert(n != null && m != null && k != null && dt != null)

    exp(array)(λ(ExpType(ArrayType(m, dt))) { x =>
      IterateIExp(n, m = m /^ n.pow(k), k, dt, C,
        _Λ_(l =>
          λ(null.asInstanceOf[AccType]) { o =>
            λ(null.asInstanceOf[ExpType]) { x =>
              acc(f(l)(x))(o)
            }
          }
        ),
        x
      )
    })
  }

  /*
    exp(array)(λ( ExpType(ArrayType(n, dt1)) ) { x =>
        makeMapI(A,
          λ( AccType(dt2) ) { o =>
            λ( ExpType(dt1) ) { x => acc(f(x))(o) } },
          x
        )
      })

    `new`(ArrayType(n, dt2), GlobalMemory, tmp =>
        acc(this)(tmp.wr) `;`
        C(tmp.rd)
      )
     */
}