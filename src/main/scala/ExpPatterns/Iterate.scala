package ExpPatterns

import CommandPatterns.IterateI
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import apart.arithmetic._

case class Iterate(k: ArithExpr,
                   f: Phrase[ExpType -> ExpType],
                   array: Phrase[ExpType])
  extends ExpPattern {

  private var n: ArithExpr = null
  private var m: ArithExpr = null
  private var dt: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(m_, dt_)) =>
        m = m_; dt = dt_

        val l = NamedVar("l")
        setParamType(f, ExpType(ArrayType(l, dt)))
        TypeChecker(f) match {
          case FunctionType(ExpType(ArrayType(l_, dt1_)), ExpType(ArrayType(l_n, dt2_)))
            if l_.equals(l) && dt1_ == dt && dt2_ == dt =>
            l_n match {
              case Prod(l__ :: Pow(n_, Cst(-1)) :: Nil) if l__.equals(l) =>
                n = n_
            }
//            l_n /^ l match {
//              case Pow(n_, Cst(-1)) => n = n_
//            }

            ExpType(ArrayType(m / n.pow(k), dt))
          case ft => error(ft.toString, "FunctionType")
        }
      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    val i = Iterate(k, VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
    i.n = n
    i.m = m
    i.dt = dt
    i
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        var a = array
        for (_ <- 0 until k.eval) {
          a = fE(a)
        }
        OperationalSemantics.eval(s, a)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(iterate ${k.toString} ${PrettyPrinter(f)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    assert(n != null && m != null && k != null && dt != null)

    exp(array)(λ(ExpType(ArrayType(m, dt))) { x =>
      IterateI(n, m = m /^ n.pow(k), k, dt, A,
        λ(null.asInstanceOf[AccType]) { o =>
          λ(null.asInstanceOf[ExpType]) { x =>
            acc(f(x))(o) }
        },
        x
      )
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    `new`(t.dataType, GlobalMemory, tmp =>
      acc(this)(tmp.wr) `;`
      C(tmp.rd)
    )
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