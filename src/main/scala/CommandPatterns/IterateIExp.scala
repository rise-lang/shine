package CommandPatterns

import AccPatterns.TruncAcc
import Compiling.RewriteToImperative
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import ExpPatterns.TruncExp
import apart.arithmetic._

case class IterateIExp(n: ArithExpr,
                       m: ArithExpr,
                       k: ArithExpr,
                       dt: DataType,
                       out: Phrase[ExpType -> CommandType],
                       f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                       in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    TypeChecker(in) match {
      case ExpType(ArrayType(nkm_, dt1_))
        if nkm_ == (n.pow(k) * m) && dt1_ == dt =>

        setParamType(out, ExpType(ArrayType(m, dt)))
        TypeChecker(out) match {
          case FunctionType(ExpType(ArrayType(m_, dt2_)), CommandType()) =>
            if (m_ != m || dt2_ != dt)
              error(m_.toString + " and " + m.toString + "; " +
                dt2_.toString + " and " + dt.toString, expected = "them to match")
          case x => error(x.toString, "FunctionType")
        }

        f match {
          case NatDependentLambdaPhrase(l, body) =>
            setParamType(body, AccType(ArrayType(l /^ n, dt)))
            setSecondParamType(body, ExpType(ArrayType(l, dt)))
            TypeChecker(body) match {
              case FunctionType(AccType(ArrayType(l_, dt3_)),
              FunctionType(ExpType(ArrayType(ln_, dt4_)), CommandType()))
                if l_ == l && dt3_ == dt && ln_ == l*n && dt4_ == dt =>
                CommandType()
              case ft => error(ft.toString, "FunctionType")
            }
          case _ => error(f.toString, "NatDependentLambdaPhrase")
        }

      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    IterateIExp(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl: Phrase[CommandType] = {
    import RewriteToImperative._

    val s = n.pow(k)*m
    val s_l = (l: ArithExpr) => n.pow(k-l)*m

    `new`( ArrayType(n.pow(k)*m, dt), GlobalMemory, buf1 => {
      `new`( ArrayType(n.pow(k)*m, dt), GlobalMemory, buf2 => {
        acc(in)(buf1.wr) `;`
        dblBufFor(s, dt, buf1, buf2, k,
          _Λ_(l =>
            λ(null.asInstanceOf[AccType]) { o =>
              λ(null.asInstanceOf[ExpType]) { x =>
                f (s_l(l)) (TruncAcc(s, s_l(l), dt, o)) (TruncExp(s, s_l(l), dt, x))
              }
            }
          ),
          out
        )
      } )
    } )
  }

  override def prettyPrint: String =
    s"(iterateIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}
