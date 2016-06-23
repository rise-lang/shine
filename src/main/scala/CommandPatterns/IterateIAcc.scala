package CommandPatterns

import AccPatterns.TruncAcc
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import apart.arithmetic._
import Compiling.{RewriteToImperative, SubstituteImplementations}
import DSL._
import ExpPatterns.TruncExp

case class IterateIAcc(n: ArithExpr,
                       m: ArithExpr,
                       k: ArithExpr,
                       dt: DataType,
                       out: Phrase[AccType],
                       f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                       in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(in)) match {
      case (AccType(ArrayType(m_, dt1_)), ExpType(ArrayType(nkm_, dt2_)))
        if m_ == m && nkm_ == (n.pow(k) * m) && dt1_ == dt && dt2_ == dt =>

        f match {
          case NatDependentLambdaPhrase(l, body) =>
            setParamType(body, AccType(ArrayType(l /^ n, dt)))
            setSecondParamType(body, ExpType(ArrayType(l, dt)))
            TypeChecker(body) match {
              case FunctionType(AccType(ArrayType(l_n, dt3_)),
              FunctionType(ExpType(ArrayType(l_, dt4_)), CommandType())) =>
                if (l_n == l /^ n && dt3_ == dt && l_ == l && dt4_ == dt) {
                  CommandType()
                } else {
                  error(s"[$dt3_]_$l_n -> [$dt4_]_$l_ -> CommandType",
                        s"[$dt]_${l /^ n} -> [$dt]_$l -> CommandType")
                }
              case ft => error(ft.toString, "FunctionType")
            }
          case _ => error(f.toString, "NatDependentLambdaPhrase")
        }

      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    IterateIAcc(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl: Phrase[CommandType] = {
    val sEnd = n.pow(k)*m
    val s = (l: ArithExpr) => n.pow(k-l)*m

    `new`( ArrayType(sEnd, dt), GlobalMemory, buf1 => {
      `new`( ArrayType(sEnd, dt), GlobalMemory, buf2 => {
        SubstituteImplementations(MapI(sEnd, dt, dt, buf1.wr,
          λ( AccType(dt) ) { o => λ( ExpType(dt) ) { x => o `:=` x } }, in)) `;`
        dblBufFor(sEnd, dt, buf1, buf2, k,
          _Λ_(l => {
            val s_l = s(l)
            val s_l1 = s(l + 1)
            λ(AccType(ArrayType(sEnd, dt))) { o =>
              λ(ExpType(ArrayType(sEnd, dt))) { x =>
                SubstituteImplementations(f(s_l)(TruncAcc(sEnd, s_l1, dt, o))(TruncExp(sEnd, s_l, dt, x)))
              }
            }
          }),
          λ(ExpType(ArrayType(sEnd, dt)))( x =>
            SubstituteImplementations(MapI(m, dt, dt, out,
              λ( AccType(dt) ) { o => λ( ExpType(dt) ) { x => o `:=` x } }, x)))
        )
      } )
    } )
  }

  override def prettyPrint: String =
    s"(iterateIAcc ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}
