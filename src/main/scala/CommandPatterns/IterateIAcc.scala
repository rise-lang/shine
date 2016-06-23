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
    val s = n.pow(k)*m
    val s_l = (l: ArithExpr) => n.pow(k-l)*m

    `new`( ArrayType(s, dt), GlobalMemory, buf1 => {
      `new`( ArrayType(s, dt), GlobalMemory, buf2 => {
        SubstituteImplementations(MapI(s, dt, dt, buf1.wr,
          λ( AccType(dt) ) { o => λ( ExpType(dt) ) { x => o `:=` x } }, in)) `;`
        dblBufFor(s, dt, buf1, buf2, k,
          _Λ_(l => {
            val s_l_l = s_l(l)
            λ(null.asInstanceOf[AccType]) { o =>
              λ(null.asInstanceOf[ExpType]) { x =>

                println(f)
                println(TypeChecker(f))

                val fl = f(s_l(l))

                println(fl)
                println(fl.t)

                fl(TruncAcc(s, s_l_l, dt, o))(TruncExp(s, s_l_l, dt, x))
              }
            }
          }
          ),
          λ(ExpType(ArrayType(s, dt)))( x =>
            SubstituteImplementations(MapI(m, dt, dt, out,
              λ( AccType(dt) ) { o => λ( ExpType(dt) ) { x => o `:=` x } }, x))
          )
        )
      } )
    } )
  }

  override def prettyPrint: String =
    s"(iterateIAcc ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}
