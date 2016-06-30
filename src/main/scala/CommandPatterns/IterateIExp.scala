package CommandPatterns

import AccPatterns.TruncAcc
import Compiling.SubstituteImplementations
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import ExpPatterns.TruncExp
import apart.arithmetic._

import scala.xml.Elem

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
    out.t =?= t"(exp[$m.$dt] -> comm)"
    f match {
      case NatDependentLambdaPhrase(l, _) =>
        f.t =?= t"($l : nat) -> acc[${l/^n}.$dt] -> exp[$l.$dt] -> comm"
      case _ => throw new Exception("This should not happen")
    }
    in.t =?= exp"[${n.pow(k)*m}.$dt]"
    comm

//    TypeChecker(in) match {
//      case ExpType(ArrayType(nkm_, dt1_))
//        if nkm_ == (n.pow(k) * m) && dt1_ == dt =>
//
//        setParamType(out, ExpType(ArrayType(m, dt)))
//        TypeChecker(out) match {
//          case FunctionType(ExpType(ArrayType(m_, dt2_)), CommandType()) =>
//            if (m_ != m || dt2_ != dt)
//              error(s"[$m_.$dt2_]", s"[$m.$dt]")
//          case x => error(x.toString, "FunctionType")
//        }
//
//        f match {
//          case NatDependentLambdaPhrase(l, body) =>
//            setParamType(body, AccType(ArrayType(l /^ n, dt)))
//            setSecondParamType(body, ExpType(ArrayType(l, dt)))
//            TypeChecker(f) match {
//              case NatDependentFunctionType(_,
//                FunctionType(AccType(ArrayType(l_, dt3_)),
//                  FunctionType(ExpType(ArrayType(ln_, dt4_)), CommandType())))
//                if l_ == l && dt3_ == dt && ln_ == l*n && dt4_ == dt =>
//                CommandType()
//              case ft => error(ft.toString, "FunctionType")
//            }
//          case _ => error(f.toString, "NatDependentLambdaPhrase")
//        }
//
//      case t_ => error(t_.toString, "ArrayType")
//    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    IterateIExp(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    val sEnd = n.pow(k)*m
    val s = (l: ArithExpr) => n.pow(k-l)*m

    val addressSpace = GlobalMemory

    `new`( ArrayType(sEnd, dt), addressSpace, buf1 => {
      `new`( ArrayType(sEnd, dt), addressSpace, buf2 => {
        SubstituteImplementations(MapI(sEnd, dt, dt, buf1.wr,
          λ( AccType(dt) ) { o => λ( ExpType(dt) ) { x => o `:=` x } }, in), env) `;`
          dblBufFor(sEnd, dt, addressSpace, buf1, buf2, k,
            _Λ_(l => {
              val s_l = s(l)
              val s_l1 = s(l + 1)
              λ(AccType(ArrayType(sEnd, dt))) { o =>
                λ(ExpType(ArrayType(sEnd, dt))) { x =>
                  SubstituteImplementations(f(s_l)(TruncAcc(sEnd, s_l1, dt, o))(TruncExp(sEnd, s_l, dt, x)), env)
                }
              }
            }),
            out
          )
      } )
    } )
  }

  override def prettyPrint: String = s"(iterateIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambdaPhrase(l, _) => l
      case _ => throw new Exception("This should not happen")
    }
    <iterateIExp n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <output type={ToString(ExpType(ArrayType(m, dt)) -> CommandType())}>
        {Core.xmlPrinter(out)}
      </output>
      <f type={ToString(l -> (AccType(ArrayType(l /^ n, dt)) -> (ExpType(ArrayType(l, dt)) -> CommandType())))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n.pow(k) * m, dt)))}>
        {Core.xmlPrinter(in)}
      </input>
    </iterateIExp>
  }
}
