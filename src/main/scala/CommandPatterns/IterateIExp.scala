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

  override def typeCheck: Unit = {
    import TypeChecker._
    out checkType t"(exp[$m.$dt] -> comm)"
    f match {
      case NatDependentLambdaPhrase(l, _) =>
        f checkType t"($l : nat) -> acc[${l/^n}.$dt] -> exp[$l.$dt] -> comm"
      case _ => throw new Exception("This should not happen")
    }
    in checkType exp"[${n.pow(k)*m}.$dt]"
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
