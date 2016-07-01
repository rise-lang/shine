package CommandPatterns

import AccPatterns.TruncAcc
import Core._
import Core.OperationalSemantics._
import apart.arithmetic._
import Compiling.SubstituteImplementations
import DSL._
import ExpPatterns.TruncExp

import scala.xml.Elem

case class IterateIAcc(n: ArithExpr,
                       m: ArithExpr,
                       k: ArithExpr,
                       dt: DataType,
                       out: Phrase[AccType],
                       f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                       in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): Unit = {
    import TypeChecker._
    out checkType acc"[$m.$dt]"
    f match {
      case NatDependentLambdaPhrase(l, _) =>
        f checkType t"($l : nat) -> acc[${l/^n}.$dt] -> exp[$l.$dt] -> comm"
      case _ => throw new Exception("This should not happen")
    }
    in checkType exp"[${n.pow(k)*m}.$dt]"
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    IterateIAcc(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    // infer the address space from the output
    val identifier = ToOpenCL.acc(out, ToOpenCL.Environment(?, ?))
    val addressSpace = env.addressspace(identifier.name)

    val sEnd = n.pow(k)*m

    val iterateLoop = (start: ArithExpr,
                       end: ArithExpr,
                       buf1: Phrase[VarType],
                       buf2: Phrase[VarType]) => {
      val s = (l: ArithExpr) => n.pow(end - l - start) * m

      end - start match {
        case Cst(x) if x > 2 =>
          // unrolling the last iteration
          dblBufFor(sEnd, dt, addressSpace, buf1, buf2, end - start - 1,
            _Λ_(l => {
              val s_l = s(l)
              val s_l1 = s(l + 1)
              λ(AccType(ArrayType(sEnd, dt))) { o =>
                λ(ExpType(ArrayType(sEnd, dt))) { x =>
                  SubstituteImplementations(
                    f(s_l)(TruncAcc(sEnd, s_l1, dt, o))(TruncExp(sEnd, s_l, dt, x)),
                    env)
                }
              }
            }),
            λ(ExpType(ArrayType(sEnd, dt)))(x =>
              SubstituteImplementations(
                f(s(end - start - 1))(TruncAcc(m, s(end - start), dt, out))(TruncExp(sEnd, s(end - start - 1), dt, x))
                , env))
          )

        case _ =>
          // extra copy to output
          dblBufFor(sEnd, dt, addressSpace, buf1, buf2, end - start,
            _Λ_(l => {
              val s_l = s(l)
              val s_l1 = s(l + 1)
              λ(AccType(ArrayType(sEnd, dt))) { o =>
                λ(ExpType(ArrayType(sEnd, dt))) { x =>
                  SubstituteImplementations(
                    f(s_l)(TruncAcc(sEnd, s_l1, dt, o))(TruncExp(sEnd, s_l, dt, x)),
                    env)
                }
              }
            }),
            λ(ExpType(ArrayType(sEnd, dt)))(x =>
              SubstituteImplementations(MapI(m, dt, dt, out,
                λ(AccType(dt)) { o => λ(ExpType(dt)) { x => o `:=` x } }, x), env))
          )
      }
    }

    val s = (l: ArithExpr) => n.pow(k-l)*m

    k match {
      case Cst(x) if x > 2 =>
        `new`(ArrayType(sEnd, dt), addressSpace, buf1 => {
          `new`(ArrayType(sEnd, dt), addressSpace, buf2 => {
            SubstituteImplementations(
              f(s(0))(TruncAcc(sEnd, s(1), dt, buf1.wr))(TruncExp(sEnd, s(0), dt, in))
              , env) `;`
              iterateLoop(1, k, buf1, buf2)
          })
        })

      case _ =>
        `new`(ArrayType(sEnd, dt), addressSpace, buf1 => {
          `new`(ArrayType(sEnd, dt), addressSpace, buf2 => {
            SubstituteImplementations(MapI(sEnd, dt, dt, buf1.wr,
              λ(AccType(dt)) { o => λ(ExpType(dt)) { x => o `:=` x } }, in), env) `;`
              iterateLoop(0, k, buf1, buf2)
          })
        })
    }
  }

  override def prettyPrint: String = s"(iterateIAcc ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambdaPhrase(l, _) => l
      case _ => throw new Exception("This should not happen")
    }
    <iterateIAcc n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(m, dt)))}>
        {Core.xmlPrinter(out)}
      </output>
      <f type={ToString(l -> (AccType(ArrayType(l /^ n, dt)) -> (ExpType(ArrayType(l, dt)) -> CommandType())))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n.pow(k) * m, dt)))}>
        {Core.xmlPrinter(in)}
      </input>
    </iterateIAcc>
  }
}
