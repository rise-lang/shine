package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{CodeGenerator, SubstituteImplementations}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.{TruncAcc, TruncExp}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import lift.arithmetic.NamedVar

import scala.xml.Elem

final case class IterateIAcc(n: Nat,
                             m: Nat,
                             k: Nat,
                             dt: DataType,
                             out: Phrase[AccType],
                             f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                             in: Phrase[ExpType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override val `type`: CommandType =
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (out :: acc"[$m.$dt]") ->
            (f :: t"($l : nat) -> acc[${l /^ n}.$dt] -> exp[$l.$dt] -> comm") ->
              (in :: exp"[${n.pow(k) * m}.$dt]") ->
                comm

      case _ => throw new Exception("This should not happen")
    }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    IterateIAcc(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {

    newDoubleBufferImpl(env)

//    // infer the address space from the output
//    val addressSpace = SubstituteImplementations.getAddressSpace(out, env) match {
//      case Some(a) => a
//      case None => idealised.OpenCL.GlobalMemory
//    }
//
//    k match {
////      case Cst(x) if x > 2 => unrollFirstAndLastIteration(env, addressSpace)
////      case Cst(x) if x > 1 => unrollFirstIteration(env, addressSpace)
//      case _ => noUnrolling(env, addressSpace)
//    }
  }

  private def newDoubleBufferImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    val `n^k*m` = n.pow(k) * m

    newDoubleBuffer(dt"[${`n^k*m`}.$dt]", dt"[$m.$dt]", ArrayType(`n^k*m`, dt), in, out,
      (v: Phrase[VarType],
       swap: Phrase[CommandType],
       done: Phrase[CommandType]) => {
        `for`(k, lp => {
          val l = NamedVar(lp.name)

          SubstituteImplementations(
            f.apply(n.pow(k - l) * m)
             .apply(TruncAcc(`n^k*m`, n.pow(k - l - 1) * m, dt, v.wr))
             .apply(TruncExp(`n^k*m`, n.pow(k - l) * m, dt, v.rd)), env) `;`
          IfThenElse(lp < Literal(IndexData(k - 1, IndexType(k))), swap, done)
        })
      })
  }

  private def noUnrolling(env: SubstituteImplementations.Environment,
                           addressSpace: AddressSpace): Phrase[CommandType] = {




    val `n^k*m` = n.pow(k) * m

    `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf1 =>
      `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf2 =>
        SubstituteImplementations(MapI(`n^k*m`, dt, dt,
          λ(exp"[$dt]")(e => λ(acc"[$dt]")(a => a := e)), in, buf1.wr), env) `;`
          dblBufFor(`n^k*m`, m, k, dt, addressSpace, buf1, buf2,
            _Λ_(l => λ(acc"[${`n^k*m`}.$dt]")(a => λ(exp"[${`n^k*m`}.$dt]")(e =>
              SubstituteImplementations(
                f (n.pow(k - l) * m)
                  (TruncAcc(`n^k*m`, n.pow(k - l - 1) * m, dt, a))
                  (TruncExp(`n^k*m`, n.pow(k - l    ) * m, dt, e)), env)))),
            λ(exp"[$m.$dt]")(x =>
              SubstituteImplementations(MapI(m, dt, dt,
                λ(exp"[$dt]")(e => λ(acc"[$dt]")(a => a := e)), x, out), env)))))
  }

  private def unrollFirstIteration(env: SubstituteImplementations.Environment,
                                    addressSpace: AddressSpace): Phrase[CommandType] = {
    val `n^(k-1)*m` = n.pow(k - 1) * m

    `new`(dt"[${`n^(k-1)*m`}.$dt]", addressSpace, buf1 =>
      `new`(dt"[${`n^(k-1)*m`}.$dt]", addressSpace, buf2 =>
        SubstituteImplementations(
          f (n.pow(k) * m) (buf1.wr) (in), env)`;`
        dblBufFor(`n^(k-1)*m`, m, k-1, dt, addressSpace, buf1, buf2,
          _Λ_(l => λ(acc"[${`n^(k-1)*m`}.$dt]")(a => λ(exp"[${`n^(k-1)*m`}.$dt]")(e =>
            SubstituteImplementations(
              f (n.pow(k - (l + 1)) * m)
                (TruncAcc(`n^(k-1)*m`, n.pow(k - (l + 1) - 1) * m, dt, a))
                (TruncExp(`n^(k-1)*m`, n.pow(k - (l + 1)    ) * m, dt, e)), env)))),
          λ(exp"[$m.$dt]")(x =>
            SubstituteImplementations(MapI(m, dt, dt,
              λ(exp"[$dt]")(e => λ(acc"[$dt]")(a => a := e)), x, out), env)))))
  }

  private def unrollFirstAndLastIteration(env: SubstituteImplementations.Environment,
                                          addressSpace: AddressSpace): Phrase[CommandType] = {
    val `n^(k-1)*m` = n.pow(k - 1) * m

    `new`(dt"[${`n^(k-1)*m`}.$dt]", addressSpace, buf1 =>
      `new`(dt"[${`n^(k-1)*m`}.$dt]", addressSpace, buf2 =>
        SubstituteImplementations(
          f (n.pow(k) * m) (buf1.wr) (in), env)`;`
          dblBufFor(`n^(k-1)*m`, n * m, k-2, dt, addressSpace, buf1, buf2,
            _Λ_(l => λ(acc"[${`n^(k-1)*m`}.$dt]")(a => λ(exp"[${`n^(k-1)*m`}.$dt]")(e =>
              SubstituteImplementations(
                f (n.pow(k - (l + 1)) * m)
                  (TruncAcc(`n^(k-1)*m`, n.pow(k - (l + 1) - 1) * m, dt, a))
                  (TruncExp(`n^(k-1)*m`, n.pow(k - (l + 1)    ) * m, dt, e)), env)))),
            λ(exp"[${n * m}.$dt]")(x =>
              SubstituteImplementations(f (n * m) (out) (x), env)))))
  }

  override def prettyPrint: String = s"(iterateIAcc ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterateIAcc n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(m, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(l -> (AccType(ArrayType(l /^ n, dt)) -> (ExpType(ArrayType(l, dt)) -> CommandType())))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n.pow(k) * m, dt)))}>
        {Phrases.xmlPrinter(in)}
      </input>
    </iterateIAcc>
  }
}
