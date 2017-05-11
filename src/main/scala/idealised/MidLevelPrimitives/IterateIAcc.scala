package idealised.MidLevelPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.SubstituteImplementations
import idealised.DSL.typed._
import idealised.LowLevelPrimitives.{TruncAcc, TruncExp}

import scala.xml.Elem

final case class IterateIAcc(n: Nat,
                             m: Nat,
                             k: Nat,
                             dt: DataType,
                             out: Phrase[AccType],
                             f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                             in: Phrase[ExpType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override def typeCheck(): Unit = {
    import TypeChecker._
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (out :: acc"[$m.$dt]") ->
          (f :: t"($l : nat) -> acc[${l /^ n}.$dt] -> exp[$l.$dt] -> comm") ->
          (in :: exp"[${n.pow(k) * m}.$dt]") ->
          comm

      case _ => throw new Exception("This should not happen")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    IterateIAcc(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    // infer the address space from the output
    val addressSpace = SubstituteImplementations.getAddressSpace(out, env) match {
      case Some(a) => a
      case None => idealised.OpenCL.GlobalMemory
    }

    k match {
//      case Cst(x) if x > 2 => unrollFirstAndLastIteration(env, addressSpace)
//      case Cst(x) if x > 1 => unrollFirstIteration(env, addressSpace)
      case _ => noUnrolling(env, addressSpace)
    }
  }

  private def noUnrolling(env: SubstituteImplementations.Environment,
                           addressSpace: AddressSpace): Phrase[CommandType] = {
    val `n^k*m` = n.pow(k) * m

    `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf1 =>
      `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf2 =>
        SubstituteImplementations(MapI(`n^k*m`, dt, dt, buf1.wr,
          λ(acc"[$dt]")(a => λ(exp"[$dt]")(e => a `:=` e)), in), env) `;`
          dblBufFor(`n^k*m`, m, k, dt, addressSpace, buf1, buf2,
            _Λ_(l => λ(acc"[${`n^k*m`}.$dt]")(a => λ(exp"[${`n^k*m`}.$dt]")(e =>
              SubstituteImplementations(
                f (n.pow(k - l) * m)
                  (TruncAcc(`n^k*m`, n.pow(k - l - 1) * m, dt, a))
                  (TruncExp(`n^k*m`, n.pow(k - l    ) * m, dt, e)), env)))),
            λ(exp"[$m.$dt]")(x =>
              SubstituteImplementations(MapI(m, dt, dt, out,
                λ(acc"[$dt]")(a => λ(exp"[$dt]")(e => a `:=` e)), x), env)))))
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
            SubstituteImplementations(MapI(m, dt, dt, out,
              λ(acc"[$dt]")(a => λ(exp"[$dt]")(e => a `:=` e)), x), env)))))
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
