package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.OpenCL.IntermediatePrimitives.OpenCLIterateIAcc

import scala.xml.Elem

final case class OpenCLIterate(a: AddressSpace,
                               n: Nat,
                               m: Nat,
                               k: Nat,
                               dt: DataType,
                               f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                               array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType = {
    val l = f.t.x
    (a: AddressSpace) ->: (n: Nat) ->: (m: Nat) ->: (k: Nat) ->: (dt: DataType) ->:
      (f :: t"($l : nat) -> exp[${l * n}.$dt, $read] -> exp[$l.$dt, $read]") ->:
        (array :: exp"[${m * n.pow(k)}.$dt, $read]") ->:
          exp"[$m.$dt, $read]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLIterate(fun.addressSpace(a), fun.nat(n), fun.nat(m), fun.nat(k), fun.data(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    ???
  }

  override def xmlPrinter: Elem = {
    val l = f.t.x
    <oclIterate a={ToString(a)} n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l ->: ExpType(ArrayType(l, dt), read) ->: ExpType(ArrayType(l /^ n, dt), read))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </oclIterate>
  }

  override def prettyPrint: String =
    s"(iterate $a $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._

    con(array)(λ(exp"[${m * n.pow(k)}.$dt, $read]")(x =>
      OpenCLIterateIAcc(a, n, m, k, dt, A,
        _Λ_[NatKind](l => λ(acc"[$l.$dt]")(o => λ(exp"[${l * n}.$dt, $read]")(x => acc(f(l)(x))(o)))),
        x) ))
  }

  // TODO
  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {

    ???
  }
}
