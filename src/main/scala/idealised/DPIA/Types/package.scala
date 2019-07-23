package idealised.DPIA

import idealised.DPIA.Types.TypeCheck._
import idealised.DPIA.Phrases.{Phrase, ToString}
import idealised.OpenCL.AddressSpace


package object Types {
  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]): Unit = p checkType pt
    def `:`[T <: PhraseType](p: Phrase[T]): Unit = p checkType pt
  }

  implicit class CheckHelper(p1: PhraseType) {
    def |(p2: PhraseType): PhraseType => Unit = (p: PhraseType) => {
      if (!(p == p1 || p == p2)) {
        error(ToString(p), expected = ToString(p1) + " or " + ToString(p2))
      }
    }
  }

  implicit final class ArrowType[T <: PhraseType](t: T) {
    @inline def ->: (dt: DataType): T = t
    @inline def ->: (n: Nat): T = t
    @inline def ->: (addressSpace: AddressSpace): T = t
    @inline def ->: (ntn: NatToNat): T = t
    @inline def ->: (ntd: NatToData): T = t
    @inline def ->: (unit: Unit): T = t
  }
}
