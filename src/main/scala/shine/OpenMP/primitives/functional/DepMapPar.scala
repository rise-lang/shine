package shine.OpenMP.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepMapPar(n: Nat,
                           ft1: NatToData,
                           ft2: NatToData,
                           f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: f.t.x ->: expT(ft1(f.t.x), read) ->: expT(ft2(f.t.x), write)
  array :: expT(n `.d` ft1, read)
  override val t: ExpType = expT(n`.d`ft2, write)
}
