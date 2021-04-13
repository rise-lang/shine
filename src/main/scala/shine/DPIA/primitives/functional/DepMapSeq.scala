package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

//noinspection TypeAnnotation
@expPrimitive
final case class DepMapSeq(unroll: Boolean)
                          (val n: Nat,
                           val ft1: NatToData,
                           val ft2: NatToData,
                           val f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           val array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: f.t.x ->: expT(ft1(f.t.x), read) ->: expT(ft2(f.t.x), write)
  array :: expT(n `.d` ft1, read)
  override val t: ExpType = expT(n`.d`ft2, write)

  def unwrap: (Nat, NatToData, NatToData, Phrase[`(nat)->:`[ExpType ->: ExpType]], Phrase[ExpType]) =
    (n, ft1, ft2, f, array)
}
