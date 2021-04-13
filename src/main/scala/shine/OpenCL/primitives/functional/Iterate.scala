package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Iterate(a: AddressSpace,
                         n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive {
  {
    val l = f.t.x
    f :: l ->: expT({l * n}`.`dt, read) ->: expT(l`.`dt, write)
    array :: expT({m * n.pow(k)}`.`dt, read)
  }
  override val t: ExpType = expT(m`.`dt, write)
}
