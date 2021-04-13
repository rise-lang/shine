package shine.DPIA.primitives.functional

import arithexpr.arithmetic.SimplifiedExpr
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class Slide(n: Nat,
                       sz: Nat,
                       sp: Nat,
                       dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive {
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  input :: expT(inputSize`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)
}
