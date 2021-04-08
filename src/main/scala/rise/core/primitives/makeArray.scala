package rise.core.primitives

import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._

final case class makeArray(n: Int) extends Builder {
  override def apply: ToBeTyped[Primitive] = ToBeTyped(makeArray.Primitive(n)())

  override def primitive: Primitive = makeArray.Primitive(n)()

  override def unapply(arg: Expr): Boolean = arg match {
    case _: makeArray.Primitive => true
    case _ => false
  }
}

object makeArray {
  private final case class Primitive(n: Int)
                                    (override val t: Type = TypePlaceholder)
    extends rise.core.Primitive
  {
    override def name: String = "makeArray"

    override def setType(t: Type): Primitive = Primitive(n)(t)

    override def typeScheme: Type =
      impl { t: DataType => {
        def tRec(m: Int, dt: DataType): Type =
          if (m <= 0) {
            ArrayType(n, dt)
          } else {
            dt ->: tRec(m - 1, dt)
          }
        tRec(n, t)
      }}
  }

  def unapply(arg: Expr): Option[Int] = arg match {
    case p: makeArray.Primitive => Some(p.n)
    case _ => None
  }
}
