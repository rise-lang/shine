package lift.OpenMP

import lift.core.DSL._
import lift.core.types._
import lift.core.{Primitive, primitives => core}
import primitiveMacro.Primitive.primitive

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  // TODO? depMapPar

  @primitive case class MapPar()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class ReducePar()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implN(n => implDT(t =>
      (t ->: t ->: t) ->: t ->: ArrayType(n, t) ->: t
    ))
  }
}