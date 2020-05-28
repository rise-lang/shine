package rise.openMP

import rise.core.TypeLevelDSL._
import rise.core.types._
import primitiveMacro.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  sealed trait Primitive extends rise.core.Primitive

  // TODO? depMapPar

  @primitive case class MapPar()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class ReducePar()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n => implDT(t => (t ->: t ->: t) ->: t ->: ArrayType(n, t) ->: t))
  }
}
