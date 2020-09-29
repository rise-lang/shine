package rise.openMP

import rise.core.TypeLevelDSL._
import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  // TODO? depMapPar

  @primitive object mapPar extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive object reducePar extends Primitive with Builder {
    implNat(n => implDT(t =>
      (t ->: t ->: t) ->: t ->: (n`.`t) ->: t))
  }
}
