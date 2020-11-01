package rise.openmp

import rise.core.dsl.Builder
import rise.core.dsl.Type._
import rise.core.exprs.Primitive
import rise.core.types.{DataType, Nat}
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  // TODO? depMapPar

  @primitive object mapPar extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive object reducePar extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      (t ->: t ->: t) ->: t ->: (n`.`t) ->: t }}
  }
}
