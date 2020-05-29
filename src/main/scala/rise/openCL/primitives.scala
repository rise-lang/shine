package rise.openCL

import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.{primitives => core}
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  sealed trait Primitive extends rise.core.Primitive

  // TODO? depMapGlobal, depMapLocal, depMapWorkGroup

  @primitive case class MapGlobal(dim: Int)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class MapLocal(dim: Int)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class MapWorkGroup(dim: Int)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class OclToMem()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implDT(t => aFunT(_ => t ->: t))
  }

  @primitive case class OclReduceSeq()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      aFunT(_ =>
        implN(n =>
          implDT(s =>
            implDT(t => (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t)
          )
        )
      )
  }

  @primitive case class OclReduceSeqUnroll()(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      aFunT(_ =>
        implN(n =>
          implDT(s =>
            implDT(t => (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t)
          )
        )
      )
  }

  @primitive case class OclIterate()(override val t: Type = TypePlaceholder)
      extends Primitive {
    // format: off
    override def typeScheme: Type =
      aFunT(_ =>
        implN(n =>
          implN(m =>
            nFunT(k =>
              implDT(t =>
                nFunT(l =>
                  ArrayType(l * n, t) ->: ArrayType(l, t)) ->:
                    ArrayType(m * n.pow(k), t) ->: ArrayType(m, t)
              )
            )
          )
        )
      )
    // format: on
  }

  @primitive case class OclCircularBuffer()(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      // TODO: should return a stream / sequential array, not an array
      aFunT(_ => implN(n => nFunT(alloc => nFunT(sz => implDT(s => implDT(t =>
        (s ->: t) ->: // function to load an input
          ArrayType(n + sz, s) ->: ArrayType(1 + n, ArrayType(sz, t))
      ))))))
  }

  @primitive case class OclRotateValues()(
    override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      // TODO: should return a stream / sequential array, not an array
      aFunT(_ => implN(n => nFunT(sz => implDT(s =>
        (s ->: s) ->: // function to write a value
          ArrayType(n + sz, s) ->: ArrayType(1 + n, ArrayType(sz, s))
      ))))
  }
}
