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

  @primitive case class OclSlideSeq(rot: core.SlideSeq.Rotate)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      aFunT(_ => implN(n => nFunT(sz => nFunT(sp => implDT(t =>
        (t ->: t) ->:
          ArrayType(sp * n + sz - sp, t) ->: ArrayType(n, ArrayType(sz, t))
      )))))
  }
}
