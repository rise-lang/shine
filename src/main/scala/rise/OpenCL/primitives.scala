package rise.OpenCL

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.{Primitive, primitives => core}
import primitiveMacro.Primitive.primitive

object primitives {
  sealed trait Primitive extends rise.core.Primitive

  // TODO? depMapGlobal, depMapLocal, depMapWorkGroup

  @primitive case class MapGlobal(dim: Int)(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class MapLocal(dim: Int)(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class MapWorkGroup(dim: Int)(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class ToMem()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = implDT(t => aFunT(a => t ->: t))
  }

  @primitive case class OclReduceSeq()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = aFunT(a => implN(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t
    ))))
  }

  @primitive case class OclReduceSeqUnroll()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = aFunT(a => implN(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t
    ))))
  }

  @primitive case class OclIterate()(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = aFunT(a => implN(n => implN(m => nFunT(k => implDT(t =>
      nFunT(l => ArrayType(l * n, t) ->: ArrayType(l, t)) ->:
        ArrayType(m * n.pow(k), t) ->: ArrayType(m, t)
    )))))
  }

  @primitive case class OclSlideSeq(rot: core.SlideSeq.Rotate)(override val t: Type = TypePlaceholder) extends Primitive {
    override def typeScheme: Type = aFunT(a => implN(n => nFunT(sz => nFunT(sp => implDT(s => implDT(t => {
      (s ->: s) ->: (ArrayType(sz, s) ->: t) ->:
        ArrayType(sp * n + sz - sp, s) ->: ArrayType(n, t)
    }))))))
  }
}
