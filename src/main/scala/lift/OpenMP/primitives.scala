package lift.OpenMP

import lift.core.DSL._
import lift.core.types._
import lift.core.{primitives => core}

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  case object mapPar extends Primitive {
    override def t: Type = core.map.t
  }

  case object reducePar extends Primitive {
    override def t: Type = core.reduce.t
  }

  // TODO: should vectorisation be in the core, or shared with OpenCL?
  // TODO: ask for a scalar type parameter instead of casting

  case object asVector extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(a =>
      ArrayType(m * n, a) -> ArrayType(m, VectorType(n, a))
    )))
  }

  case object asScalar extends Primitive {
    override def t: Type = implN(n => implN(m => implT(a =>
      ArrayType(m, VectorType(n, a)) -> ArrayType(m * n, a)
    )))
  }

  case object vectorFromScalar extends Primitive {
    override def t: Type = implN(n => implT(a =>
      a -> VectorType(n, a)
    ))
  }
}