package lift.core

import lift.arithmetic.BigSum
import lift.core.DSL._
import lift.core.types._

object primitives {
  case object let extends Primitive {
    override def t: Type = implDT(s => implDT(t =>
      (s ->: t) ->: (s ->: t)
    ))
  }

  case object cast extends Primitive {
    override def t: Type = implBT(s => implBT(t => s ->: t))
  }

  case object depJoin extends Primitive {
    override def t: Type = implN(n => implN2N(lenF => implDT(dt => {
      DepArrayType(n, n2dtFun(n)(i => ArrayType(lenF(i), dt))) ->:
        ArrayType(BigSum(from = 0, upTo = n - 1, n => lenF(n)), dt)
    })))
  }

  case object depMapSeq extends Primitive {
    override def t: Type = implN(n => implN2DT(ft1 => implN2DT(ft2 =>
      nFunT(k => ft1(k) ->: ft2(k)) ->: DepArrayType(n, ft1) ->: DepArrayType(n, ft2)
    )))
  }

  case object depZip extends Primitive {
    override def t: Type = implN(n => implN2DT(ft1 => implN2DT(ft2 =>
      DepArrayType(n, ft1) ->: DepArrayType(n, ft2) ->: DepArrayType(n, n2dtFun(i => TupleType(ft1(i), ft2(i))))
    )))
  }

  case object drop extends Primitive {
    override def t: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(n + m, t) ->: ArrayType(m, t)
    )))
  }

  case object fst extends Primitive {
    override def t: Type = implDT(s => implDT(t => TupleType(s, t) ->: s))
  }

  case object generate extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      (IndexType(n) ->: t) ->: ArrayType(n, t)
    ))
  }

  case object idx extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      IndexType(n) ->: ArrayType(n, t) ->: t
    ))
  }

  case object id extends Primitive {
    override def t: Type = implDT(t => t ->: t)
  }

  case object indexAsNat extends Primitive {
    override def t: Type = implN(n =>
      IndexType(n) ->: NatType
    )
  }

  case object iterate extends Primitive {
    override def t: Type = implN(n => implN(m => nFunT(k => implDT(t =>
      nFunT(l => ArrayType(l * n, t) ->: ArrayType(l, t)) ->:
        ArrayType(m * n.pow(k), t)->: ArrayType(m, t)
    ))))
  }

  case object join extends Primitive {
    override def t: Type = implN(n => implN(m => implDT(t =>
      ArrayType(n, ArrayType(m, t)) ->: ArrayType(n * m, t)
    )))
  }

  case object map extends Primitive {
    override def t: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  case object mapSeq extends Primitive {
    override def t: Type = map.t
  }

  case object mapSeqUnroll extends Primitive {
    override def t: Type = map.t
  }

  case object natAsIndex extends Primitive {
    override def t: Type = nFunT(n => NatType ->: IndexType(n))
  }

  case object partition extends Primitive {
    override def t: Type = implN(n => implDT(dt =>
      nFunT(m => n2nFunT(lenF =>
        ArrayType(n, dt) ->: DepArrayType(m, n2dtFun(m)(i => ArrayType(lenF(i), dt)))
      ))))
  }

  // TODO? could be expressed in terms of a pad idx -> val
  case object padCst extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      t ->: ArrayType(n, t) ->: ArrayType(l + n + q, t)
    ))))
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  case object padClamp extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      ArrayType(n, t) ->: ArrayType(l + n + q, t)
    ))))
  }

  case object pair extends Primitive {
    override def t: Type = implDT(s => implDT(t =>
      s ->: t ->: TupleType(s, t)
    ))
  }

  case object reduce extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      (t ->: t ->: t) ->: t ->: ArrayType(n, t) ->: t
    ))
  }

  case object reduceSeq extends Primitive {
    override def t: Type = implN(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t
    )))
  }

  case object reduceSeqUnroll extends Primitive {
    override def t: Type = reduceSeq.t
  }

  case object reorder extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      (IndexType(n) ->: IndexType(n)) ->: // idxF
        (IndexType(n) ->: IndexType(n)) ->: // idxFinv
          ArrayType(n, t) ->: ArrayType(n, t)
    ))
  }

  case object gather extends Primitive {
    override def t: Type = implN(n => implN(m => implDT(t =>
      ArrayType(m, IndexType(n)) ->: ArrayType(n, t) ->: ArrayType(m, t)
    )))
  }

  case object scanSeq extends Primitive {
    override def t: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t ->: t) ->: t ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  case object slide extends Primitive {
    override def t: Type = implN(n => nFunT(sz => nFunT(sp => implDT(t => {
      ArrayType(sp * n + sz - sp, t) ->: ArrayType(n, ArrayType(sz, t))
    }))))
  }

  object slideSeq {
    trait Rotate {}
    case object Values extends Rotate {}
    case object Indices extends Rotate {}
  }

  case class slideSeq(rot: slideSeq.Rotate) extends Primitive {
    override def t: Type = implN(n => nFunT(sz => nFunT(sp => implDT(s => implDT(t => {
      (s ->: s) ->: (ArrayType(sz, s) ->: t) ->:
        ArrayType(sp * n + sz - sp, s) ->: ArrayType(n, t)
    })))))
  }

  case object snd extends Primitive {
    override def t: Type = implDT(s => implDT(t => TupleType(s, t) ->: t))
  }

  case object split extends Primitive {
    override def t: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(m * n, t) ->: ArrayType(m, ArrayType(n, t))
    )))
  }

  case object take extends Primitive {
    override def t: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(n + m, t) ->: ArrayType(n, t)
    )))
  }

  case object transpose extends Primitive {
    override def t: Type = implN(n => implN(m => implDT(dt =>
      ArrayType(n, ArrayType(m, dt)) ->: ArrayType(m, ArrayType(n, dt))
    )))
  }

  // if-then-else
  case object select extends Primitive {
    override def t: Type = implDT(t => bool ->: t ->: t ->: t)
  }

  case object neg extends Primitive {
    override def t: Type = implDT(t => t ->: t)
  }

  case object unzip extends Primitive {
    override def t: Type = implN(n => implDT(dt1 => implDT(dt2 =>
      ArrayType(n, TupleType(dt1, dt2)) ->: TupleType(ArrayType(n, dt1), ArrayType(n, dt2))
    )))
  }

  case object zip extends Primitive {
    override def t: Type = implN(n => implDT(a => implDT(b =>
      ArrayType(n, a) ->: ArrayType(n, b) ->: ArrayType(n, TupleType(a, b))
    )))
  }

  case object add extends Primitive {
    override def t: Type = implDT(t => t ->: t ->: t)
  }
  case object sub extends Primitive {
    override def t: Type = add.t
  }
  case object mul extends Primitive {
    override def t: Type = add.t
  }
  case object div extends Primitive {
    override def t: Type = add.t
  }
  case object mod extends Primitive {
    override def t: Type = add.t
  }

  case object gt extends Primitive {
    override def t: Type = implDT(t => t ->: t ->: bool)
  }
  case object lt extends Primitive {
    override def t: Type = gt.t
  }
  case object equal extends Primitive {
    override def t: Type = gt.t
  }

  // TODO: should vectorisation be in the core or not?

  // TODO: track alignment in type system?
  case object asVectorAligned extends Primitive {
    override def t: Type = nFunT(n => implN(m => implDT(a =>
      ArrayType(m * n, a) ->: ArrayType(m, VectorType(n, a))
    )))
  }

  case object asVector extends Primitive {
    override def t: Type = nFunT(n => implN(m => implST(t =>
      ArrayType(m * n, t) ->: ArrayType(m, VectorType(n, t))
    )))
  }

  case object asScalar extends Primitive {
    override def t: Type = implN(n => implN(m => implST(t =>
      ArrayType(m, VectorType(n, t)) ->: ArrayType(m * n, t)
    )))
  }

  case object vectorFromScalar extends Primitive {
    override def t: Type = implN(n => implST(t =>
      t ->: VectorType(n, t)
    ))
  }

  case class printType(msg: String = "") extends Primitive {
    override def t: Type = implT(t => t ->: t)
  }

  case class typeHole(msg: String = "") extends Primitive {
    override def t: Type = implT(t => t)
  }
}