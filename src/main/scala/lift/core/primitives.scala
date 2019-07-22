package lift.core

import lift.arithmetic.{BigSum, InclusiveIndexVar}
import lift.core.types._
import lift.core.DSL._

object primitives {
  case object asIndex extends Primitive {
    override def t: Type = nFunT(n => NatType._R ->: IndexType(n)._R)
  }

  // TODO: ask for basic type parameters
  case object cast extends Primitive {
    override def t: Type = implDT(s => implDT(t => s._R ->: s._R))
  }

  case object depJoin extends Primitive {
    override def t: Type = implN(n => implNNF(lenF => implDT(dt => {
      DepArrayType(n, NatDataTypeLambda(n, (i: NatIdentifier) => ArrayType(lenF(i), dt)))._R ->:
        ArrayType(BigSum(from = 0, upTo = n - 1, (n: InclusiveIndexVar) => lenF(n)), dt)._R
    })))
  }

  case object depMapSeq extends Primitive {
    override def t: Type = implN(n => implNDF(ft1 => implNDF(ft2 =>
      nFunT(k => ft1(k)._R ->: ft2(k)._R) ->: DepArrayType(n, ft1)._R ->: DepArrayType(n, ft2)._R
    )))
  }

  case object drop extends Primitive {
    override def t: Type = nFunT(n => implN(m => implW(w => implDT(t =>
      ArrayType(n + m, t).__(w) ->: ArrayType(m, t).__(w)
    ))))
  }

  case class ForeignFunction(decl: ForeignFunction.Decl, override val t: Type) extends Primitive {
    override def toString: String = decl.name
  }

  object ForeignFunction {
    case class Decl(name: String, definition: Option[Def])
    case class Def(params: Seq[String], body: String)
  }

  case object fst extends Primitive {
    override def t: Type = implDT(s => implDT(t => TupleType(s, t)._R ->: s._R))
  }

  case object generate extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      IndexType(n)._R ->: t._R ->: ArrayType(n, t)._R
    ))
  }

  case object idx extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      IndexType(n)._R ->: ArrayType(n, t)._R ->: t._R
    ))
  }

  case object indexAsNat extends Primitive {
    override def t: Type = implN(n =>
      IndexType(n)._R ->: NatType._R
    )
  }

  case object iterate extends Primitive {
    override def t: Type = implN(n => implN(m => nFunT(k => implDT(t =>
      nFunT(l => ArrayType(l * n, t)._R ->: ArrayType(l, t)._R) ->:
        ArrayType(m * n.pow(k), t)._R ->: ArrayType(m, t)._R
    ))))
  }

  case object join extends Primitive {
    override def t: Type = implN(n => implN(m => implW(w => implDT(t =>
      ArrayType(n, ArrayType(m, t)).__(w) ->: ArrayType(n * m, t).__(w)
    ))))
  }

  case object map extends Primitive {
    override def t: Type = implN(n => implDT(s => implDT(t =>
      (s._R ->: t._W) ->: ArrayType(n, s)._R ->: ArrayType(n, t)._W
    )))
  }

  case object mapSeq extends Primitive {
    override def t: Type = map.t
  }

  // TODO? could be expressed in terms of a pad idx -> val
  case object padCst extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      t._R ->: ArrayType(n, t)._R ->: ArrayType(l + n + q, t)._R
    ))))
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  case object padClamp extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      ArrayType(n, t)._R ->: ArrayType(l + n + q, t)._R
    ))))
  }

  case object pair extends Primitive {
    override def t: Type = implDT(s => implDT(t =>
      s._R ->: t._R ->: TupleType(s, t)._R
    ))
  }

  case object reduce extends Primitive {
    override def t: Type = implN(n => implA(w => implDT(s => implDT(t =>
      (s._R ->: t._R ->: t._W) ->: t._W ->: ArrayType(n, s)._R ->: t._R
    ))))
  }

  case object reduceSeq extends Primitive {
    override def t: Type = reduce.t
  }

  case object reduceSeqUnroll extends Primitive {
    override def t: Type = reduce.t
  }

  case object reorder extends Primitive {
    override def t: Type = implN(n => implDT(a =>
      (IndexType(n)._R ->: IndexType(n)._R) ->: // idxF
        (IndexType(n)._R ->: IndexType(n)._R) ->: // idxFinv
          ArrayType(n, a)._R ->: ArrayType(n, a)._R
    ))
  }

  case object scan extends Primitive {
    override def t: Type = implN(n => implDT(s => implDT(t =>
      (s._R ->: t._R ->: t._W) ->: t._W ->: ArrayType(n, s)._R ->: ArrayType(n, t)._R
    )))
  }

  case object scanSeq extends Primitive {
    override def t: Type = scan.t
  }

  case object slide extends Primitive {
    override def t: Type = implN(n => nFunT(sz => nFunT(sp => implDT(t => {
      ArrayType(sp * n + sz - sp, t)._R ->: ArrayType(n, ArrayType(sz, t))._R
    }))))
  }

  object slideSeq {
    trait Rotate {}
    case object Values extends Rotate {}
    case object Indices extends Rotate {}
  }

  case class slideSeq(roprimT: slideSeq.Rotate) extends Primitive {
    override def t: Type = slide.t
  }

  case object slideSeqBuffer extends Primitive {
    override def t: Type = slide.t
  }

  case object snd extends Primitive {
    override def t: Type = implDT(s => implDT(t => TupleType(s, t)._R ->: t._R))
  }

  case object split extends Primitive {
    override def t: Type = nFunT(n => implN(m => implW(w => implDT(t =>
      ArrayType(m * n, t).__(w) ->: ArrayType(m, ArrayType(n, t)).__(w)
    ))))
  }

  case object take extends Primitive {
    override def t: Type = nFunT(n => implN(m => implW(w => implDT(t =>
      ArrayType(n + m, t).__(w) ->: ArrayType(n, t).__(w)
    ))))
  }

  case object transpose extends Primitive {
    override def t: Type = implN(n => implN(m => implDT(dt =>
      ArrayType(n, ArrayType(m, dt))._R ->: ArrayType(m, ArrayType(n, dt))._R
    )))
  }

  // if-then-else
  case object select extends Primitive {
    override def t: Type = implDT(t => bool._R ->: t._R ->: t._R ->: t._R)
  }

  case object neg extends Primitive {
    override def t: Type = implDT(t => t._R ->: t._R)
  }

  case object unzip extends Primitive {
    override def t: Type = implN(n => implDT(dt1 => implDT(dt2 =>
      ArrayType(n, TupleType(dt1, dt2))._R ->: TupleType(ArrayType(n, dt1), ArrayType(n, dt2))._R
    )))
  }

  case object zip extends Primitive {
    override def t: Type = implN(n => implDT(a => implDT(b =>
      ArrayType(n, a)._R ->: ArrayType(n, b)._R ->: ArrayType(n, TupleType(a, b))._R
    )))
  }

  case object add extends Primitive {
    override def t: Type = implDT(t => t._R ->: t._R ->: t._R)
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
    override def t: Type = implDT(t => t._R ->: t._R ->: bool._R)
  }
  case object lt extends Primitive {
    override def t: Type = gt.t
  }
  case object equal extends Primitive {
    override def t: Type = gt.t
  }

  // TODO: should vectorisation be in the core or not?
  // TODO: ask for a scalar type parameter instead of casting

  case object asVector extends Primitive {
    override def t: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(m * n, t)._R ->: ArrayType(m, VectorType(n, t))._R
    )))
  }

  case object asScalar extends Primitive {
    override def t: Type = implN(n => implN(m => implDT(t =>
      ArrayType(m, VectorType(n, t))._R ->: ArrayType(m * n, t)._R
    )))
  }

  case object vectorFromScalar extends Primitive {
    override def t: Type = implN(n => implDT(t =>
      t._R ->: VectorType(n, t)._R
    ))
  }
}