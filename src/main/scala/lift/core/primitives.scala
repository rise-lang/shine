package lift.core

import lift.arithmetic.BigSum
import lift.core.DSL._
import lift.core.types._
import lift.core.Primitive
import primitiveMacro.Primitive.primitive

object primitives {

  @primitive case class Let() extends Primitive {
    override def typeScheme: Type = implDT(s => implDT(t =>
      (s ->: t) ->: (s ->: t)
    ))
  }

  @primitive case class Cast() extends Primitive {
    override def typeScheme: Type = implBT(s => implBT(t => s ->: t))
  }

  @primitive case class DepJoin() extends Primitive {
    override def typeScheme: Type = implN(n => implN2N(lenF => implDT(dt => {
      DepArrayType(n, n2dtFun(n)(i => ArrayType(lenF(i), dt))) ->:
        ArrayType(BigSum(from = 0, upTo = n - 1, n => lenF(n)), dt)
    })))
  }

  @primitive case class DepMapSeq() extends Primitive {
    override def typeScheme: Type = implN(n => implN2DT(ft1 => implN2DT(ft2 =>
      nFunT(k => ft1(k) ->: ft2(k)) ->: DepArrayType(n, ft1) ->: DepArrayType(n, ft2)
    )))
  }

  @primitive case class DepZip() extends Primitive {
    override def typeScheme: Type = implN(n => implN2DT(ft1 => implN2DT(ft2 =>
      DepArrayType(n, ft1) ->: DepArrayType(n, ft2) ->: DepArrayType(n, n2dtFun(i => TupleType(ft1(i), ft2(i))))
    )))
  }

  @primitive case class Drop() extends Primitive {
    override def typeScheme: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(n + m, t) ->: ArrayType(m, t)
    )))
  }

  @primitive case class Fst() extends Primitive {
    override def typeScheme: Type = implDT(s => implDT(t => TupleType(s, t) ->: s))
  }

  @primitive case class Gather() extends Primitive {
    override def typeScheme: Type = implN(n => implN(m => implDT(t =>
      ArrayType(m, IndexType(n)) ->: ArrayType(n, t) ->: ArrayType(m, t)
    )))
  }

  @primitive case class Generate() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(t =>
      (IndexType(n) ->: t) ->: ArrayType(n, t)
    ))
  }

  @primitive case class Idx() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(t =>
      IndexType(n) ->: ArrayType(n, t) ->: t
    ))
  }

  @primitive case class Id() extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t)
  }

  @primitive case class IndexAsNat() extends Primitive {
    override def typeScheme: Type = implN(n =>
      IndexType(n) ->: NatType
    )
  }

  @primitive case class Iterate() extends Primitive {
    override def typeScheme: Type = implN(n => implN(m => nFunT(k => implDT(t =>
      nFunT(l => ArrayType(l * n, t) ->: ArrayType(l, t)) ->:
        ArrayType(m * n.pow(k), t)->: ArrayType(m, t)
    ))))
  }

  @primitive case class Join() extends Primitive {
    override def typeScheme: Type = implN(n => implN(m => implDT(t =>
      ArrayType(n, ArrayType(m, t)) ->: ArrayType(n * m, t)
    )))
  }

  @primitive case class Map() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class MapSeq() extends Primitive {
    override def typeScheme: Type = Map().typeScheme
  }

  @primitive case class MapSeqUnroll() extends Primitive {
    override def typeScheme: Type = Map().typeScheme
  }

  @primitive case class NatAsIndex() extends Primitive {
    override def typeScheme: Type = nFunT(n => NatType ->: IndexType(n))
  }

  // TODO? could be expressed in terms of a pad idx -> val
  @primitive case class PadCst() extends Primitive {
    override def typeScheme: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      t ->: ArrayType(n, t) ->: ArrayType(l + n + q, t)
    ))))
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  @primitive case class PadClamp() extends Primitive {
    override def typeScheme: Type = implN(n => nFunT(l => nFunT(q => implDT(t =>
      ArrayType(n, t) ->: ArrayType(l + n + q, t)
    ))))
  }

  @primitive case class Partition() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(dt =>
      nFunT(m => n2nFunT(lenF =>
        ArrayType(n, dt) ->: DepArrayType(m, n2dtFun(m)(i => ArrayType(lenF(i), dt)))
      ))))
  }

  @primitive case class Pair() extends Primitive {
    override def typeScheme: Type = implDT(s => implDT(t =>
      s ->: t ->: TupleType(s, t)
    ))
  }

  @primitive case class Reduce() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(t =>
      (t ->: t ->: t) ->: t ->: ArrayType(n, t) ->: t
    ))
  }

  @primitive case class ReduceSeq() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t
    )))
  }

  @primitive case class ReduceSeqUnroll() extends Primitive {
    override def typeScheme: Type = ReduceSeq().typeScheme
  }

  @primitive case class Reorder() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(t =>
      (IndexType(n) ->: IndexType(n)) ->: // idxF
        (IndexType(n) ->: IndexType(n)) ->: // idxFinv
          ArrayType(n, t) ->: ArrayType(n, t)
    ))
  }

  @primitive case class ScanSeq() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(s => implDT(t =>
      (s ->: t ->: t) ->: t ->: ArrayType(n, s) ->: ArrayType(n, t)
    )))
  }

  @primitive case class Slide() extends Primitive {
    override def typeScheme: Type = implN(n => nFunT(sz => nFunT(sp => implDT(t => {
      ArrayType(sp * n + sz - sp, t) ->: ArrayType(n, ArrayType(sz, t))
    }))))
  }

  object SlideSeq {
    trait Rotate {}
    case object Values extends Rotate {}
    case object Indices extends Rotate {}
  }

  @primitive case class SlideSeq(rot: SlideSeq.Rotate) extends Primitive {
    override def typeScheme: Type = implN(n => nFunT(sz => nFunT(sp => implDT(s => implDT(t => {
      (s ->: s) ->: (ArrayType(sz, s) ->: t) ->:
        ArrayType(sp * n + sz - sp, s) ->: ArrayType(n, t)
    })))))
  }

  @primitive case class Snd() extends Primitive {
    override def typeScheme: Type = implDT(s => implDT(t => TupleType(s, t) ->: t))
  }

  @primitive case class Split() extends Primitive {
    override def typeScheme: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(m * n, t) ->: ArrayType(m, ArrayType(n, t))
    )))
  }

  @primitive case class Take() extends Primitive {
    override def typeScheme: Type = nFunT(n => implN(m => implDT(t =>
      ArrayType(n + m, t) ->: ArrayType(n, t)
    )))
  }

  @primitive case class Transpose() extends Primitive {
    override def typeScheme: Type = implN(n => implN(m => implDT(dt =>
      ArrayType(n, ArrayType(m, dt)) ->: ArrayType(m, ArrayType(n, dt))
    )))
  }

  // if-then-else
  @primitive case class Select() extends Primitive {
    override def typeScheme: Type = implDT(t => bool ->: t ->: t ->: t)
  }

  @primitive case class Unzip() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(dt1 => implDT(dt2 =>
      ArrayType(n, TupleType(dt1, dt2)) ->: TupleType(ArrayType(n, dt1), ArrayType(n, dt2))
    )))
  }

  @primitive case class Zip() extends Primitive {
    override def typeScheme: Type = implN(n => implDT(a => implDT(b =>
      ArrayType(n, a) ->: ArrayType(n, b) ->: ArrayType(n, TupleType(a, b))
    )))
  }

  @primitive case class Neg() extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t)
  }

  @primitive case class Add() extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t ->: t)
  }

  @primitive case class Sub() extends Primitive {
    override def typeScheme: Type = Add().typeScheme
  }

  @primitive case class Mul() extends Primitive {
    override def typeScheme: Type = Add().typeScheme
  }

  @primitive case class Div() extends Primitive {
    override def typeScheme: Type = Add().typeScheme
  }

  @primitive case class Mod() extends Primitive {
    override def typeScheme: Type = Add().typeScheme
  }

  @primitive case class Gt() extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t ->: bool)
  }

  @primitive case class Lt() extends Primitive {
    override def typeScheme: Type = Gt().typeScheme
  }

  @primitive case class Equal() extends Primitive {
    override def typeScheme: Type = Gt().typeScheme
  }

  // TODO: should vectorisation be in the core or not?

  // TODO: track alignment in type system?
  @primitive case class AsVectorAligned() extends Primitive {
    override def typeScheme: Type = nFunT(n => implN(m => implDT(a =>
      ArrayType(m * n, a) ->: ArrayType(m, VectorType(n, a))
    )))
  }

  @primitive case class AsVector() extends Primitive {
    override def typeScheme: Type = nFunT(n => implN(m => implST(t =>
      ArrayType(m * n, t) ->: ArrayType(m, VectorType(n, t))
    )))
  }

  @primitive case class AsScalar() extends Primitive {
    override def typeScheme: Type = implN(n => implN(m => implST(t =>
      ArrayType(m, VectorType(n, t)) ->: ArrayType(m * n, t)
    )))
  }

  @primitive case class VectorFromScalar() extends Primitive {
    override def typeScheme: Type = implN(n => implST(t =>
      t ->: VectorType(n, t)
    ))
  }

  @primitive case class PrintType(msg: String = "") extends Primitive {
    override def typeScheme: Type = implT(t => t ->: t)
  }

  @primitive case class TypeHole(msg: String = "") extends Primitive {
    override def typeScheme: Type = implT(t => t)
  }
}