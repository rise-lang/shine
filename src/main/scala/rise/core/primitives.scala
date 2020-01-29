package rise.core

import arithexpr.arithmetic.BigSum
import rise.core.TypeLevelDSL._
import rise.core.types._
import primitiveMacro.Primitive.primitive

object primitives {

  case class Annotation(e: Expr, annotation: Type) extends Primitive {
    override val t: Type = TypePlaceholder
    override def typeScheme: Type =
      throw TypeException("cannot get the type scheme of an annotated Expr")
    override def setType(t: Type): Annotation =
      throw TypeException("cannot set the type of an annotated Expr")
    override def name: String = s"Annotated Expr: $annotation"
  }

  @primitive case class MakeArray(n: Int)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    private def tRec(m: Int, dt: DataType): Type =
      if (m <= 0) {
        ArrayType(n, dt)
      } else {
        dt ->: tRec(m - 1, dt)
      }
    override def typeScheme: Type = implDT(t => tRec(n, t))
  }

  @primitive case class Cast()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(s => implBT(t => s ->: t))
  }

  @primitive case class DepJoin()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN2N(lenF =>
          implDT(dt => {
            DepArrayType(n, n2dtFun(n)(i => ArrayType(lenF(i), dt))) ->:
              ArrayType(BigSum(from = 0, upTo = n - 1, n => lenF(n)), dt)
          })
        )
      )
  }

  @primitive case class DepMapSeq()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN2DT(ft1 =>
          implN2DT(ft2 =>
            nFunT(k => ft1(k) ->: ft2(k)) ->: DepArrayType(n, ft1) ->: DepArrayType(
              n,
              ft2
            )
          )
        )
      )
  }

  @primitive case class DepZip()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN2DT(ft1 =>
          implN2DT(ft2 =>
            DepArrayType(n, ft1) ->: DepArrayType(n, ft2) ->: DepArrayType(
              n,
              n2dtFun(i => PairType(ft1(i), ft2(i)))
            )
          )
        )
      )
  }

  @primitive case class Drop()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      nFunT(n =>
        implN(m => implDT(t => ArrayType(n + m, t) ->: ArrayType(m, t)))
      )
  }

  @primitive case class Fst()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s => implDT(t => PairType(s, t) ->: s))
  }

  @primitive case class Gather()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN(m =>
          implDT(t =>
            ArrayType(m, IndexType(n)) ->: ArrayType(n, t) ->: ArrayType(m, t)
          )
        )
      )
  }

  @primitive case class Generate()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n => implDT(t => (IndexType(n) ->: t) ->: ArrayType(n, t)))
  }

  @primitive case class Idx()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n => implDT(t => IndexType(n) ->: ArrayType(n, t) ->: t))
  }

  @primitive case class Id()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t)
  }

  @primitive case class IndexAsNat()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implN(n => IndexType(n) ->: NatType)
  }

  @primitive case class Iterate()(override val t: Type = TypePlaceholder)
      extends Primitive {
    // format: off
    override def typeScheme: Type =
      implN(n =>
        implN(m =>
          nFunT(k =>
            implDT(t => 
              nFunT(l =>
                ArrayType(l * n, t) ->: ArrayType(l, t)) ->: 
                  ArrayType(m * n.pow(k), t)->: ArrayType(m, t)
            )
          )
        )
      )
    // format: on
  }

  @primitive case class Join()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN(m =>
          implDT(t => ArrayType(n, ArrayType(m, t)) ->: ArrayType(n * m, t))
        )
      )
  }

  @primitive case class Let()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s => implDT(t => (s ->: t) ->: (s ->: t)))
  }

  @primitive case class Map()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class MapFst()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s =>
        implDT(t =>
          implDT(s2 => (s ->: s2) ->: PairType(s, t) ->: PairType(s2, t))
        )
      )
  }

  @primitive case class MapSnd()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s =>
        implDT(t =>
          implDT(t2 => (t ->: t2) ->: PairType(s, t) ->: PairType(s, t2))
        )
      )
  }

  @primitive case class MapSeq()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class MapSeqUnroll()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
        )
      )
  }

  @primitive case class ToMem()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t)
  }

  @primitive case class NatAsIndex()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = nFunT(n => NatType ->: IndexType(n))
  }

  // TODO? could be expressed in terms of a pad idx -> val
  @primitive case class PadCst()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        nFunT(l =>
          nFunT(q =>
            implDT(t => t ->: ArrayType(n, t) ->: ArrayType(l + n + q, t))
          )
        )
      )
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  @primitive case class PadClamp()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        nFunT(l =>
          nFunT(q => implDT(t => ArrayType(n, t) ->: ArrayType(l + n + q, t)))
        )
      )
  }

  @primitive case class Partition()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(dt =>
          nFunT(m =>
            n2nFunT(lenF =>
              ArrayType(n, dt) ->: DepArrayType(
                m,
                n2dtFun(m)(i => ArrayType(lenF(i), dt))
              )
            )
          )
        )
      )
  }

  @primitive case class Pair()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s => implDT(t => s ->: t ->: PairType(s, t)))
  }

  @primitive case class Reduce()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n => implDT(t => (t ->: t ->: t) ->: t ->: ArrayType(n, t) ->: t))
  }

  @primitive case class ReduceSeq()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s => implDT(t => (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t)
        )
      )
  }

  @primitive case class ReduceSeqUnroll()(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s => implDT(t => (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t)
        )
      )
  }

  @primitive case class Reorder()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(t =>
          (IndexType(n) ->: IndexType(n)) ->: // idxF
            (IndexType(n) ->: IndexType(n)) ->: // idxFinv
            ArrayType(n, t) ->: ArrayType(n, t)
        )
      )
  }

  @primitive case class ScanSeq()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(s =>
          implDT(t =>
            (s ->: t ->: t) ->: t ->: ArrayType(n, s) ->: ArrayType(n, t)
          )
        )
      )
  }

  @primitive case class Slide()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        nFunT(sz =>
          nFunT(sp =>
            implDT(t => {
              ArrayType(sp * n + sz, t) ->: ArrayType(1 + n, ArrayType(sz, t))
            })
          )
        )
      )
  }

  object SlideSeq {
    trait Rotate {}
    case object Values extends Rotate {}
    case object Indices extends Rotate {}
  }

  @primitive case class SlideSeq(rot: SlideSeq.Rotate)(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    // format: off
    override def typeScheme: Type =
      implN(n =>
        nFunT(sz =>
          nFunT(sp =>
            implDT(s =>
              implDT(t =>
                (s ->: s) ->: (ArrayType(sz, s) ->: t) ->:
                  ArrayType(sp * n + sz, s) ->: ArrayType(1 + n, t)
              )
            )
          )
        )
      )
    // format: on
  }

  @primitive case class Snd()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implDT(s => implDT(t => PairType(s, t) ->: t))
  }

  @primitive case class Split()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      nFunT(n =>
        implN(m =>
          implDT(t => ArrayType(m * n, t) ->: ArrayType(m, ArrayType(n, t)))
        )
      )
  }

  @primitive case class Take()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      nFunT(n =>
        implN(m => implDT(t => ArrayType(n + m, t) ->: ArrayType(n, t)))
      )
  }

  @primitive case class Transpose()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN(m =>
          implDT(dt =>
            ArrayType(n, ArrayType(m, dt)) ->: ArrayType(m, ArrayType(n, dt))
          )
        )
      )
  }

  // if-then-else
  @primitive case class Select()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implDT(t => bool ->: t ->: t ->: t)
  }

  @primitive case class Unzip()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(dt1 =>
          implDT(dt2 =>
            ArrayType(n, PairType(dt1, dt2)) ->: PairType(
              ArrayType(n, dt1),
              ArrayType(n, dt2)
            )
          )
        )
      )
  }

  @primitive case class Zip()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implDT(a =>
          implDT(b =>
            ArrayType(n, a) ->: ArrayType(n, b) ->: ArrayType(n, PairType(a, b))
          )
        )
      )
  }

  @primitive case class Neg()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t)
  }

  @primitive case class Not()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = bool ->: bool
  }

  @primitive case class Add()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: t)
  }

  @primitive case class Sub()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: t)
  }

  @primitive case class Mul()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: t)
  }

  @primitive case class Div()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: t)
  }

  @primitive case class Mod()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: t)
  }

  @primitive case class Gt()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: bool)
  }

  @primitive case class Lt()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: bool)
  }

  @primitive case class Equal()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type = implBT(t => t ->: t ->: bool)
  }

  // TODO: should vectorisation be in the core or not?

  // TODO: track alignment in type system?
  @primitive case class AsVectorAligned()(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      nFunT(n =>
        implN(m =>
          implDT(a => ArrayType(m * n, a) ->: ArrayType(m, VectorType(n, a)))
        )
      )
  }

  @primitive case class AsVector()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      nFunT(n =>
        implN(m =>
          implST(t => ArrayType(m * n, t) ->: ArrayType(m, VectorType(n, t)))
        )
      )
  }

  @primitive case class AsScalar()(override val t: Type = TypePlaceholder)
      extends Primitive {
    override def typeScheme: Type =
      implN(n =>
        implN(m =>
          implST(t => ArrayType(m, VectorType(n, t)) ->: ArrayType(m * n, t))
        )
      )
  }

  @primitive case class VectorFromScalar()(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type =
      implN(n => implST(t => t ->: VectorType(n, t)))
  }

  @primitive case class PrintType(msg: String = "")(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type = implT(t => t ->: t)
  }

  @primitive case class TypeHole(msg: String = "")(
      override val t: Type = TypePlaceholder
  ) extends Primitive {
    override def typeScheme: Type = implT(t => t)
  }
}
