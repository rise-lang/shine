package lift.core

import lift.arithmetic.{BigSum, InclusiveIndexVar}
import lift.core.types._
import lift.core.DSL._

/** IMPORTANT NOTE! **/
/**
  * Scala's arrow operator is left-associative! But usually people mean in in a right-associative way!
  * example
  * a -> b -> c means (a -> b) -> c, not the usually implied a -> (b -> c)
  *
  * PLEASE BE EXPLICIT WITH PARENTHESISATION
  */


object primitives {

  case object asIndex extends Primitive {
    override def t: Type = nFunT(n => NatType -> IndexType(n))
  }

//  case class BinOp(op: Operators.Binary.Value) extends Primitive {
//    override def toString: String = s"$op"
//
//    override def t: Type = implT(a => a -> (a -> a))
//  }

  // TODO: ask for basic type parameters
  case object cast extends Primitive {
    override def t: Type = implT(a => implT(b => a -> b))
  }

  case object depJoin extends Primitive {
    override def t: Type = implN(n => implNNF(lenF => implT(dt => {
      DepArrayType(n, NatDataTypeLambda(n, (i: NatIdentifier) => ArrayType(lenF(i), dt))) ->
        ArrayType(BigSum(from = 0, upTo = n - 1, (n: InclusiveIndexVar) => lenF(n)), dt)
    })))
  }

  case object depMapSeq extends Primitive {
    override def t: Type = implN(n => implNDF(ft1 => implNDF(ft2 =>
      nFunT(k => ft1(k) -> ft2(k)) -> (DepArrayType(n, ft1) -> DepArrayType(n, ft2))
    )))
  }

  case object drop extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(m, a)
    )))
  }

  case class ForeignFunctionCall(decl: ForeignFunctionDecl, override val t: Type) extends Primitive {
    override def toString: String = decl.name
  }

  case class ForeignFunctionDecl(name: String, args: Seq[String], body: String)

  case object fst extends Primitive {
    override def t: Type = implT(a => implT(b => TupleType(a, b) -> a))
  }

  case object generate extends Primitive {
    override def t: Type = implN(n => implT(a =>
      (IndexType(n) -> a) -> ArrayType(n, a)
    ))
  }

  case object idx extends Primitive {
    override def t: Type = implN(n => implT(a =>
      IndexType(n) -> (ArrayType(n, a) -> a)
    ))
  }

  case object indexAsNat extends Primitive {
    override def t: Type = implN(n =>
      IndexType(n) -> NatType
    )
  }

  case object iterate extends Primitive {
    override def t: Type = implN(n => implN(m => nFunT(k => implT(a =>
      nFunT(l => ArrayType(l * n, a) -> ArrayType(l, a)) ->
        (ArrayType(m * n.pow(k), a) -> ArrayType(m, a))
    ))))
  }

  case object join extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(n * m, dt)
    )))
  }

  case object map extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> b) -> (ArrayType(n, a) -> ArrayType(n, b))
    )))
  }

  case object mapSeq extends Primitive {
    override def t: Type = map.t
  }

  // TODO? could be expressed in terms of a pad idx -> val
  case object padCst extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(r => implT(a =>
      a -> (ArrayType(n, a) -> ArrayType(l + n + r, a))
    ))))
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  case object padClamp extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(r => implT(a =>
      ArrayType(n, a) -> ArrayType(l + n + r, a)
    ))))
  }

  case object reduce extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> (b -> b)) -> (b -> (ArrayType(n, a) -> b))
    )))
  }

  case object reduceSeq extends Primitive {
    override def t: Type = reduce.t
  }

  case object reduceSeqUnroll extends Primitive {
    override def t: Type = reduce.t
  }

  case object reorder extends Primitive {
    override def t: Type = implN(n => implT(a =>
      (IndexType(n) -> IndexType(n)) -> (// idxF
        (IndexType(n) -> IndexType(n)) -> (// idxFinv
          ArrayType(n, a) -> ArrayType(n, a)))
    ))
  }

  case object scan extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> (b -> b)) -> (b -> (ArrayType(n, a) -> ArrayType(n, b)))
    )))
  }

  case object scanSeq extends Primitive {
    override def t: Type = scan.t
  }

  case object slide extends Primitive {
    override def t: Type = implN(n => nFunT(sz => nFunT(sp => implT(dt => {
      val inputSize = sp * n + sz - sp
      ArrayType(inputSize, dt) -> ArrayType(n, ArrayType(sz, dt))
    }))))
  }

  object slideSeq {
    trait Rotate {}
    case object Values extends Rotate {}
    case object Indices extends Rotate {}
  }

  case class slideSeq(rot: slideSeq.Rotate) extends Primitive {
    override def t: Type = slide.t
  }

  case object slideSeqBuffer extends Primitive {
    override def t: Type = slide.t
  }

  case object snd extends Primitive {
    override def t: Type = implT(a => implT(b => TupleType(a, b) -> b))
  }

  case object split extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(dt =>
      ArrayType(m * n, dt) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

  case object take extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(n, a)
    )))
  }

  case object transpose extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

//  case class UnaryOp(op: Operators.Unary.Value) extends Primitive {
//    override def toString: String = s"$op"
//  }

  // if-then-else
  case object select extends Primitive {
    override def t: Type = implT(a => bool -> (a -> (a -> a)))
  }

  case object neg extends Primitive {
    override def t: Type = implT(a => a -> a)
  }

  case object unzip extends Primitive {
    override def t: Type = implN(n => implT(dt1 => implT(dt2 =>
      ArrayType(n, TupleType(dt1, dt2)) -> TupleType(ArrayType(n, dt1), ArrayType(n, dt2))
    )))
  }

  case object zip extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      ArrayType(n, a) -> (ArrayType(n, b) -> ArrayType(n, TupleType(a, b)))
    )))
  }

  case object add extends Primitive {
    override def t: Type = implT(a => a -> (a -> a))
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
    override def t: Type = implT(a => a -> (a -> bool))
  }
  case object lt extends Primitive {
    override def t: Type = gt.t
  }
  case object equal extends Primitive {
    override def t: Type = gt.t
  }

  case class ForeignFunDecl(name: String, args: Seq[String], body: String)

  case class ForeignFun(decl: ForeignFunDecl, override val t: Type) extends Primitive {
    override def toString: String = decl.name
  }

  // TODO: should vectorisation be in the core or not?
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