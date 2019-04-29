package lift.core

import lift.core.types._
import lift.core.DSL._

object primitives {
  case object map extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> b) -> (ArrayType(n, a) -> ArrayType(n, b))
    )))
  }

  case object mapSeq extends Primitive {
    override def t: Type = map.t
  }

  case object reduce extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> (b -> b)) -> (b -> (ArrayType(n, a) -> b))
    )))
  }

  case object reduceSeq extends Primitive {
    override def t: Type = reduce.t
  }

  case object scan extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> (b -> b)) -> (b -> (ArrayType(n, a) -> ArrayType(n, b)))
    )))
  }

  case object scanSeq extends Primitive {
    override def t: Type = scan.t
  }

  case object join extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(n * m, dt)
    )))
  }

  case object split extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(dt =>
      ArrayType(m * n, dt) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

  case object slide extends Primitive {
    override def t: Type = implN(n => nFunT(sz => nFunT(sp => implT(dt => {
      val inputSize = sp * n + sz - sp
      ArrayType(inputSize, dt) -> ArrayType(n, ArrayType(sz, dt))
    }))))
  }

  case object slideSeq extends Primitive {
    override def t: Type = slide.t
  }

  case object reorder extends Primitive {
    override def t: Type = implN(n => implT(a =>
      (IndexType(n) -> IndexType(n)) -> (// idxF
        (IndexType(n) -> IndexType(n)) -> (// idxFinv
          ArrayType(n, a) -> ArrayType(n, a)))
    ))
  }

  case object transpose extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

  case object take extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(n, a)
    )))
  }

  case object drop extends Primitive {
    override def t: Type = nFunT(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(m, a)
    )))
  }

  case object padCst extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(r => implT(a =>
      a -> (ArrayType(n, a) -> ArrayType(l + n + r, a))
    ))))
  }

  case object padClamp extends Primitive {
    override def t: Type = implN(n => nFunT(l => nFunT(r => implT(a =>
      ArrayType(n, a) -> ArrayType(l + n + r, a)
    ))))
  }

  case object zip extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      ArrayType(n, a) -> (ArrayType(n, b) -> ArrayType(n, TupleType(a, b)))
    )))
  }

  case object fst extends Primitive {
    override def t: Type = implT(a => implT(b => TupleType(a, b) -> a))
  }

  case object snd extends Primitive {
    override def t: Type = implT(a => implT(b => TupleType(a, b) -> b))
  }

  case object idx extends Primitive {
    override def t: Type = implN(n => implT(a =>
      IndexType(n) -> (ArrayType(n, a) -> a)
    ))
  }

  // if-then-else
  case object select extends Primitive {
    override def t: Type = implT(a => bool -> (a -> (a -> a)))
  }

  case object neg extends Primitive {
    override def t: Type = implT(a => a -> a)
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

  // TODO: ask for basic type parameters
  case object cast extends Primitive {
    override def t: Type = implT(a => implT(b => a -> b))
  }

  case class ForeignFunDecl(name: String, args: Seq[String], body: String)

  case class ForeignFun(decl: ForeignFunDecl, override val t: Type) extends Primitive {
    override def toString: String = decl.name
  }

  case object generate extends Primitive {
    override def t: Type = implN(n => implT(a =>
      (IndexType(n) -> a) -> ArrayType(n, a)
    ))
  }

  case object iterate extends Primitive {
    override def t: Type = implN(n => implN(m => nFunT(k => implT(a =>
      nFunT(l => ArrayType(l * n, a) -> ArrayType(l, a)) ->
        (ArrayType(m * n.pow(k), a) -> ArrayType(m, a))
    ))))
  }

  case object indexAsNat extends Primitive {
    override def t: Type = implN(n =>
      IndexType(n) -> NatType
    )
  }
}