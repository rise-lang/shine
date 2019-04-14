package lift.core

import lift.core.types._
import lift.arithmetic._
import lift.core.DSL._

object primitives {
  private def nFun(f: NatIdentifier => Type): Type = {
    val x = NamedVar(freshName("n"))
    NatDependentFunctionType(x, f(x))
  }

  private def tFun(f: DataTypeIdentifier => Type): Type = {
    val x = DataTypeIdentifier(freshName("dt"))
    TypeDependentFunctionType(x, f(x))
  }

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

  case object join extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(n * m, dt)
    )))
  }

  case object split extends Primitive {
    override def t: Type = nFun(n => implN(m => implT(dt =>
      ArrayType(m * n, dt) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

  case object slide extends Primitive {
    override def t: Type = implN(n => nFun(sz => nFun(sp => implT(dt => {
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
    override def t: Type = nFun(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(n, a)
    )))
  }

  case object drop extends Primitive {
    override def t: Type = nFun(n => implN(m => implT(a =>
      ArrayType(n + m, a) -> ArrayType(m, a)
    )))
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

  case class UnaryOp(op: Operators.Unary.Value) extends Primitive {
    override def toString: String = s"$op"

    override def t: Type = implT(a => a -> a)
  }

  case class BinOp(op: Operators.Binary.Value) extends Primitive {
    override def toString: String = s"$op"

    override def t: Type = implT(a => a -> (a -> a))
  }

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
    override def t: Type = implN(n => implN(m => nFun(k => implT(a =>
      nFun(l => ArrayType(l * n, a) -> ArrayType(l, a)) ->
        (ArrayType(m * n.pow(k), a) -> ArrayType(m, a))
    ))))
  }

  case object indexAsNat extends Primitive {
    override def t: Type = implN(n =>
      IndexType(n) -> NatType
    )
  }
}

object Operators {

  object Unary extends Enumeration {
    val NEG: Unary.Value = Value("-")
  }

  object Binary extends Enumeration {
    val ADD: Binary.Value = Value("+")
    val SUB: Binary.Value = Value("-")
    val MUL: Binary.Value = Value("*")
    val DIV: Binary.Value = Value("/")
    val MOD: Binary.Value = Value("%")
    val GT: Binary.Value = Value(">")
    val LT: Binary.Value = Value("<")
    val EQ: Binary.Value = Value("==")
  }

}