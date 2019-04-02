package lift.core

import lift.core.types._
import lift.arithmetic._

object primitives {
  private def nFun(f: NatIdentifier => Type): Type = {
    val n = NamedVar(freshName("n"))
    NatDependentFunctionType(n, f(n))
  }

  private def implN(f: NatIdentifier => Type): Type = {
    f(NamedVar(freshName("n")))
  }

  private def tFun(f: DataTypeIdentifier => Type): Type = {
    val dt = DataTypeIdentifier(freshName("dt"))
    TypeDependentFunctionType(dt, f(dt))
  }

  private def implT(f: DataTypeIdentifier => Type): Type = {
    f(DataTypeIdentifier(freshName("dt")))
  }

  private def fnFun(f: NatNatFunctionIdentifier => Type): Type = {
    val fn = NatNatFunctionIdentifier(freshName("fn"))
    NatNatDependentFunctionType(fn, f(fn))
  }

  private def implFn(f: NatNatFunctionIdentifier => Type): Type = {
    f(NatNatFunctionIdentifier(freshName("fn")))
  }

  private def fdtFun(f: NatDataTypeFunctionIdentifier => Type): Type = {
    val fdt = NatDataTypeFunctionIdentifier(freshName("fdt"))
    NatDataTypeDependentFunctionType(fdt, f(fdt))
  }

  private def implFdt(f: NatDataTypeFunctionIdentifier => Type): Type = {
    f(NatDataTypeFunctionIdentifier(freshName("fdt")))
  }

  implicit private final class To(private val a: Type) extends AnyVal {
    @inline def ->(b: Type): Type = FunctionType(a, b)
  }

  implicit private final class NatDataTypeApplyHelper(val f: lift.core.types.NatDataTypeFunction) {
    def apply(n: Nat): DataType = {
      NatDataTypeApply(f, n)
    }
  }

  implicit private final class NatNatApplyHelper(val f: lift.core.types.NatNatFunction) {
    def apply(n: Nat): Nat = {
      //NatDataTypeApply(f, n)
      ???
    }
  }

  case object map extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> b) -> ArrayType(n, a) -> ArrayType(n, b)
    )))
  }

  case object mapSeq extends Primitive {
    override def t: Type = map.t
  }

  case object depMap extends Primitive {
    override def t: Type = implN(n => implFdt(fdt1 => implFdt(fdt2 =>
      nFun(k => fdt1(k) -> fdt2(k)) -> DepArrayType(n, fdt1) -> DepArrayType(n, fdt2)
    )))
  }

  case object reduce extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      (a -> b -> b) -> b -> ArrayType(n, a) -> b
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

  case object transpose extends Primitive {
    override def t: Type = implN(n => implN(m => implT(dt =>
      ArrayType(n, ArrayType(m, dt)) -> ArrayType(m, ArrayType(n, dt))
    )))
  }

  case object zip extends Primitive {
    override def t: Type = implN(n => implT(a => implT(b =>
      ArrayType(n, a) -> ArrayType(n, b) -> ArrayType(n, TupleType(a, b))
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

    override def t: Type = implT(a => a -> a -> a)
  }

  case class BinOp(op: Operators.Binary.Value) extends Primitive {
    override def toString: String = s"($op)"

    override def t: Type = implT(a => a -> a -> a)
  }

  case object IfThenElse extends Primitive {
    override def toString: String = s"if then else"

    override def t: Type = implT(a => bool -> a -> a)
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