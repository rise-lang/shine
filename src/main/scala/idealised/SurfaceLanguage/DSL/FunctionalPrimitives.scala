package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}
import lift.arithmetic.NamedVar

import scala.language.implicitConversions

object depMapSeq {

  def withIndex(f: Expr): Expr = fun(x => withIndex(f,x))
  def withIndex(f: Expr, x: Expr): DepMapSeq= DepMapSeq(f, x, None)

  def apply(f: Expr): Expr = fun(x => depMapSeq(f, x))

  def apply(f: Expr, x: Expr): DepMapSeq = DepMapSeq(nFun(_ => f), x, None)
}

object mapSeq {
  def apply(f: Expr): Expr = fun(x => mapSeq(f, x))

  def apply(f: Expr, x: Expr): MapSeq = MapSeq(f, x)
}

object map {
  def apply(f: Expr): Expr = fun(x => map(f, x))

  def apply(f: Expr, x: Expr): Map = Map(f, x)
}

object zip {
  def apply(lhs: Expr, rhs: Expr): Zip = Zip(lhs, rhs, None)
}

object unzip {
  def apply(e: Expr): Unzip = Unzip(e, None)
}

object split {
  def apply(n: Nat): Expr = fun(array => split(n, array))

  def apply(n: Nat, array: Expr): Split = Split(n, array, None)
}

object join {
  def apply(): Expr = fun(array => join(array))

  def apply(array: Expr): Join = Join(array, None)

  implicit def toJoin(j: join.type): Expr = join()
}

object partition {
  def apply(m: Nat, f:NatIdentifier => Nat): Expr = fun(array => partition(m, f, array))

  def apply(m:Nat, f:NatIdentifier => Nat, array: Expr): Partition = {
    val ident = NamedVar("p")
    Partition(m, ident, f(ident), array, None)
  }
}


object slide {
  def apply(s1: Nat, s2: Nat): Expr = fun(array => slide(s1, s2, array))

  def apply(s1: Nat, s2: Nat, array: Expr): Slide = Slide(s1, s2, array, None)
}

object slideSeq {
  def apply(size: Nat, step: Nat): Expr =
    fun(x => slideSeq(size, step, x))

  def apply(size: Nat, step: Nat, x: Expr): SlideSeq =
    SlideSeq(size, step, x, None)
}

object take {
  def apply(n:Nat): Expr = fun(array => take(n, array))

  def apply(n:Nat, array:Expr):Take = Take(n, array, None)
}

object drop {
  def apply(n:Nat): Expr = fun(array => drop(n, array))

  def apply(n:Nat, array:Expr):Drop = Drop(n, array, None)
}

object pad {

  def apply(l:Nat, r:Nat, pad:Expr):Expr = fun(array => Pad(l, r, pad, array, None))

  def apply(l:Nat, r:Nat, pad:Expr, array:Expr):Pad = Pad(l, r, pad, array, None)
}

object slice {
  def apply(start:Nat, length:Nat): Expr = drop(start) >>> take(length)
}

object reduceSeq {
  def apply(f: Expr): Expr =
    fun((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr,
            init: Expr): Expr =
    fun(array => reduceSeq(f, init, array))

  def apply(f: Expr,
            init: Expr,
            array: Expr): ReduceSeq =
    ReduceSeq(f, init, array, None)
}

object scanSeq {
  def apply(f: Expr): Expr =
    fun((init, array) => scanSeq(f, init, array))

  def apply(f: Expr, init: Expr): Expr =
    fun(array => scanSeq(f, init, array))

  def apply(f: Expr,
            init: Expr,
            array: Expr) =
    ScanSeq(f, init, array, None)
}

object iterate {
  def apply(k: Nat, f: Expr): Expr =
    fun(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Expr,
            array: Expr): Iterate =
    Iterate(k, f, array, None)
}

object reorder {
  def apply(idxF: Expr,
            idxFinv: Expr
           ): Expr = {
    val idxF_ = idxF(NamedVar(newName()))
    val idxFinv_ = idxFinv(NamedVar(newName()))
    fun(array => Reorder(idxF_, idxFinv_, array, None))
  }
}

object transpose {
  def apply(): Expr = fun(array => transpose(array))

  def apply(array: Expr): Transpose = Transpose(array, None)

  implicit def toTranspose(t: transpose.type): Expr = transpose()
}

object tuple {
  def apply(fst: Expr, snd: Expr): Tuple = Tuple(fst, snd, None)
}

object asVector {
  def apply(n: Nat): Expr =
    fun(array => asVector(n, array))

  def apply(n: Nat, array: Expr): AsVector =
    AsVector(n, array)
}

object asScalar {
  def apply(): Expr = fun(array => asScalar(array))

  def apply(array: Expr): AsScalar = AsScalar(array)

  implicit def toAsScalar(a: asScalar.type): Expr = asScalar()
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralExpr(VectorData(Vector.fill(len)(FloatData(f))))

  def apply(len: Nat, e: Expr) = VectorFromScalar(len, e)
}

object foreignFun {
  def apply(returnT: DataType, name: String, param: (DataType, String), body: String, arg: Expr) =
    ForeignFunction(ForeignFunction.Declaration(name, Seq(param._2), body),
      Seq(param._1), returnT, Seq(arg))

  def apply(returnT: DataType, name: String, params: Seq[(DataType, String)], body: String, args: Seq[Expr]) =
    ForeignFunction(ForeignFunction.Declaration(name, params.map(_._2), body),
      params.map(_._1), returnT, args)
}

object printType {
  def apply(msg: String = ""): Expr = fun(x => PrintType(x, msg, None))
  def apply(msg: String, x: Expr) = PrintType(x, msg, None)
}