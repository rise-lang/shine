package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}
import lift.arithmetic.{Cst, InclusiveIndexVar, NamedVar}

import scala.language.implicitConversions

object depMapSeq {

  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] = fun(x => withIndex(f,x))
  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]], x:DataExpr): DepMapSeq= DepMapSeq(f, x, None)

  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => depMapSeq(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapSeq = DepMapSeq(dFun(_ => f), x, None)
}

object depMapSeqUnroll {

  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] = fun(x => withIndex(f,x))
  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]], x:DataExpr): DepMapSeqUnroll= DepMapSeqUnroll(f, x, None)

  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => depMapSeqUnroll(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapSeqUnroll = DepMapSeqUnroll(dFun(_ => f), x, None)
}


object mapSeq {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => mapSeq(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapSeq = MapSeq(f, x)
}

object map {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => map(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): Map = Map(f, x)
}

object mapOut {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => mapOut(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapOut = MapOut(f, x)
}

object zip {
  def apply(lhs: DataExpr, rhs: DataExpr): Zip = Zip(lhs, rhs, None)
}

object unzip {
  def apply(e: DataExpr): Unzip = Unzip(e, None)
}

object split {
  def apply(n: Nat): Expr[DataType -> DataType] = fun(array => split(n, array))

  def apply(n: Nat, array: DataExpr): Split = Split(n, array, None)
}

object join {
  def apply(): Expr[DataType -> DataType] = fun(array => join(array))

  def apply(array: DataExpr): Join = Join(array, None)

  implicit def toJoin(j: join.type): Expr[DataType -> DataType] = join()
}

object partition {

  def apply(m: Nat, f:NatIdentifier => Nat): Expr[DataType -> DataType] = fun(array => partition(m, f, array))

  def apply(m:Nat, f:NatIdentifier => Nat, array: DataExpr): Partition = {
    val ident = InclusiveIndexVar("p", Cst(0), m)
    Partition(m, ident, f(ident), array, None)
  }
}


object slide {
  def apply(s1: Nat, s2: Nat): Expr[DataType -> DataType] = fun(array => slide(s1, s2, array))

  def apply(s1: Nat, s2: Nat, array: DataExpr): Slide = Slide(s1, s2, array, None)
}

object mapSeqSlide {
  def apply(size: Nat, step: Nat, f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => mapSeqSlide(size, step, f, x))

  def apply(size: Nat, step: Nat, f: Expr[DataType -> DataType], x: DataExpr): MapSeqSlide =
    MapSeqSlide(size, step, f, x, None)
}

object take {
  def apply(n:Nat): Expr[DataType -> DataType] = fun(array => take(n, array))

  def apply(n:Nat, array:DataExpr):Take = Take(n, array, None)
}

object drop {
  def apply(n:Nat): Expr[DataType -> DataType] = fun(array => drop(n, array))

  def apply(n:Nat, array:DataExpr):Drop = Drop(n, array, None)
}

object pad {

  def apply(l:Nat, r:Nat, pad:DataExpr):Expr[DataType -> DataType] = fun(array => Pad(l, r, pad, array, None))

  def apply(l:Nat, r:Nat, pad:DataExpr, array:DataExpr):Pad = Pad(l, r, pad, array, None)
}

object slice {
  def apply(start:Nat, length:Nat): Expr[DataType -> DataType] = drop(start) >>> take(length)
}

object reduceSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr): Expr[DataType -> DataType] =
    fun(array => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr): ReduceSeq =
    ReduceSeq(f, init, array, None)
}

object reduceSeqUnroll {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduceSeqUnroll(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr): Expr[DataType -> DataType] =
    fun(array => reduceSeqUnroll(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr): ReduceSeqUnroll =
    ReduceSeqUnroll(f, init, array, None)
}

object scanSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => scanSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    fun(array => scanSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr) =
    ScanSeq(f, init, array, None)
}

object iterate {
  def apply(k: Nat, f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] =
    fun(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Expr[`(nat)->`[DataType -> DataType]],
            array: DataExpr): Iterate =
    Iterate(k, f, array, None)
}

object gather {
  def apply(idxF: Expr[`(nat)->`[DataType ->DataType]]): Expr[DataType -> DataType] = {
    val idxF_ = idxF(NamedVar(newName()))
    fun(array => Gather(idxF_, array, None))
  }
}

object scatter {
  def apply(idxF: Expr[`(nat)->`[DataType ->DataType]]): Expr[DataType -> DataType] = {
    val idxF_ = idxF(NamedVar(newName()))
    fun(array => Scatter(idxF_, array, None))
  }
}

object transpose {
  def apply(): Expr[DataType -> DataType] = fun(array => transpose(array))

  def apply(array: DataExpr): Transpose = Transpose(array, None)

  implicit def toTranspose(t: transpose.type): Expr[DataType -> DataType] = transpose()
}

object transposeW {
  def apply(): Expr[DataType -> DataType] = fun(array => transposeW(array))

  def apply(array: DataExpr): TransposeOnWrite = TransposeOnWrite(array, None)

  implicit def toTransposeW(t: transposeW.type): Expr[DataType -> DataType] = transposeW()
}

object withIndex {
  def apply(): Expr[DataType -> DataType] = fun(array => withIndex(array))

  def apply(array: DataExpr): WithIndex = WithIndex(array, None)
}

object tuple {
  def apply(fst: DataExpr, snd: DataExpr): Tuple = Tuple(fst, snd, None)
}

object asVector {
  def apply(n: Nat): Expr[DataType -> DataType] =
    fun(array => asVector(n, array))

  def apply(n: Nat, array: DataExpr): AsVector =
    AsVector(n, array)
}

object asScalar {
  def apply(): Expr[DataType -> DataType] = fun(array => asScalar(array))

  def apply(array: DataExpr): AsScalar = AsScalar(array)

  implicit def toAsScalar(a: asScalar.type): Expr[DataType -> DataType] = asScalar()
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralExpr(VectorData(Vector.fill(len)(FloatData(f))))

  def apply(len: Nat, e: DataExpr) = VectorFromScalar(len, e)
}

object foreignFun {
  def apply(returnT: DataType, name: String, param: (DataType, String), body: String, arg: DataExpr) =
    ForeignFunction(ForeignFunction.Declaration(name, Seq(param._2), body),
      Seq(param._1), returnT, Seq(arg))

  def apply(returnT: DataType, name: String, params: Seq[(DataType, String)], body: String, args: Seq[DataExpr]) =
    ForeignFunction(ForeignFunction.Declaration(name, params.map(_._2), body),
      params.map(_._1), returnT, args)
}

object printType {
  def apply(msg: String = ""): Expr[DataType -> DataType] = fun(x => PrintType(x, msg, None))
  def apply(msg: String, x: Expr[DataType]) = PrintType(x, msg, None)
}