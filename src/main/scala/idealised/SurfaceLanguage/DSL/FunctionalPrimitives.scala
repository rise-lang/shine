package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.NamedVar


object map {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => map(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): Map = Map(f, x)
}

object zip {
  def apply(lhs: DataExpr, rhs: DataExpr): Zip = Zip(lhs, rhs)
}

object unzip {
  def apply(e: DataExpr): Unzip = Unzip(e)
}

object split {
  def apply(n: Nat): Expr[DataType -> DataType] = fun(array => split(n, array))

  def apply(n: Nat, array: DataExpr): Split = Split(n, array)
}

object join {
  def apply(): Expr[DataType -> DataType] = fun(array => join(array))

  def apply(array: DataExpr): Join = Join(array)
}

object slide {
  def apply(s1: Nat, s2: Nat): Expr[DataType -> DataType] = fun(array => slide(s1, s2, array))

  def apply(s1: Nat, s2: Nat, array: DataExpr): Slide = Slide(s1, s2, array)
}

object take {
  def apply(n:Nat): Expr[DataType -> DataType] = fun(array => take(n, array))

  def apply(n:Nat, array:DataExpr):Take = Take(n, array)
}

object drop {
  def apply(n:Nat): Expr[DataType -> DataType] = fun(array => drop(n, array))

  def apply(n:Nat, array:DataExpr):Drop = Drop(n, array)
}

object reduce {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduce(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr): Expr[DataType -> DataType] =
    fun(array => reduce(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr): Reduce =
    Reduce(f, init, array)
}

object iterate {
  def apply(k: Nat, f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] =
    fun(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Expr[`(nat)->`[DataType -> DataType]],
            array: DataExpr): Iterate =
    Iterate(k, f, array)
}

object gather {
  def apply(idxF: Expr[`(nat)->`[DataType ->DataType]]): Expr[DataType -> DataType] = {
    val idxF_ = idxF(NamedVar(newName()))
    fun(array => Gather(idxF_, array))
  }
}

object scatter {
  def apply(idxF: Expr[`(nat)->`[DataType ->DataType]]): Expr[DataType -> DataType] = {
    val idxF_ = idxF(NamedVar(newName()))
    fun(array => Scatter(idxF_, array))
  }
}

object transpose {
  def apply(): Expr[DataType -> DataType] = fun(array => transpose(array))

  def apply(array: DataExpr): Transpose = Transpose(array)
}

object transposeW {
  def apply(): Expr[DataType -> DataType] = fun(array => transposeW(array))

  def apply(array: DataExpr): TransposeOnWrite = TransposeOnWrite(array)
}

object tuple {
  def apply(fst: DataExpr, snd: DataExpr): Tuple = Tuple(fst, snd)
}

object foreignFun {
  def apply(returnT: DataType, name: String, param: (DataType, String), body: String, arg: DataExpr) =
    ForeignFunction(ForeignFunctionDeclaration(name, Seq(param._2), body),
      Seq(param._1), returnT, Seq(arg))

  def apply(returnT: DataType, name: String, params: Seq[(DataType, String)], body: String, args: Seq[DataExpr]) =
    ForeignFunction(ForeignFunctionDeclaration(name, params.map(_._2), body),
      params.map(_._1), returnT, args)
}

object printType {
  def apply(msg: String = ""): Expr[DataType -> DataType] = fun(x => PrintType(x, msg))
  def apply(msg: String, x: Expr[DataType]) = PrintType(x, msg)
}