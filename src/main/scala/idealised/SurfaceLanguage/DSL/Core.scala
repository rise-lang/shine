package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import lift.arithmetic.NamedVar

object identifier {
  def apply(name: String): IdentifierExpr = IdentifierExpr(name, None)
  def apply(name: String, dt: DataType): IdentifierExpr = IdentifierExpr(name, Some(dt))
}

object fun {
  def apply[T <: Type](f: IdentifierExpr => Expr[T]): Expr[DataType -> T] = {
    val param = identifier(newName())
    LambdaExpr(param, f(param))
  }

  def apply[T <: Type](f: (IdentifierExpr, IdentifierExpr) => Expr[T]): Expr[DataType -> (DataType -> T)] = {
    val p1 = identifier(newName())
    val p2 = identifier(newName())
    LambdaExpr(p1, LambdaExpr(p2, f(p1, p2)))
  }

  def apply[T <: Type](dt: DataType)
                      (f: IdentifierExpr => Expr[T]): Expr[DataType -> T] = {
    val param = identifier(newName(), dt)
    LambdaExpr(param, f(param))
  }

}


object dFun {

  def apply[T <: Type](f: NamedVar => Expr[T]): NatDependentLambdaExpr[T] = {
    val x = NamedVar(newName())
    NatDependentLambdaExpr(x, f(x))
  }

  def apply[T <: Type](f: DataTypeIdentifier => Expr[T]): TypeDependentLambdaExpr[T] = {
    val x = DataTypeIdentifier(newName())
    TypeDependentLambdaExpr(x, f(x))
  }

}
