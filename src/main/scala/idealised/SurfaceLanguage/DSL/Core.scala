package idealised.SurfaceLanguage.DSL

import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage._
import lift.arithmetic.NamedVar

object identifier {
  def apply(name: String): IdentifierExpr = IdentifierExpr(name, None)
  def apply(name: String, dt: DataType): IdentifierExpr = IdentifierExpr(name, Some(dt))
  def apply(name: String, et: ExpType): IdentifierExpr = IdentifierExpr(name, Some(et.dataType))
}

trait funDef {
  def apply[T <: PhraseType](f: IdentifierExpr => Expr[T]): Expr[ExpType -> T] = {
    val param = identifier(newName())
    LambdaExpr(param, f(param))
  }

  def apply[T <: PhraseType](f: (IdentifierExpr, IdentifierExpr) => Expr[T]): Expr[ExpType -> (ExpType -> T)] = {
    val p1 = identifier(newName())
    val p2 = identifier(newName())
    LambdaExpr(p1, LambdaExpr(p2, f(p1, p2)))
  }

  def apply[T <: PhraseType](dt: DataType)
                            (f: IdentifierExpr => Expr[T]): Expr[ExpType -> T] = {
    val param = identifier(newName(), dt)
    LambdaExpr(param, f(param))
  }

  def apply[T <: PhraseType](et: ExpType)
                            (f: IdentifierExpr => Expr[T]): Expr[ExpType -> T] = {
    val param = identifier(newName(), et)
    LambdaExpr(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef


trait dependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Expr[T]): NatDependentLambdaExpr[T] = {
    val x = NamedVar(newName())
    NatDependentLambdaExpr(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Expr[T]): TypeDependentLambdaExpr[T] = {
    val x = DataTypeIdentifier(newName())
    TypeDependentLambdaExpr(x, f(x))
  }

}

object _Λ_ extends dependentFunDef

