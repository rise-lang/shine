package idealised.rewriting

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives._
import idealised.rewriting.Elevate.Strategy

object Rules {

  def splitJoin: Int => Strategy =
    n => {
      case Map(f, arg, _) => join() o map(map(f)) o split(n) $ arg
    }

  def mapFusion: Strategy = {
    case Map(f, Map(g, arg, _), _) => map(f o g, arg)
  }

  def mapFission: Strategy = {
    case Map(LambdaExpr(x1, ApplyExpr(f, ApplyExpr(g, x2))), arg, _) if x1 == x2 =>
      map(f) o map(g) $ arg
  }

  def copy = fun(y => y) // TODO: think about how to avoid being beta reduced ...

  def addCopy: Strategy = x => copy(x)

  def transposeTranspose: Strategy = x => (transpose() o transpose()) $ x

}
