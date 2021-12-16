package util

object PatternMatching {

  def matchWithDefault[T,R](x : T, d : R) : PartialFunction[T,R] => R =
    _.lift(x) match { case Some(p) => p case None => d } }
