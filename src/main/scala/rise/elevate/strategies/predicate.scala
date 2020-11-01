package rise.elevate.strategies

import elevate.core._
import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import rise.core.dsl.ToBeTyped
import rise.core.exprs.primitives._
import rise.core.exprs._
import rise.core.types._
import rise.elevate._
import rise.elevate.rules.lowering.isComputation


object predicate {

  // Matching Single Nodes

  case object isLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: Lambda => Success(l)
      case _         => Failure(isLambda)
    }
    override def toString: String = "isLambda"
  }

  case object isDepLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: DepLambda[_] => Success(l)
      case _               => Failure(isDepLambda)
    }
    override def toString: String = "isDepLambda"
  }

  case object isMakeArray extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@makeArray(_) => Success(m)
      case _            => Failure(isMakeArray)
    }
    override def toString: String = "isMakeArray"
  }

  case object isIdentifier extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case i: Identifier => Success[Rise](i)
      case _             => Failure[Rise](isIdentifier)
    }
    override def toString: String = "isIdentifier"
  }

  case object isReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@reduce() => Success(r)
      case _          => Failure(isReduce)
    }
    override def toString: String = "isReduce"
  }

  case object isTranspose extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case t@transpose() => Success(t)
      case _             => Failure(isTranspose)
    }
    override def toString = "isTranspose"
  }

  case object isReduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@reduceSeq() => Success(r)
      case _             => Failure(isReduceSeq)
    }
    override def toString = "isReduce"
  }

  case object isReduceSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@reduceSeqUnroll() => Success(r)
      case _                   => Failure(isReduceSeqUnroll)
    }
    override def toString = "isReduce"
  }

  def isReduceX: Strategy[Rise] = (isReduce <+ isReduceSeq <+ isReduceSeqUnroll)

  case object isArray extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case e ::: ArrayType(_,_) => Success(e)
      case _ => Failure(isArray)
    }
  }

  case object isGenerate extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case g@generate() => Success(g)
      case _            => Failure(isGenerate)
    }
    override def toString: String = "isGenerate"
  }

  case object isApply extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a:App => Success(a)
      case _     => Failure(isApply)
    }
    override def toString: String = "isApply"
  }

  case object isMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@map() => Success(m)
      case _       => Failure(isMap)
    }
    override def toString: String = "isMap"
  }

  case object isLet extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l@let() => Success(l)
      case _       => Failure(isLet)
    }
    override def toString = "isLet"
  }

  // Matching Applied Primitives

//  case class isApplied(s: Strategy[Rise]) extends Strategy[Rise] {
//    def apply(e: Rise): RewriteResult[Rise] = e match {
//      case App(f, _) => s(f)
//      case _         => Failure(isAppliedLet)
//    }
//    override def toString: String = "isAppliedLet"
//  }

  case object isAppliedLet extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(let(), _), _) => Success(a)
      case _                       => Failure(isAppliedLet)
    }
    override def toString: String = "isAppliedLet"
  }

  case object isAppliedMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@App(App(map(), f), arg) => Success(m)
      case _                         => Failure(isMap)
    }
    override def toString: String = "isAppliedMap"
  }

  case object isAppliedZip extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case z@App(App(zip(), a), b) => Success(z)
      case _                       => Failure(isAppliedZip)
    }
    override def toString: String = "isAppliedZip"
  }

  case object isAppliedReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@App(App(App(reduce(), op), init), arg) => Success(r)
      case _                                        => Failure(isMap)
    }
    override def toString: String = "isAppliedReduce"
  }

  case class isApplied(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(f,e) => s(f).mapSuccess(_ => a)
      case _          => Failure(isApplied(s))
    }
  }

  case class isVectorizeablePrimitive()(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(map(), f), input) if isComputation()(ev)(f) && !isVectorArray(a.t) => Success(a)
      case r@App(App(App(reduce(), op), init), input) if isComputation()(ev)(op) => Success(r)
      case _ => Failure(isVectorizeablePrimitive())
    }
  }

  def isVectorArray(t: Type): Boolean = t match {
    case ArrayType(_, VectorType(_,_)) => true
    case _ => false
  }

  case class isEqualToUntyped(x: ToBeTyped[Expr]) extends Strategy[Rise] {
    override def apply(e: Rise): RewriteResult[Rise] = {
      val p = x.toUntypedExpr
      if (p == e) Success(e) else Failure(isEqualToUntyped(x))
    }
  }
}
