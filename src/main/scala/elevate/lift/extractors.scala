package elevate.lift

import lift.core.primitives._
import lift.core.types.{Kind, Type}
import lift.core.{Apply, DepApply, DepLambda, Expr, Identifier, Lambda, Literal, Primitive, TypedExpr, semantics}

object extractors {

  object $$ {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case TypedExpr(Apply(f, e), _) => Some((f, e))
      case Apply(f,e) => Some((f,e))
      case _ => None
    }
  }

  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = e match {
      case TypedExpr(e, t) => Some((e, t))
      case _ => None
    }
  }

  object _lambda {
    def unapply(e: Expr): Option[(Identifier, Expr)] = e match {
      case TypedExpr(Lambda(x,e), _) => Some((x,e))
      case Lambda(x,e) => Some((x,e))
      case _ => None
    }
  }

  object _depLambda {
    def unapply(e: Expr): Option[(Kind#I, Expr)] = e match {
      case TypedExpr(DepLambda(x,e), _) => Some((x,e))
      case DepLambda(x,e) => Some((x,e))
      case _ => None
    }
  }

  object _apply {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case TypedExpr(Apply(f, e), _) => Some((f, e))
      case Apply(f, e) => Some((f, e))
      case _ => None
    }
  }

  object _depApply {
    def unapply(e: Expr): Option[(Expr, Kind#T)] = e match {
      case TypedExpr(DepApply(x,e), _) => Some((x,e))
      case DepApply(x,e) => Some((x,e))
      case _ => None
    }
  }

  object _literal {
    def unapply(e: Expr): Option[semantics.Data] = e match {
      case TypedExpr(Literal(d), _) => Some(d)
      case Literal(d) => Some(d)
      case _ => None
    }
  }

  object _primitive {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(p:Primitive, _) => true
      case _:Primitive => true
      case _ => false
    }
  }

  object _foreignFunction {
    def unapply(e: Expr): Option[(ForeignFunction.Decl, Type)] = e match {
      case TypedExpr(ForeignFunction(d,t), _) => Some((d,t))
      case ForeignFunction(d,t) => Some((d,t))
      case _ => None
    }
  }

  object _join {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`join`, _) => true
      case `join` => true
      case _ => false
    }
  }

  object _map {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`map`, _) => true
      case `map` => true
      case _ => false
    }
  }

  object _mapSeq {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`mapSeq`, _) => true
      case `mapSeq` => true
      case _ => false
    }
  }

  object _reduce {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`reduce`, _) => true
      case `reduce` => true
      case _ => false
    }
  }

  object _slide {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`slide`, _) => true
      case `slide` => true
      case _ => false
    }
  }

  object _split {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`split`, _) => true
      case `split` => true
      case _ => false
    }
  }

  object _transpose {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`transpose`, _) => true
      case `transpose` => true
      case _ => false
    }
  }

  object _zip {
    def unapply(e: Expr): Boolean = e match {
      case TypedExpr(`zip`, _) => true
      case `zip` => true
      case _ => false
    }
  }
}
