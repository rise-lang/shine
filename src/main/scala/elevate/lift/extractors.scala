package elevate.lift

import lift.core.primitives._
import lift.core.types.{Kind, Type}
import lift.core.{ForeignFunction, Apply, DepApply, DepLambda, Expr, Identifier, Lambda, Literal, Primitive, semantics}

object extractors {

  object _typed {
    def unapply(e: Expr): Option[Expr] = e match {
      case TypedExpr(e, _) => Some(e)
      case e => Some(e)
    }
  }

  object $$ {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case _typed(Apply(f,e)) => Some((f,e))
      case _ => None
    }
  }

  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = e match {
      case TypedExpr(e, t) => Some((e, t))
      case _ => None
    }
  }

  object _identifier {
    def unapply(e: Expr): Option[String] = e match {
      case _typed(Identifier(n)) => Some(n)
      case _ => None
    }
  }

  object _lambda {
    def unapply(e: Expr): Option[(Identifier, Expr)] = e match {
      case _typed(Lambda(x,e)) => Some((x,e))
      case _ => None
    }
  }

  object _depLambda {
    def unapply(e: Expr): Option[(Kind#I, Expr)] = e match {
      case _typed(DepLambda(x,e)) => Some((x,e))
      case _ => None
    }
  }

  object _apply {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case _typed(Apply(f, e)) => Some((f, e))
      case _ => None
    }
  }

  object _depApply {
    def unapply(e: Expr): Option[(Expr, Kind#T)] = e match {
      case _typed(DepApply(x,e)) => Some((x,e))
      case _ => None
    }
  }

  object _literal {
    def unapply(e: Expr): Option[semantics.Data] = e match {
      case _typed(Literal(d)) => Some(d)
      case _ => None
    }
  }

  object _primitive {
    def unapply(e: Expr): Boolean = e match {
      case _typed(_:Primitive) => true
      case _ => false
    }
  }

  object _foreignFunction {
    def unapply(e: Expr): Option[(ForeignFunction.Decl, Type)] = e match {
      case _typed(ForeignFunction(d,t)) => Some((d,t))
      case _ => None
    }
  }

  object _join {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`join`) => true
      case _ => false
    }
  }

  object _map {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`map`) => true
      case _ => false
    }
  }

  object _mapSeq {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`mapSeq`) => true
      case _ => false
    }
  }

  object _reduceX {
    def unapply(e: Expr): Option[Expr] = e match {
      case _typed(`reduce`) => Some(`reduce`)
      case _typed(`reduceSeq`) => Some(`reduceSeq`)
      case _ => None
    }
  }

  object _slide {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`slide`) => true
      case _ => false
    }
  }

  object _split {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`split`) => true
      case _ => false
    }
  }

  object _transpose {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`transpose`) => true
      case _ => false
    }
  }

  object _zip {
    def unapply(e: Expr): Boolean = e match {
      case _typed(`zip`) => true
      case _ => false
    }
  }
}
