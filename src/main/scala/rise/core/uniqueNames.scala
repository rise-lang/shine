package rise.core

import rise.core.types._

object uniqueNames {
  private case class CountingVisitor(
    values: Map[Identifier, Int],
    others: Map[Kind.Identifier, Int]
  ) extends traversal.Visitor
  {
    override def visitExpr(e: Expr): traversal.Result[Expr] = e match {
      case Lambda(x, _) =>
        traversal.Continue(e, CountingVisitor(
          values + (x -> (values.getOrElse(x, 0) + 1)),
          others
        ))
      case DepLambda(x: Kind.Identifier, _) =>
        traversal.Continue(e, CountingVisitor(
          values,
          others + (x -> (others.getOrElse(x, 0) + 1))
        ))
      case _ => traversal.Continue(e, this)
    }

    override def visitType[T <: Type](t: T): traversal.Result[T] = {
      case class TypeVisitor(
        others: Map[Kind.Identifier, Int]
      ) extends traversal.Visitor
      {
        override def visitType[U <: Type](t: U): traversal.Result[U] = t match {
          case DepFunType(x: Kind.Identifier, _) =>
            traversal.Continue(t, TypeVisitor(
              others + (x -> (others.getOrElse(x, 0) + 1))
            ))
          case _ => traversal.Continue(t, this)
        }
      }
      traversal.types.DepthFirstGlobalResult(t, TypeVisitor(others))
      traversal.Continue(t, this)
    }
  }

  def check(e: Expr): Boolean = {
    traversal.DepthFirstGlobalResult(e, CountingVisitor(Map(), Map())) match {
      case traversal.Continue(_, CountingVisitor(values, others)) =>
        val valuesDup = values.filter({ case (_, n) => n > 1 })
        val othersDup = others.filter({ case (_, n) => n > 1 })
        valuesDup.isEmpty && othersDup.isEmpty
      case _ => throw new Exception("")
    }
  }
}
