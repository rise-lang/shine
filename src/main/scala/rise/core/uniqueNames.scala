package rise.core

import rise.core.DSL._
import rise.core.types._

object uniqueNames {
  private case class CountingVisitor(
    values: Map[Identifier, Int],
    others: Map[Kind.Identifier, Int]
  ) extends traversal.Visitor {
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
      ) extends traversal.Visitor {
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

  def enforce(e: Expr): Expr = {
    var valN = 0
    var natN = 0
    var dtN = 0

    case class RenamingVisitor(
      values: Map[Identifier, Identifier],
      others: Map[Kind.Identifier, Kind.Identifier]
    ) extends traversal.Visitor {
      override def visitExpr(e: Expr): traversal.Result[Expr] = e match {
        case x: Identifier =>
          traversal.Continue(values(x), this)
        case Lambda(x, b) =>
          val x2 = Identifier(s"x$valN")(traversal.types.DepthFirstLocalResult(
            x.t, TypeVisitor(others)
          ))
          valN += 1
          traversal.Stop(lambda(x2,
            traversal.DepthFirstLocalResult(b, RenamingVisitor(
              values + (x -> x2),
              others
            ))
          ))
        case DepLambda(x: NatIdentifier, b) =>
          val x2 = NatIdentifier(s"n$natN", x.range, isExplicit = true)
          natN += 1
          traversal.Stop(depLambda[NatKind](x2,
            traversal.DepthFirstLocalResult(b, RenamingVisitor(
              values,
              others + (x -> x2)
            ))
          ))
        case DepLambda(x: DataTypeIdentifier, b) =>
          val x2 = DataTypeIdentifier(s"dt$dtN", isExplicit = true)
          dtN += 1
          traversal.Stop(depLambda[DataKind](x2,
            traversal.DepthFirstLocalResult(b, RenamingVisitor(
              values,
              others + (x -> x2)
            ))
          ))
        case rise.core.primitives.Annotation(a, t) =>
          traversal.Stop(rise.core.primitives.Annotation(
            traversal.DepthFirstLocalResult(a, this),
            traversal.types.DepthFirstLocalResult(t, TypeVisitor(others))
          ))
        case _ => traversal.Continue(e, this)
      }

      override def visitType[T <: Type](t: T): traversal.Result[T] =
        traversal.Stop(
          traversal.types.DepthFirstLocalResult(t, TypeVisitor(others)))

      override def visitNat(ae: Nat): traversal.Result[Nat] =
        traversal.Stop(natVisit(ae, others))
    }

    case class TypeVisitor(
      others: Map[Kind.Identifier, Kind.Identifier]
    ) extends traversal.Visitor {
      override def visitType[U <: Type](t: U): traversal.Result[U] =
        t match {
          case i: DataTypeIdentifier =>
            traversal.Stop(others.get(i)
              .map(_.asInstanceOf[DataTypeIdentifier])
              .getOrElse(i).asInstanceOf[U])
          case DepFunType(x: NatIdentifier, b) =>
            val x2 = NatIdentifier(s"n$natN", x.range, isExplicit = true)
            natN += 1
            traversal.Stop(DepFunType[NatKind, Type](x2,
              traversal.types.DepthFirstLocalResult(b, TypeVisitor(
                others + (x -> x2)
              ))
            ).asInstanceOf[U])
          case DepFunType(x: DataTypeIdentifier, b) =>
            val x2 = DataTypeIdentifier(s"dt$dtN", isExplicit = true)
            dtN += 1
            traversal.Stop(DepFunType[DataKind, Type](x2,
              traversal.types.DepthFirstLocalResult(b, TypeVisitor(
                others + (x -> x2)
              ))
            ).asInstanceOf[U])
          case _ => traversal.Continue(t, this)
        }

      override def visitNat(ae: Nat): traversal.Result[Nat] =
        traversal.Stop(natVisit(ae, others))
    }

    def natVisit(n: Nat, others: Map[Kind.Identifier, Kind.Identifier]): Nat = {
      n.visitAndRebuild({
        case i: NatIdentifier =>
          others.get(i)
            .map(_.asInstanceOf[NatIdentifier])
            .getOrElse(i)
        case ae => ae
      })
    }

    val r = traversal.DepthFirstLocalResult(e, RenamingVisitor(Map(), Map()))
    r
  }
}
