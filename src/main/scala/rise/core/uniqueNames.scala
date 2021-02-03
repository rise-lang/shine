package rise.core

import rise.core.Traverse.{Pure, PureTraversal}
import rise.core.types._

object uniqueNames {
  private case class CountingVisitor(
    var values: Map[Identifier, Int],
    var types: Map[Kind.Identifier, Int]
  ) extends PureTraversal {
    override def binding[I <: Identifier]: I => Pure[I] = i => {
      values = values + (i -> (values.getOrElse(i, 0) + 1))
      return_(i)
    }
    override def depBinding[I <: Kind.Identifier]: I => Pure[I] = i => {
      types = types + (i -> (types.getOrElse(i, 0) + 1))
      return_(i)
    }
  }

  def check(e: Expr): Boolean = {
    val cv = CountingVisitor(Map(), Map())
    Traverse(e, cv)
    val valuesDup = cv.values.filter({ case (_, n) => n > 1 })
    val typesDup = cv.types.filter({ case (_, n) => n > 1 })
    valuesDup.isEmpty && typesDup.isEmpty
  }

  def enforce(e: Expr): Expr = {
    var valN = -1
    def nextValN: Int = {
      valN += 1
      valN
    }

    var natN = -1
    def nextNatN: Int = {
      natN += 1
      natN
    }

    var dtN = -1
    def nextDtN: Int = {
      dtN += 1
      dtN
    }

    var aN = -1
    def nextAN: Int = {
      aN += 1
      aN
    }

    def renameInExpr(e: Expr)(values: Map[Identifier, Identifier],
                              types: Map[Kind.Identifier, Kind.Identifier]): Expr =
      Renaming(values, types).expr(e).unwrap

    def renameInTypes[T <: Type](t: T)(types: Map[Kind.Identifier, Kind.Identifier]): T =
      Renaming(Map(), types).etype(t).unwrap

    def renameInNat(n: Nat)(types: Map[Kind.Identifier, Kind.Identifier]): Nat = {
      n.visitAndRebuild({
        case i: NatIdentifier =>
          types.get(i)
            .map(_.asInstanceOf[NatIdentifier])
            .getOrElse(i)
        case ae => ae
      })
    }

    case class Renaming(
      values: Map[Identifier, Identifier],
      types: Map[Kind.Identifier, Kind.Identifier]
    ) extends PureTraversal {
      override def nat : Nat => Pure[Nat] = n => return_(renameInNat(n)(types))
      override def expr : Expr => Pure[Expr] = e => (e match {
        case x: Identifier => return_(values(x))

        case l@Lambda(x, b) =>
          val x2 = x.copy(s"x$nextValN")(renameInTypes(x.t)(types))
          val b2 = renameInExpr(b)(values + (x -> x2), types)
          val t2 = renameInTypes(l.t)(types)
          return_(Lambda(x2, b2)(t2))

        case d@DepLambda(x: NatIdentifier, b) =>
          val x2 = NatIdentifier(s"n$nextNatN", x.range, x.isExplicit)
          val b2 = renameInExpr(b)(values, types + (x -> x2))
          val t2 = renameInTypes(d.t)(types + (x -> x2))
          return_(DepLambda[NatKind](x2, b2)(t2))

        case d@DepLambda(x: DataTypeIdentifier, b) =>
          val x2 = DataTypeIdentifier(s"dt$nextDtN", x.isExplicit)
          val b2 = renameInExpr(b)(values, types + (x -> x2))
          val t2 = renameInTypes(d.t)(types)
          return_(DepLambda[DataKind](x2, b2)(t2))

        case d@DepLambda(x: AddressSpaceIdentifier, b) =>
          val x2 = AddressSpaceIdentifier(s"a$nextAN", x.isExplicit)
          val b2 = renameInExpr(b)(values, types + (x -> x2))
          val t2 = renameInTypes(d.t)(types)
          return_(DepLambda[AddressSpaceKind](x2, b2)(t2))

        case e => super.expr(e)
      }).asInstanceOf[Pure[Expr]]

      override def etype[T <: Type] : T => Pure[T] = t => (t match {
        case i: DataTypeIdentifier =>
          return_(types.get(i).map(_.asInstanceOf[DataTypeIdentifier]).getOrElse(i))

        case DepFunType(x: NatIdentifier, b) =>
          val x2 = types.getOrElse(x,
            NatIdentifier(s"n$nextNatN", x.range, x.isExplicit)).asInstanceOf[NatIdentifier]
          val b2 = renameInTypes(b)(types + (x -> x2))
          return_(DepFunType[NatKind, Type](x2, b2))

        case DepFunType(x: DataTypeIdentifier, b) =>
          val x2 = types.getOrElse(x,
            DataTypeIdentifier(s"dt$nextDtN", x.isExplicit)).asInstanceOf[DataTypeIdentifier]
          val b2 = renameInTypes(b)(types + (x -> x2))
          return_(DepFunType[DataKind, Type](x2, b2))

        case DepFunType(x: AddressSpaceIdentifier, b) =>
          val x2 = types.getOrElse(x,
            AddressSpaceIdentifier(s"dt$nextAN", x.isExplicit)).asInstanceOf[AddressSpaceIdentifier]
          val b2 = renameInTypes(b)(types + (x -> x2))
          return_(DepFunType[AddressSpaceKind, Type](x2, b2))

        case e => super.etype(e)
      }).asInstanceOf[Pure[T]]
    }

    Renaming(Map(), Map()).expr(e).unwrap
  }
}
