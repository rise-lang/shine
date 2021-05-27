package rise.core

import util.monads._
import rise.core.traverse._
import rise.core.types._

object uniqueNames {
  private val collectNames = new PureAccumulatorTraversal[(Seq[Identifier], Seq[Kind.Identifier])] {
    override val accumulator: Monoid[(Seq[Identifier], Seq[Kind.Identifier])] = PairMonoid(SeqMonoid, SeqMonoid)
    override def identifier[I <: Identifier]: VarType => I => Pair[I] = {
      case Binding => i => accumulate((Seq(i), Seq()))(i)
      case _ => return_
    }
    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = {
      case Binding => i => accumulate((Seq(), Seq(i)))(i)
      case _ => return_
    }
  }

  def check(e: Expr): Boolean = {
    val ((vs, ts), _) = traverse(e, collectNames)
    vs == vs.distinct && ts == ts.distinct
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
                              types: Map[Kind.Identifier, Kind.Identifier]): Pure[Expr] =
      Renaming(values, types).expr(e)

    def renameInTypes[T <: Type](t: T)(types: Map[Kind.Identifier, Kind.Identifier]): Pure[T] =
      Renaming(Map(), types).`type`(t)

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
      override def expr : Expr => Pure[Expr] = {
        case x: Identifier => return_(values(x) : Expr)

        case l@Lambda(x, b) => for {
          xt2 <- renameInTypes(x.t)(types)
          x2 = x.copy(s"x$nextValN")(xt2)
          b2 <- renameInExpr(b)(values + (x -> x2), types)
          t2 <- renameInTypes(l.t)(types)
        } yield Lambda(x2, b2)(t2)

        case d@DepLambda(x: NatIdentifier, b) =>
          val x2 = NatIdentifier(s"n$nextNatN", x.range)
          for {
            b2 <- renameInExpr(b)(values, types + (x -> x2))
            t2 <- renameInTypes(d.t)(types + (x -> x2))
          } yield DepLambda[NatKind](x2, b2)(t2)

        case d@DepLambda(x: DataTypeIdentifier, b) =>
          val x2 = DataTypeIdentifier(s"dt$nextDtN")
          for {
            b2 <- renameInExpr(b)(values, types + (x -> x2))
            t2 <- renameInTypes(d.t)(types)
          } yield DepLambda[DataKind](x2, b2)(t2)

        case d@DepLambda(x: AddressSpaceIdentifier, b) =>
          val x2 = AddressSpaceIdentifier(s"a$nextAN")
          for {
            b2 <- renameInExpr(b)(values, types + (x -> x2))
            t2 <- renameInTypes(d.t)(types)
          } yield DepLambda[AddressSpaceKind](x2, b2)(t2)

        case e => super.expr(e)
      }

      override def `type`[T <: Type] : T => Pure[T] = {
        case i: DataTypeIdentifier =>
          return_(types.getOrElse(i, i).asInstanceOf[T])

        case DepFunType(x: NatIdentifier, b) =>
          val x2 = types.getOrElse(x, NatIdentifier(s"n$nextNatN", x.range)).asInstanceOf[NatIdentifier]
          for { b2 <- renameInTypes(b)(types + (x -> x2)) }
            yield DepFunType[NatKind, Type](x2, b2).asInstanceOf[T]

        case DepFunType(x: DataTypeIdentifier, b) =>
          val x2 = types.getOrElse(x, DataTypeIdentifier(s"dt$nextDtN")).asInstanceOf[DataTypeIdentifier]
          for { b2 <- renameInTypes(b)(types + (x -> x2)) }
            yield DepFunType[DataKind, Type](x2, b2).asInstanceOf[T]

        case DepFunType(x: AddressSpaceIdentifier, b) =>
          val x2 = types.getOrElse(x, AddressSpaceIdentifier(s"dt$nextAN")).asInstanceOf[AddressSpaceIdentifier]
          for { b2 <- renameInTypes(b)(types + (x -> x2)) }
            yield DepFunType[AddressSpaceKind, Type](x2, b2).asInstanceOf[T]

        case e => super.`type`(e)
      }
    }

    Renaming(Map(), Map()).expr(e).unwrap
  }
}
