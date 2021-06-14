package rise.core

import util.monads._
import rise.core.traverse._
import rise.core.types.Kind.{IAddressSpace, IDataType, INat}
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

    def renameInExpr(e: Expr)(
      values: Map[Identifier, Identifier],
      nats: Map[NatIdentifier, NatIdentifier],
      dts: Map[DataTypeIdentifier, DataTypeIdentifier],
      ass: Map[AddressSpaceIdentifier, AddressSpaceIdentifier]
    ): Pure[Expr] = Renaming(values, nats, dts, ass).expr(e)

    def renameInTypes[T <: Type](t: T)(
      nats: Map[NatIdentifier, NatIdentifier],
      dts: Map[DataTypeIdentifier, DataTypeIdentifier],
      ass: Map[AddressSpaceIdentifier, AddressSpaceIdentifier]
    ): Pure[T] = Renaming(Map(), nats, dts, ass).`type`(t)

    def renameInNat(n: Nat)(types: Map[NatIdentifier, NatIdentifier]): Nat = {
      n.visitAndRebuild({
        case i: NatIdentifier => types.getOrElse(i, i)
        case ae => ae
      })
    }

    case class Renaming(
      values: Map[Identifier, Identifier],
      nats: Map[NatIdentifier, NatIdentifier],
      dts: Map[DataTypeIdentifier, DataTypeIdentifier],
      ass: Map[AddressSpaceIdentifier, AddressSpaceIdentifier]
    ) extends PureTraversal {
      override def nat : Nat => Pure[Nat] = n => return_(renameInNat(n)(nats))
      override def expr : Expr => Pure[Expr] = {
        case x: Identifier => return_(values(x) : Expr)

        case l@Lambda(x, b) => for {
          xt2 <- renameInTypes(x.t)(nats, dts, ass)
          x2 = x.copy(s"x$nextValN")(xt2)
          b2 <- renameInExpr(b)(values + (x -> x2), nats, dts, ass)
          t2 <- renameInTypes(l.t)(nats, dts, ass)
        } yield Lambda(x2, b2)(t2)

        case d@DepLambda(NatKind, x: NatIdentifier, b) =>
          val x2 = NatIdentifier(s"n$nextNatN", x.range)
          for {
            b2 <- renameInExpr(b)(values, nats + (x -> x2), dts, ass)
            t2 <- renameInTypes(d.t)(nats + (x -> x2), dts, ass)
          } yield DepLambda(NatKind, x2, b2)(t2)

        case d@DepLambda(DataKind, x: DataTypeIdentifier, b) =>
          val x2 = DataTypeIdentifier(s"dt$nextDtN")
          for {
            b2 <- renameInExpr(b)(values, nats, dts + (x -> x2), ass)
            t2 <- renameInTypes(d.t)(nats, dts, ass)
          } yield DepLambda(DataKind, x2, b2)(t2)

        case d@DepLambda(AddressSpaceKind, x: AddressSpaceIdentifier, b) =>
          val x2 = AddressSpaceIdentifier(s"a$nextAN")
          for {
            b2 <- renameInExpr(b)(values, nats, dts, ass + (x -> x2))
            t2 <- renameInTypes(d.t)(nats, dts, ass)
          } yield DepLambda(AddressSpaceKind, x2, b2)(t2)

        case e => super.expr(e)
      }

      override def `type`[T <: Type] : T => Pure[T] = {
        case i: DataTypeIdentifier => return_(dts.getOrElse(i, i).asInstanceOf[T])

        case DepFunType(NatKind, x: NatIdentifier, b) =>
          val x2 = nats.getOrElse(x, NatIdentifier(s"n$nextNatN", x.range))
          for { b2 <- renameInTypes(b)(nats + (x -> x2), dts, ass) }
            yield DepFunType(NatKind, x2, b2).asInstanceOf[T]

        case DepFunType(DataKind, x: DataTypeIdentifier, b) =>
          val x2 = dts.getOrElse(x, DataTypeIdentifier(s"dt$nextDtN"))
          for { b2 <- renameInTypes(b)(nats, dts + (x -> x2), ass) }
            yield DepFunType(DataKind, x2, b2).asInstanceOf[T]

        case DepFunType(AddressSpaceKind, x: AddressSpaceIdentifier, b) =>
          val x2 = ass.getOrElse(x, AddressSpaceIdentifier(s"dt$nextAN"))
          for { b2 <- renameInTypes(b)(nats, dts, ass + (x -> x2)) }
            yield DepFunType(AddressSpaceKind, x2, b2).asInstanceOf[T]

        case e => super.`type`(e)
      }
    }

    Renaming(Map(), Map(), Map(), Map()).expr(e).unwrap
  }
}
