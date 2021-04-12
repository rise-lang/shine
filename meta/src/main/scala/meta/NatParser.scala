package meta

import fastparse.ScalaWhitespace._
import fastparse._

import meta.RiseTypeParser._

object NatParser {

  sealed trait NatAST
  object NatAST {
    case class Identifier(name: String) extends NatAST
    case class Number(n: String) extends NatAST
    case class BinaryOp(lhs: NatAST, op: String, rhs: NatAST) extends NatAST
    case class TernaryOp(cond: BinaryOp, thenN: NatAST, elseN: NatAST) extends NatAST
    case class Nat2NatApply(f: RISETypeAST.Identifier, n: NatAST) extends NatAST
    case class Sum(id: Identifier, from: NatAST, upTo: NatAST, body: NatAST) extends NatAST
  }

  def Nat[_: P]: P[NatAST] = {
    def CompOrNat: P[NatAST] = {
      def CompOp: P[String] = P("<".! | ">".!)
      P(AddSubOrNat ~ (CompOp ~/ AddSubOrNat).rep).map(asBinaryOpOrNat)
    }

    def AddSubOrNat: P[NatAST] = {
      def AddSubOps: P[String] = P("+".! | "-".!)
      P(DivMulPowModOrNat ~ (AddSubOps ~ DivMulPowModOrNat).rep).map(asBinaryOpOrNat)
    }

    def DivMulPowModOrNat: P[NatAST] = {
      def DivMulPowModOp: P[String] = P("*".! | "/".! | "^".! | "%".!)
      P(SingleNat ~ (DivMulPowModOp ~ SingleNat).rep).map(asBinaryOpOrNat)
    }

    def SingleNat: P[NatAST] = {
      def Number: P[NatAST.Number] = P(CharIn("0-9").rep(1).!).map(NatAST.Number)

      def Sum: P[NatAST.Sum] = {
        def Assignment: P[(NatAST.Identifier, NatAST)] =
          P(NatIdentifier ~ "=" ~ Nat | "(" ~ Assignment ~ ")")
        P("sum" ~ "_" ~ Assignment ~ "^" ~ Nat ~ Nat).map(NatAST.Sum.tupled)
      }

      def Nat2NatApply: P[NatAST.Nat2NatApply] =
        P(TypeIdentifier ~ "(" ~ Nat ~ ")").map(NatAST.Nat2NatApply.tupled)

      def NatIdentifier: P[NatAST.Identifier] = P(TypeIdentifier).map(i => NatAST.Identifier(i.name))

      def Parens: P[NatAST] = P("(" ~ Nat ~ ")")

      P(Number | Sum | Nat2NatApply | NatIdentifier | Parens)
    }

    P(CompOrNat)
  }

  private def asBinaryOpOrNat: ((NatAST, Seq[(String, NatAST)])) => NatAST = {
    case (n, ns) => ns.foldLeft(n){
      case (lhs, (op, rhs)) => NatAST.BinaryOp(lhs, op, rhs)
    }
  }

}
