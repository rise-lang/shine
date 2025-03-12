package meta.parser

import fastparse.ScalaWhitespace._
import fastparse._

object Nat {

  sealed trait AST
  object AST {
    case class Identifier(name: String) extends AST
    case class Number(n: String) extends AST
    case class BinaryOp(lhs: AST, op: String, rhs: AST) extends AST
    case class TernaryOp(cond: BinaryOp, thenN: AST, elseN: AST) extends AST
    case class Nat2NatApply(f: rise.Type.AST.Identifier, n: AST) extends AST
    case class Sum(id: Identifier, from: AST, upTo: AST, body: AST) extends AST
  }

  def Nat[$: P]: P[AST] = {
    def CompOrNat: P[AST] = {
      def CompOp: P[String] = P("<".! | ">".!)
      P(AddSubOrNat ~ (CompOp ~/ AddSubOrNat).rep).map(asBinaryOpOrNat)
    }

    def AddSubOrNat: P[AST] = {
      def AddSubOps: P[String] = P("+".! | "-".!)
      P(DivMulPowModOrNat ~ (AddSubOps ~ DivMulPowModOrNat).rep).map(asBinaryOpOrNat)
    }

    def DivMulPowModOrNat: P[AST] = {
      def DivMulPowModOp: P[String] = P("*".! | "/".! | "^".! | "%".!)
      P(SingleNat ~ (DivMulPowModOp ~ SingleNat).rep).map(asBinaryOpOrNat)
    }

    def SingleNat: P[AST] = {
      def Number: P[AST.Number] = P(CharIn("0-9").rep(1).!).map(AST.Number)

      def Sum: P[AST.Sum] = {
        def Assignment: P[(AST.Identifier, AST)] =
          P(NatIdentifier ~ "=" ~ Nat | "(" ~ Assignment ~ ")")
        P("sum" ~ "_" ~ Assignment ~ "^" ~ Nat ~ Nat).map(AST.Sum.tupled)
      }

      def Nat2NatApply: P[AST.Nat2NatApply] =
        P(rise.Type.TypeIdentifier ~ "(" ~ Nat ~ ")").map(AST.Nat2NatApply.tupled)

      def NatIdentifier: P[AST.Identifier] = P(rise.Type.TypeIdentifier).map(i => AST.Identifier(i.name))

      def Parens: P[AST] = P("(" ~ Nat ~ ")")

      P(Number | Sum | Nat2NatApply | NatIdentifier | Parens)
    }

    P(CompOrNat)
  }

  private def asBinaryOpOrNat: ((AST, Seq[(String, AST)])) => AST = {
    case (n, ns) => ns.foldLeft(n){
      case (lhs, (op, rhs)) => AST.BinaryOp(lhs, op, rhs)
    }
  }

}
