package meta

import fastparse.ScalaWhitespace._
import fastparse._

import meta.TypeParser._

object NatParser {

  sealed trait NatAST
  object NatAST {
    case class Identifier(id: TypeAST.Identifier) extends NatAST
    case class Number(n: String) extends NatAST
    case class BinaryOp(lhs: NatAST, op: String, rhs: NatAST) extends NatAST
    case class TernaryOp(cond: BinaryOp, thenN: NatAST, elseN: NatAST) extends NatAST
    case class Nat2NatApply(f: TypeAST.Identifier, n: NatAST) extends NatAST
    case class Sum(id: Identifier, from: NatAST, upTo: NatAST, body: NatAST) extends NatAST
  }

  def Nat[_: P]: P[NatAST] = P( CompOrNat )

  def CompOrNat[_: P]: P[NatAST] = P( AddSubOrNat ~ (CompOp ~/ AddSubOrNat).rep ).map(asBinaryOpOrNat)
  def CompOp[_: P]: P[String] = P( "<".! | ">".! )

  def AddSubOrNat[_: P]: P[NatAST] = P( DivMulPowModOrNat ~ (AddSubOps ~ DivMulPowModOrNat).rep ).map(asBinaryOpOrNat)
  def AddSubOps[_: P]: P[String] = P( "+".! | "-".! )

  def DivMulPowModOrNat[_: P]: P[NatAST] = P( SingleNat ~ (DivMulPowModOp ~ SingleNat).rep  ).map(asBinaryOpOrNat)
  def DivMulPowModOp[_: P]: P[String] = P( "*".! | "/".! | "^".! | "%".! )

  def SingleNat[_: P]: P[NatAST] = P( Number | Sum | Nat2NatApply | NatIdentifier | Parens )

  def Number[_: P]: P[NatAST.Number] = P( CharIn("0-9").rep(1).! ).map(NatAST.Number)

  def Sum[_: P]: P[NatAST.Sum] =
    P("sum" ~ "_" ~ Assignment ~ "^" ~ Nat ~ Nat).map(NatAST.Sum.tupled)
  def Assignment[_: P]: P[(NatAST.Identifier, NatAST)] = P( NatIdentifier ~ "=" ~ Nat | "(" ~ Assignment ~ ")" )

  def Nat2NatApply[_: P]: P[NatAST.Nat2NatApply] =
    P( TypeIdentifier ~ "(" ~ Nat ~ ")" ).map(NatAST.Nat2NatApply.tupled)

  def NatIdentifier[_: P]: P[NatAST.Identifier] = P( TypeIdentifier ).map(NatAST.Identifier)

  def Parens[_: P]: P[NatAST] = P( "(" ~ Nat ~ ")" )

  private def asBinaryOpOrNat: ((NatAST, Seq[(String, NatAST)])) => NatAST = {
    case (n, ns) => ns.foldLeft(n){
      case (lhs, (op, rhs)) => NatAST.BinaryOp(lhs, op, rhs)
    }
  }

}
