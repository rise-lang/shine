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

  def Nat[_: P]: P[NatAST] = P( Number | Sum | Nat2NatApply | NatIdentifier | ("(" ~ RightNat ~ ")") )

  def Number[_: P]: P[NatAST.Number] = P( CharIn("0-9").rep(1).! ).map(NatAST.Number)

  def Sum[_: P]: P[NatAST.Sum] =
    P( "sum" ~ "_" ~ ( "(" ~ Assignment ~ ")" | Assignment) ~ "^" ~ RightNat ~ RightNat ).map(NatAST.Sum.tupled)

  def Assignment[_: P]: P[(NatAST.Identifier, NatAST)] = P( NatIdentifier ~ "=" ~ RightNat )

  def Nat2NatApply[_: P]: P[NatAST.Nat2NatApply] =
    P( TypeIdentifier ~ "(" ~ RightNat ~ ")" ).map(NatAST.Nat2NatApply.tupled)

  def NatIdentifier[_: P]: P[NatAST.Identifier] = P( TypeIdentifier ).map(NatAST.Identifier)

  def RightNat[_: P]: P[NatAST] = P( NatBinaryOp | Nat )

  def NatBinaryOp[_: P]: P[NatAST.BinaryOp] = P( Nat ~ BinOperator ~ RightNat ).map(NatAST.BinaryOp.tupled)
  def BinOperator[_:P]: P[String] = P( "+".! | "-".! | "*".! | "/".! | "^".! | "%".! | "<".! | ">".! )

//  def NatTernaryOp[_: P]: P[NatAST.TernaryOp] =
//    P( NatBinaryOp ~ "?" ~ LeftNat ~ ":" ~ Nat ).map(NatAST.TernaryOp.tupled)

}
