package meta

import fastparse.ScalaWhitespace._
import fastparse._

object RiseDpiaShared {

  object TypeAST {
    object Kind extends Enumeration {
      val Data, Nat, Nat2Nat, Nat2Data, Address, Fragment, MatrixLayout, Function = Value

      def fromString(s: String): Value = s match {
        case "data" => Data
        case "nat" => Nat
        case "nat2nat" => Nat2Nat
        case "nat2data" => Nat2Data
        case "address" => Address
        case "fragment" => Fragment
        case "matrixLayout" => MatrixLayout
      }
    }
  }

  def Identifier[_: P]: P[String] = {
    def Keywords: P[Unit] =
      P(("def" | (Kind: P[Unit]) | RiseTypeParser.DataType.TypeName) ~~ CharPred(_.isWhitespace))

    val LowerChar = scalaparse.syntax.Identifiers.NamedFunction(CharPredicates.isLower)
    val IdCharacter = scalaparse.syntax.Identifiers.NamedFunction(c =>
      CharPredicates.isLetter(c) || CharPredicates.isDigit(c))

    P((!Keywords ~ CharPred(LowerChar).! ~~ CharsWhile(IdCharacter).!.?).
      map(t => t._1 ++ t._2.getOrElse("")))
  }

  def Kind[_: P]: P[String] =
    P("data".! | "address".! | "nat2nat".! | "nat2data".! | "nat".! |
      "fragment".! | "matrixLayout".!)

  def IdentifierKindPair[_: P]: P[(String, String)] = P(Identifier ~ ":" ~ Kind)

}
