package meta.parser

import fastparse.ScalaWhitespace._
import fastparse._

package object shared {
  def Identifier[_: P]: P[String] = {
    def Keywords: P[Unit] =
      P(("def" |
        (Kind.Kind: P[Unit]) |
        meta.parser.rise.Type.DataType.TypeName) ~~ CharPred(_.isWhitespace))

    val LowerChar = scalaparse.syntax.Identifiers.NamedFunction(CharPredicates.isLower)
    val IdCharacter = scalaparse.syntax.Identifiers.NamedFunction(c =>
      CharPredicates.isLetter(c) || CharPredicates.isDigit(c))

    P((!Keywords ~ CharPred(LowerChar).! ~~ CharsWhile(IdCharacter).!.?).
      map(t => t._1 ++ t._2.getOrElse("")))
  }

  def IdentifierKindPair[_: P]: P[(String, Kind.AST)] = P(Identifier ~ ":" ~ Kind.Kind)
}
