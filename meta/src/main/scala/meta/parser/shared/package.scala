package meta.parser

import fastparse.ScalaWhitespace._
import fastparse._

package object shared {
  def Identifier[$: P]: P[String] = {
    def Keywords: P[Unit] =
      P(( "def" |
          (rise.Kind.Kind: P[Unit]) | rise.Type.DataType.TypeName |
          (DPIA.Kind.Kind: P[Unit])
        ) ~~ CharPred(_.isWhitespace))

    val LowerChar = scalaparse.syntax.Identifiers.NamedFunction(CharPredicates.isLower)
    val IdCharacter = scalaparse.syntax.Identifiers.NamedFunction(c =>
      CharPredicates.isLetter(c) || CharPredicates.isDigit(c))

    P((!Keywords ~ CharPred(LowerChar).! ~~ CharsWhile(IdCharacter).!.?).
      map(t => t._1 ++ t._2.getOrElse("")))
  }
}
