package meta.parser.rise

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser.shared.Identifier

object Decl {
  sealed trait AST
  object AST {
    case class Identifier(name: String) extends AST
    case class PrimitiveDeclaration(id: Identifier,
                                    scalaParams: Option[(Int, Int)],
                                    typeSignature: Type.AST) extends AST
  }

  def PrimitiveDeclarations[$: P]: P[Seq[AST.PrimitiveDeclaration]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  def PrimitiveDeclaration[$: P]: P[AST.PrimitiveDeclaration] = {
    def ScalaParams: P[(Int, Int)] = {
      import scalaparse.Scala.TrailingCommaOps
      P("(" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index ~ ")")
    }

    P("def" ~ Identifier.map(AST.Identifier) ~ ScalaParams.? ~ ":" ~ Type.TypeSignature)
      .map(AST.PrimitiveDeclaration.tupled)
  }
}
