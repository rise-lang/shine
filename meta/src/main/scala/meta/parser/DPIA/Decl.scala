package meta.parser.DPIA

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser.shared.Identifier

object Decl {
  sealed trait AST
  object AST {
    case class Identifier(name: String) extends AST
    case class Param(id: Identifier, ty: KindOrType) extends AST
    case class PrimitiveDeclaration(id: Identifier,
                                    scalaParams: Option[(Int, Int)],
                                    params: Seq[Param],
                                    returnType: Type.AST) extends AST
  }

  sealed trait KindOrType
  object KindOrType {
    final case class Kind(kind: meta.parser.DPIA.Kind.AST) extends KindOrType
    final case class Type(typeAST: meta.parser.DPIA.Type.AST) extends KindOrType
  }

  def PrimitiveDeclarations[$: P]: P[Seq[AST.PrimitiveDeclaration]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  def PrimitiveDeclaration[$: P]: P[AST.PrimitiveDeclaration] = {
    import scalaparse.Scala.TrailingCommaOps
    def ScalaParams: P[(Int, Int)] = {
      P("{" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index ~ "}")
    }

    def Param: P[AST.Param] = (
          (Identifier.map(AST.Identifier) ~ ":" ~ Kind.Kind).map(
            pair => AST.Param(pair._1, KindOrType.Kind(pair._2)))
        | (Identifier.map(AST.Identifier) ~ ":" ~ Type.PhraseType).map(
            pair => AST.Param(pair._1, KindOrType.Type(pair._2)))
      )

    def Params: P[Seq[AST.Param]] = Param.repTC(0)

    P("def" ~ Identifier.map(AST.Identifier) ~ ScalaParams.? ~ "(" ~ Params ~ ")" ~ ":" ~ Type.PhraseType)
      .map(AST.PrimitiveDeclaration.tupled)
  }
}
