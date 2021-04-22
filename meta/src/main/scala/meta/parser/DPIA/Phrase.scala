package meta.parser.DPIA

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser.shared.{Identifier, Kind}

object Phrase extends App {
  sealed trait AST
  object AST {
    case class Identifier(name: String) extends AST
    case class Param(id: Identifier, ty: Either[Kind.AST, Type.AST]) extends AST
    case class PrimitiveDeclaration(id: Identifier,
                                    scalaParams: Option[(Int, Int)],
                                    params: Seq[Param],
                                    returnType: Type.AST) extends AST
  }

  def PrimitiveDeclarations[_: P]: P[Seq[AST.PrimitiveDeclaration]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  // def drop(n: nat, m: nat, t: data, input: exp[n+m.t, read]): exp[m.t, read]
  // def mapGlobal[dim: Int](n: nat, s: data, t: data, f: exp[s, read] -> exp[t, read], array: exp[n.s, read]): exp[n.t, read]
  def PrimitiveDeclaration[_: P]: P[AST.PrimitiveDeclaration] = {
    import scalaparse.Scala.TrailingCommaOps
    def ScalaParams: P[(Int, Int)] = {
      P("[" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index ~ "]")
    }

    def Param: P[AST.Param] = (
          (Identifier.map(AST.Identifier) ~ ":" ~ Kind.Kind).map(pair => AST.Param(pair._1, Left(pair._2)))
        | (Identifier.map(AST.Identifier) ~ ":" ~ Type.PhraseType).map(pair => AST.Param(pair._1, Right(pair._2)))
      )

    def Params: P[Seq[AST.Param]] = Param.repTC(1)

    P("def" ~ Identifier.map(AST.Identifier) ~ ScalaParams.? ~ "(" ~ Params ~ ")" ~ ":" ~ Type.PhraseType)
      .map(AST.PrimitiveDeclaration.tupled)
  }
}
