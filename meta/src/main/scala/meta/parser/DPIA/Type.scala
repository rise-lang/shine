package meta.parser.DPIA

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser._
import shared._

object Type {
  sealed trait AST
  object AST {
    case class ExpType(dataType: rise.Type.AST, access: Access.AST) extends AST
    case class AccType(dataType: rise.Type.AST) extends AST
    case object CommType extends AST

    case class PairType(lhs: AST, rhs: AST) extends AST
    case class FunType(inT: AST, outT: AST) extends AST
    case class DepFunType(id: Identifier, kind: Kind.AST, t: AST) extends AST
    case class Identifier(name: String) extends AST

    case class VariadicType(n: Identifier, ty: AST) extends AST
  }

  object Access {
    sealed trait AST
    object AST {
      case class Identifier(name: String) extends AST
      case object Read extends AST
      case object Write extends AST
    }
  }

  def PhraseType[$: P]: P[AST] = {
    def DataType: P[rise.Type.AST] = rise.Type.DataType.DataType

    def AccessType: P[Access.AST] = P(
      "read".!.map(_ => Access.AST.Read) |
      "write".!.map(_ => Access.AST.Write) |
      Identifier.map(Access.AST.Identifier)
    )

    def ExpType: P[AST.ExpType] = P("exp[" ~ DataType ~ "," ~ AccessType ~ "]").map(AST.ExpType.tupled)

    def AccType: P[AST.AccType] = P("acc[" ~ DataType ~ "]").map(AST.AccType)

    def VarType: P[AST.PairType] = P("var[" ~ DataType ~ "]").
      map(dt => AST.PairType(AST.ExpType(dt, Access.AST.Read), AST.AccType(dt)))

    def CommType: P[AST.CommType.type] = P("comm".!.map(_ => AST.CommType))

    def PairType: P[AST.PairType] =
      P("(" ~ NoCut(PhraseType) ~ "," ~/ PhraseType ~ ")").map(AST.PairType.tupled)

    def FunType: P[AST.FunType] =
      P(NoCut(NonFunPhraseType | ("(" ~ PhraseType ~ ")")) ~ "->" ~/ PhraseType).map(AST.FunType.tupled)

    def DepFunType: P[AST.DepFunType] = {
      def IdentifierKindPair: P[(AST.Identifier, Kind.AST)] =
        P(Identifier.map(AST.Identifier) ~ ":" ~ Kind.Kind)

      P("(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ PhraseType).map(AST.DepFunType.tupled)
    }

    def NonFunPhraseType: P[AST] = P( ExpType | AccType | VarType | CommType | PairType | DepFunType )

    def OnlyPhraseType: P[AST] = P( FunType | NonFunPhraseType )

    def VariadicType: P[AST] = P(Identifier.map(AST.Identifier) ~ "*" ~ OnlyPhraseType).map(AST.VariadicType.tupled)

    OnlyPhraseType | VariadicType
  }
}
