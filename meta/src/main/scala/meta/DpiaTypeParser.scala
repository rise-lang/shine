package meta

import fastparse.ScalaWhitespace._
import fastparse._
import meta.NatParser._
import meta.RiseDpiaShared.Identifier
import meta.RiseTypeParser.RISETypeAST

object DpiaTypeParser {

  sealed trait DPIATypeAST
  object DPIATypeAST {
    case class ExpType(dataType: RISETypeAST, access: AccessAST) extends DPIATypeAST
    case class AccType(dataType: RISETypeAST) extends DPIATypeAST
    case object CommType extends DPIATypeAST

    case class PairType(lhs: DPIATypeAST, rhs: DPIATypeAST) extends DPIATypeAST
    case class FunType(inT: DPIATypeAST, outT: DPIATypeAST) extends DPIATypeAST
    case class DepFunType(id: String, kind: String, t: DPIATypeAST) extends DPIATypeAST

    sealed trait AccessAST
    object AccessAST {
      case class Identifier(name: String) extends AccessAST
      case object Read extends AccessAST
      case object Write extends AccessAST
    }
  }

  def PrimitiveDeclarations[_: P]: P[Seq[(String, Option[(Int, Int)], DPIATypeAST)]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  def PrimitiveDeclaration[_: P] = {
    import scalaparse.Scala.TrailingCommaOps
    def ScalaFunArgs: P[(Int, Int)] = {
      P("(" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index  ~ ")")
    }

    P("def" ~ Identifier ~ "(" ~ (Identifier ~ ":" ~ Type).repTC(0) ~ ")" ~ ":" ~ Type)
  }

  def Type[_: P]: P[DPIATypeAST] = ???

}
