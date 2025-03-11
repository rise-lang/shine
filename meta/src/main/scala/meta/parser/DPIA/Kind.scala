package meta.parser.DPIA

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser.rise
import meta.parser.shared.Identifier

object Kind {
  sealed trait AST
  object AST {
    case class RiseKind(riseKind: rise.Kind.AST) extends AST
    case object Access extends AST
    case class VariadicKind(n: String, kind: AST) extends AST
  }

  def Kind[$: P]: P[AST] = {
    def OnlyKind: P[AST] = P(rise.Kind.Kind.map(AST.RiseKind) | "access".!.map(_ => AST.Access))

    OnlyKind | (Identifier ~ "*" ~ OnlyKind).map(AST.VariadicKind.tupled)
  }
}
