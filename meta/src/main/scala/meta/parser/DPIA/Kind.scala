package meta.parser.DPIA

import fastparse._
import meta.parser.rise

object Kind {
  sealed trait AST
  object AST {
    case class RiseKind(riseKind: rise.Kind.AST) extends AST
    case object Access extends AST
  }

  def Kind[_: P]: P[AST] = P(
    rise.Kind.Kind.map(AST.RiseKind) |
      "access".!.map(_ => AST.Access)
  )
}
