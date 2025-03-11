package meta.parser.rise

import fastparse._

object Kind {
  sealed trait AST
  object AST {
    case object Data extends AST
    case object Address extends AST
    case object Nat2Nat extends AST
    case object Nat2Data extends AST
    case object Nat extends AST
    case object Fragment extends AST
    case object MatrixLayout extends AST
  }

  def Kind[$: P]: P[AST] = P(
    "data".!.map(_ => AST.Data) |
      "address".!.map(_ => AST.Address) |
      "nat2nat".!.map(_ => AST.Nat2Nat) |
      "nat2data".!.map(_ => AST.Nat2Data) |
      "nat".!.map(_ => AST.Nat) |
      "fragment".!.map(_ => AST.Fragment) |
      "matrixLayout".!.map(_ => AST.MatrixLayout)
    )
}
