package meta.parser.shared

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
    case object Function extends AST
  }

  def fromString(s: String): AST = s match {
    case "data" => AST.Data
    case "nat" => AST.Nat
    case "nat2nat" => AST.Nat2Nat
    case "nat2data" => AST.Nat2Data
    case "address" => AST.Address
    case "fragment" => AST.Fragment
    case "matrixLayout" => AST.MatrixLayout
  }

  def Kind[_: P]: P[AST] =
    P("data".! | "address".! | "nat2nat".! | "nat2data".! | "nat".! | "fragment".! | "matrixLayout".!).map(fromString)
}
