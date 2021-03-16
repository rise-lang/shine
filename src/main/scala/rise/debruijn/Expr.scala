package rise.debruijn

import rise.core.semantics
import rise.core.types._

// TODO? same data type as `ENode` in egg
sealed trait Expr
case class Var(index: Int) extends Expr
case class App(f: Expr, e: Expr) extends Expr
case class Lambda(e: Expr) extends Expr
case class DepApp[K <: Kind](f: Expr, x: K#T) extends Expr
case class DepLambda(k: Kind, e: Expr) extends Expr
case class Literal(d: semantics.Data) extends Expr
case class Primitive(p: rise.core.Primitive) extends Expr
