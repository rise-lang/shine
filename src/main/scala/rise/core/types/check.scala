package rise.core.types

import rise.core.DSL.Type._
import rise.core._
import scala.util.Try

// Checks if a given Expr is well typed
object check {
  def apply(expr: Expr): Try[Type] = Try(TypingContext.empty() `|-` expr)

  case class TypingContext(private val map: Predef.Map[String, Type]) {
    def apply(x: String): Type = map.apply(x)

    def updated(key: String, value: Type): TypingContext = this.copy(map.updated(key, value))

    def `|-`(e: Expr): Type = check(this)(e)
  }

  object TypingContext {
    def empty(): TypingContext = TypingContext(Predef.Map[String, Type]())
  }

  private def check(ctx: TypingContext)(expr: Expr): Type = expr match {
    case Identifier(name) =>
      // ----------- Var
      val t = ctx(name)
      expr `:` t

    case Lambda(x, e) =>
      val t1 = x.t
      val t2 = ctx.updated(x.name, t1) `|-` e
      // ----------- Lam
      expr `:` t1 ->: t2

    case App(f, e) =>
      val (t1, t2) = ctx `|-` f match {
        case FunType(inT, outT) => (inT, outT)
        case t => throw TypeException(s"expected function type and got $t")
      }
      val t1_ = ctx `|-` e
      t1 ==? t1_
      // ----------- App
      expr `:` t2

    case DepLambda(kind, x, e) =>
      val t = ctx `|-` e
      // ----------- DepLambda
      expr `:` DepFunType(kind, x, t)

    case DepApp(kind, f, e) =>
      val (x, t) = ctx `|-` f match {
        case DepFunType(kind2, x, t) if kind == kind2 => (x, t)
        case t => throw TypeException(s"expected dependent function type and got $t")
      }
      // ----------- DepApp
      expr `:` substitute.kindInType(kind, e, `for`= x, in = t)

    case Literal(d) =>
      // ----------- Literal
      expr `:` d.dataType

    // Annotations and assertions should be gone after type inference
    case TypeAnnotation(e, t) => e `:` t
    case TypeAssertion(e, t) => e `:` t
    case Opaque(e, t) => e `:` t

    case p: Primitive =>
      expr `:` p.t
  }

  implicit class TypeOp(t1: Type) {
    def ==?(t2: Type): Type = {
      if (t1 != t2) {
        throw TypeException(s"expected $t1 and $t2 to be equal")
      }
      t1 match {
        case TypePlaceholder =>
          throw TypeException(s"TypePlaceholder found")
        case _ => t1
      }
    }

    def `:`(e: Expr): Type = {
      e.t ==? t1
    }
  }
}
