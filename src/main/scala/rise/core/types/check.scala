package rise.core.types

import rise.core.dsl.Type._
import rise.core.exprs._
import rise.core.util.substitute

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

    case DepLambda(x, e) =>
      val t = ctx `|-` e
      // ----------- DepLambda
      expr `:` (x match {
        case n: NatIdentifier => DepFunType[NatKind, Type](n, t)
        case dt: DataTypeIdentifier => DepFunType[DataKind, Type](dt, t)
        case a: AddressSpaceIdentifier => DepFunType[AddressSpaceKind, Type](a, t)
        case n2n: NatToNatIdentifier => DepFunType[NatToNatKind, Type](n2n, t)
        case n2d: NatToDataIdentifier => DepFunType[NatToDataKind, Type](n2d, t)
      })

    case DepApp(f, e) =>
      val (x, t) = ctx `|-` f match {
        case DepFunType(x, t) => (x, t)
        case t => throw TypeException(s"expected dependent function type and got $t")
      }
      // ----------- DepApp
      expr `:` substitute.kindInType(e, `for`= x, in = t)

    case Literal(d) =>
      // ----------- Literal
      expr `:` d.dataType

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
        case i: Kind.Explicitness if !i.isExplicit =>
          throw TypeException(s"Implicit type variable found")
        case _ => t1
      }
    }

    def `:`(e: Expr): Type = {
      e.t ==? t1
    }
  }
}
