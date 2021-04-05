package rise.eqsat

import rise.core
import rise.core.{primitives => rcp}
import rise.core.semantics
import rise.core.semantics.NatData
import rise.core.types.TypePlaceholder
import rise.core.{types => rct}

// TODO: could also be outside of eqsat package
case class Expr(node: Node[Expr, Nat, DataType], t: Type) {
  override def toString: String = s"(${node.toString} : $t)"

  // shifts De-Bruijn indices up or down if they are >= cutoff
  def shifted(up: Boolean, cutoff: Int): Expr = {
    Expr(node match {
      case Var(index) =>
        val newIndex = if (index >= cutoff) {
          if (up) { index + 1 } else { index - 1 }
        } else {
          index
        }
        Var(newIndex)
      case Lambda(e) => Lambda(e.shifted(up, cutoff + 1))
      case App(f, e) => App(f.shifted(up, cutoff), e.shifted(up, cutoff))
      case NatLambda(e) => NatLambda(e.shifted(up, cutoff))
      case NatApp(f, x) => NatApp(f.shifted(up, cutoff), x)
      case DataLambda(e) => DataLambda(e.shifted(up, cutoff))
      case DataApp(f, x) => DataApp(f.shifted(up, cutoff), x)
      case Literal(_) | Primitive(_) => node
    }, t)
  }

  def replace(index: Int, subs: Expr): Expr = {
    node match {
      case Var(idx) if idx == index => subs
      case Var(_) => this
      case Lambda(e) =>
        val e2 = e.replace(index + 1, subs.shifted(up = true, 0))
        Expr(Lambda(e2), t)
      case App(f, e) =>
        val f2 = f.replace(index, subs)
        val e2 = e.replace(index, subs)
        Expr(App(f2, e2), t)
      case NatLambda(e) => Expr(NatLambda(e.replace(index, subs)), t)
      case NatApp(f, x) => Expr(NatApp(f.replace(index, subs), x), t)
      case DataLambda(e) => Expr(DataLambda(e.replace(index, subs)), t)
      case DataApp(f, x) => Expr(DataApp(f.replace(index, subs), x), t)
      case Literal(_) | Primitive(_) => this
    }
  }

  // substitutes %0 for arg in this
  def withArgument(arg: Expr): Expr = {
    replace(0, arg.shifted(up = true, 0))
      .shifted(up = false, 0)
  }
}

object Expr {
  case class Bound(expr: Seq[core.Identifier],
                   nat: Seq[rct.NatIdentifier],
                   data: Seq[rct.DataTypeIdentifier]) {
    def indexOf(i: core.Identifier): Int =
      expr.indexOf(i)
    def indexOf(i: rct.NatIdentifier): Int =
      nat.indexOf(i)
    def indexOf(i: rct.DataTypeIdentifier): Int =
      data.indexOf(i)

    def +(i: core.Identifier): Bound =
      this.copy(expr = i +: expr)
    def +(i: rct.NatIdentifier): Bound =
      this.copy(nat = i +: nat)
    def +(i: rct.DataTypeIdentifier): Bound =
      this.copy(data = i +: data)
  }

  def fromNamed(e: core.Expr): Expr = {
    def rec(e: core.Expr,
            bound: Bound): Expr = {
      Expr(e match {
        case i: core.Identifier => Var(bound.indexOf(i))
        case core.App(f, e) => App(rec(f, bound), rec(e, bound))
        case core.Lambda(i, e) => Lambda(rec(e, bound + i))
        case core.DepApp(f, n: rct.Nat) => NatApp(rec(f, bound), Nat.fromNamed(n, bound))
        case core.DepApp(f, dt: rct.DataType) => DataApp(rec(f, bound), DataType.fromNamed(dt, bound))
        case core.DepApp(_, _) => ???
        case core.DepLambda(n: rct.NatIdentifier, e) =>
          NatLambda(rec(e, bound + n))
        case core.DepLambda(dt: rct.DataTypeIdentifier, e) =>
          DataLambda(rec(e, bound + dt))
        case core.DepLambda(_, _) => ???
        case core.Literal(d) => Literal(d)
        case p: core.Primitive => Primitive(p.setType(core.types.TypePlaceholder))
      }, Type.fromNamed(e.t, bound))
    }

    rec(e, Bound(Seq(), Seq(), Seq()))
  }

  def toNamed(e: Expr): core.Expr = {
    def rec(expr: Expr, bound: Bound): core.Expr = {
      (expr.node match {
        case Var(index) => bound.expr(index).setType _
        case App(f, e) => core.App(rec(f, bound), rec(e, bound)) _
        case Lambda(e) =>
          val i = core.Identifier(s"x${bound.expr.size}")(Type.toNamed(expr.t.node.asInstanceOf[FunType[Type]].inT, bound))
          core.Lambda(i, rec(e, bound + i)) _
        // TODO: Nat and DataType toNamed
        case NatApp(f, x) => core.DepApp[rct.NatKind](rec(f, bound), Nat.toNamed(x, bound)) _
        case NatLambda(e) =>
          val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
          core.DepLambda[rct.NatKind](i, rec(e, bound + i)) _
        case DataApp(f, x) => core.DepApp[rct.DataKind](rec(f, bound), DataType.toNamed(x, bound)) _
        case DataLambda(e) =>
          val i = rct.DataTypeIdentifier(s"dt${bound.data.size}", isExplicit = true)
          core.DepLambda[rct.DataKind](i, rec(e, bound + i)) _
        case Literal(d) => core.Literal(d).setType _
        case Primitive(p) => p.setType _
      })(Type.toNamed(expr.t, bound))
    }

    rec(e, Bound(Seq(), Seq(), Seq()))
  }
}

object ExprDSL {
  import scala.language.implicitConversions
  private def expr(n: Node[Expr, Nat, DataType]) = Expr(n, Type(NatType))

  // TODO: types?
  def %(index: Int): Expr = expr(Var(index))
  def app(a: Expr, b: Expr): Expr = expr(App(a, b))
  def lam(e: Expr): Expr = expr(Lambda(e))
  def nApp(f: Expr, x: Nat): Expr = expr(NatApp(f, x))
  def nLam(e: Expr): Expr = expr(NatLambda(e))
  def map: Expr = expr(Primitive(rcp.map.primitive))
  def transpose: Expr = expr(Primitive(rcp.transpose.primitive))
  def add: Expr = expr(Primitive(rcp.add.primitive))
  def mul: Expr = expr(Primitive(rcp.mul.primitive))
  def div: Expr = expr(Primitive(rcp.div.primitive))
  def l(d: semantics.Data): Expr = expr(Literal(d))

  val f32: DataType = DataType(ScalarType(rct.f32))
  def n(index: Int): Nat = Nat(NatVar(index))

  def nFunT(t: Type): Type = Type(NatFunType(t))
  implicit def dataTypeToType(dt: DataType) = Type(dt.node)
  implicit final class FunConstructorT(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): Type = Type(FunType(t, r))
  }
  implicit final class FunConstructorDT(private val r: DataType) extends AnyVal {
    @inline def ->:(t: Type): Type = Type(FunType(t, r : Type))
  }
  implicit final class ArrayConstructor(private val s: Nat) extends AnyVal {
    @inline def `.`(et: DataType): DataType = DataType(ArrayType(s, et))
  }
}