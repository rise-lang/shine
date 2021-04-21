package rise.eqsat

import rise.core
import rise.core.{primitives => rcp}
import rise.core.semantics
import rise.core.{types => rct}

// TODO: could also be outside of eqsat package
/** A Rise expression based on DeBruijn indexing */
case class Expr(node: Node[Expr, Nat, DataType], t: Type) {
  override def toString: String = s"($node : $t)"

  /** Shifts De-Bruijn indices up or down if they are >= cutoff
    * @todo some traversals could be avoided for 0-shifts? */
  def shifted(shift: Expr.Shift, cutoff: Expr.Shift): Expr = {
    Expr(node match {
      case Var(index) =>
        val delta = if (index >= cutoff._1) shift._1 else 0
        Var(index + delta)
      case Lambda(e) =>
        Lambda(e.shifted(shift, cutoff.copy(_1 = cutoff._1 + 1)))
      case App(f, e) =>
        App(f.shifted(shift, cutoff), e.shifted(shift, cutoff))
      case NatLambda(e) =>
        NatLambda(e.shifted(shift, cutoff.copy(_2 = cutoff._2 + 1)))
      case NatApp(f, x) =>
        NatApp(f.shifted(shift, cutoff), x)
      case DataLambda(e) =>
        DataLambda(e.shifted(shift, cutoff.copy(_3 = cutoff._3 + 1)))
      case DataApp(f, x) =>
        DataApp(f.shifted(shift, cutoff), x)
      case Literal(_) | Primitive(_) => node
    }, t)
  }

  def replace(index: Int, subs: Expr): Expr = {
    node match {
      case Var(idx) if idx == index => subs
      case Var(_) => this
      case Lambda(e) =>
        // TODO: could shift lazily
        val e2 = e.replace(index + 1, subs.shifted((1, 0, 0), (0, 0, 0)))
        Expr(Lambda(e2), t)
      case App(f, e) =>
        val f2 = f.replace(index, subs)
        val e2 = e.replace(index, subs)
        Expr(App(f2, e2), t)
      case NatLambda(e) =>
        // TODO: could shift lazily
        val subs2 = subs.shifted((0, 1, 0), (0, 0, 0))
        Expr(NatLambda(e.replace(index, subs2)), t)
      case NatApp(f, x) =>
        Expr(NatApp(f.replace(index, subs), x), t)
      case DataLambda(e) =>
        // TODO: could shift lazily
        val subs2 = subs.shifted((0, 0, 1), (0, 0, 0))
        Expr(DataLambda(e.replace(index, subs2)), t)
      case DataApp(f, x) =>
        Expr(DataApp(f.replace(index, subs), x), t)
      case Literal(_) | Primitive(_) => this
    }
  }

  // substitutes %0 for arg in this
  def withArgument(arg: Expr): Expr = {
    replace(0, arg.shifted((1, 0, 0), (0, 0, 0)))
      .shifted((-1, 0, 0), (0, 0, 0))
  }
}

object Expr {
  /** Shift expr, nat and datatype indices */
  type Shift = (Int, Int, Int)

  object Bound {
    def empty: Bound = Bound(Seq(), Seq(), Seq())
  }

  case class Bound(expr: Seq[core.Identifier],
                   nat: Seq[rct.NatIdentifier],
                   data: Seq[rct.DataTypeIdentifier]) {
    private def assertFound(i: Int, name: => String): Int =
      if (i >= 0) { i } else { throw new Exception(s"identifier $name was not bound") }
    def indexOf(i: core.Identifier): Int =
      assertFound(expr.indexOf(i), i.toString)
    def indexOf(i: rct.NatIdentifier): Int =
      assertFound(nat.indexOf(i), i.toString)
    def indexOf(i: rct.DataTypeIdentifier): Int =
      assertFound(data.indexOf(i), i.toString)

    def +(i: core.Identifier): Bound =
      this.copy(expr = i +: expr)
    def +(i: rct.NatIdentifier): Bound =
      this.copy(nat = i +: nat)
    def +(i: rct.DataTypeIdentifier): Bound =
      this.copy(data = i +: data)
  }

  def fromNamed(expr: core.Expr, bound: Bound = Bound.empty): Expr = {
    Expr(expr match {
      case i: core.Identifier => Var(bound.indexOf(i))
      case core.App(f, e) => App(fromNamed(f, bound), fromNamed(e, bound))
      case core.Lambda(i, e) => Lambda(fromNamed(e, bound + i))
      case core.DepApp(f, n: rct.Nat) =>
        NatApp(fromNamed(f, bound), Nat.fromNamed(n, bound))
      case core.DepApp(f, dt: rct.DataType) =>
        DataApp(fromNamed(f, bound), DataType.fromNamed(dt, bound))
      case core.DepApp(_, _) => ???
      case core.DepLambda(n: rct.NatIdentifier, e) =>
        NatLambda(fromNamed(e, bound + n))
      case core.DepLambda(dt: rct.DataTypeIdentifier, e) =>
        DataLambda(fromNamed(e, bound + dt))
      case core.DepLambda(_, _) => ???
      case core.Literal(d) => Literal(d)
      case p: core.Primitive => Primitive(p)//.setType(core.types.TypePlaceholder))
    }, Type.fromNamed(expr.t, bound))
  }

  def toNamed(expr: Expr, bound: Bound = Bound.empty): core.Expr = {
    (expr.node match {
      case Var(index) => bound.expr(index).setType _
      case App(f, e) => core.App(toNamed(f, bound), toNamed(e, bound)) _
      case Lambda(e) =>
        val funT = expr.t.node.asInstanceOf[FunType[Type]]
        val i = core.Identifier(s"x${bound.expr.size}")(Type.toNamed(funT.inT, bound))
        core.Lambda(i, toNamed(e, bound + i)) _
      case NatApp(f, x) => core.DepApp[rct.NatKind](toNamed(f, bound), Nat.toNamed(x, bound)) _
      case NatLambda(e) =>
        val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
        core.DepLambda[rct.NatKind](i, toNamed(e, bound + i)) _
      case DataApp(f, x) => core.DepApp[rct.DataKind](toNamed(f, bound), DataType.toNamed(x, bound)) _
      case DataLambda(e) =>
        val i = rct.DataTypeIdentifier(s"dt${bound.data.size}", isExplicit = true)
        core.DepLambda[rct.DataKind](i, toNamed(e, bound + i)) _
      case Literal(d) => core.Literal(d).setType _
      case Primitive(p) => p.setType _
    })(Type.toNamed(expr.t, bound))
  }
}

object ExprDSL {
  import scala.language.implicitConversions

  implicit def dataTypeToType(dt: DataType): Type = Type(dt.node)

  def %(index: Int, t: Type): Expr = Expr(Var(index), t)
  def lam(argT: Type, e: Expr): Expr = Expr(Lambda(e), argT ->: e.t)
  def app(a: Expr, b: Expr): Expr =
    Expr(App(a, b), a.t.node.asInstanceOf[FunType[Type]].outT)
  // TODO: could compute nApp type using type index shifts (withNatArgument)
  def nApp(f: Expr, x: Nat, t: Type): Expr = Expr(NatApp(f, x), t)
  def nLam(e: Expr): Expr = Expr(NatLambda(e), nFunT(e.t))
  def l(d: semantics.Data): Expr = Expr(Literal(d), DataType.fromNamed(d.dataType))

  def map(t: Type): Expr = Expr(Primitive(rcp.map.primitive), t)
  def transpose(t: Type): Expr = Expr(Primitive(rcp.transpose.primitive), t)
  def add(t: Type): Expr = Expr(Primitive(rcp.add.primitive), t)
  def mul(t: Type): Expr = Expr(Primitive(rcp.mul.primitive), t)

  def `%n`(index: Int): Nat = Nat(NatVar(index))
  def cst(value: Long): Nat = Nat(NatCst(value))

  def `%dt`(index: Int): DataType = DataType(DataTypeVar(index))
  val int: DataType = DataType(ScalarType(rct.int))
  val f32: DataType = DataType(ScalarType(rct.f32))

  def nFunT(t: Type): Type = Type(NatFunType(t))
  implicit final class FunConstructorT(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): Type = Type(FunType(t, r))
  }
  implicit final class FunConstructorDT(private val r: DataType) extends AnyVal {
    @inline def ->:(t: Type): Type = Type(FunType(t, r: Type))
  }
  implicit final class ArrayConstructor(private val s: Nat) extends AnyVal {
    @inline def `.`(et: DataType): DataType = DataType(ArrayType(s, et))
  }
}
