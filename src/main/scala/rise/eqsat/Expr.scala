package rise.eqsat

import rise.core
import rise.core.{semantics, primitives => rcp, types => rct}
import rise.core.types.{DataType => rcdt}

object ExprWithHashCons {
  def nat(egraph: EGraph)(dt: NatId): Nat =
    Nat(egraph(dt).map(nat(egraph)))

  def dataType(egraph: EGraph)(dt: DataTypeId): DataType =
    DataType(egraph(dt).map(nat(egraph), dataType(egraph)))

  def `type`(egraph: EGraph)(t: TypeId): Type =
    Type(egraph(t).map(`type`(egraph), nat(egraph), dataType(egraph)))

  def expr(egraph: EGraph)(e: ExprWithHashCons): Expr =
    Expr(e.node.map(expr(egraph), nat(egraph), dataType(egraph), a => a), `type`(egraph)(e.t))

  def fromExpr(egraph: EGraph)(e: Expr): ExprWithHashCons =
    ExprWithHashCons(e.node.map(fromExpr(egraph), egraph.addNat, egraph.addDataType, a => a),
      egraph.addType(e.t))
}

case class ExprWithHashCons(node: Node[ExprWithHashCons, NatId, DataTypeId, Address], t: TypeId) {
  override def toString: String = s"($node : $t)"

  /** Shifts De-Bruijn indices up or down if they are >= cutoff */
  def shifted[E, ED, ND, DT](egraph: EGraph, shift: Expr.Shift, cutoff: Expr.Shift): ExprWithHashCons = {
    ExprWithHashCons(NodeSubs.shifted(egraph, node, shift, cutoff){ case (e, s, c) => e.shifted(egraph, s, c) },
      NodeSubs.Type.shifted(egraph, t, (shift._2, shift._3, shift._5), (cutoff._2, cutoff._3, cutoff._2)))
  }

  def replace[E, ED, ND, DT](egraph: EGraph, index: Int, subs: ExprWithHashCons): ExprWithHashCons = {
    NodeSubs.replace(node, index, subs)
    { n => ExprWithHashCons(n, t) }
    { case (e, i, s) => e.replace(egraph, i, s) }
    { case (e, s, c) => e.shifted(egraph, s, c) }
  }

  def replace[E, ED, ND, DT](egraph: EGraph, index: Int, subs: NatId): ExprWithHashCons = {
    ExprWithHashCons(NodeSubs.replace(egraph, node, index, subs){ case (e, i, s) => e.replace(egraph, i, s) },
      NodeSubs.Type.replace(egraph, t, index, subs))
  }

  // substitutes %0 for arg in this
  def withArgument[E, ED, ND, DT](egraph: EGraph, arg: ExprWithHashCons): ExprWithHashCons = {
    val argS = arg.shifted(egraph, (1, 0, 0, 0, 0), (0, 0, 0, 0, 0))
    val bodyR = this.replace(egraph, 0, argS)
    bodyR.shifted(egraph, (-1, 0, 0, 0, 0), (0, 0, 0, 0, 0))
  }

  // substitutes %n0 for arg in this
  def withNatArgument[E, ED, ND, DT](egraph: EGraph, arg: NatId): ExprWithHashCons = {
    val argS = NodeSubs.Nat.shifted(egraph, arg, (1, 0), (1, 0))
    val bodyR = this.replace(egraph, 0, argS)
    bodyR.shifted(egraph, (0, -1, 0, 0, 0), (0, 0, 0, 0, 0))
  }
}

// TODO: could also be outside of eqsat package
/** A Rise expression based on DeBruijn indexing */
case class Expr(node: Node[Expr, Nat, DataType, Address], t: Type) {
  override def toString: String = s"($node : $t)"
/*
  /** Shifts De-Bruijn indices up or down if they are >= cutoff */
  def shifted(shift: Expr.Shift, cutoff: Expr.Shift): Expr = {
    Expr(NodeSubs.shifted(node, shift, cutoff){ case (e, s, c) => e.shifted(s, c) },
      t.shifted((shift._2, shift._3), (cutoff._2, cutoff._3)))
  }

  def replace(index: Int, subs: Expr): Expr = {
    NodeSubs.replace(node, index, subs)
    { n => Expr(n, t) }
    { case (e, i, s) => e.replace(i, s) }
    { case (e, s, c) => e.shifted(s, c) }
  }

  def replace(index: Int, subs: Nat): Expr = {
    Expr(NodeSubs.replace(node, index, subs){ case (e, i, s) => e.replace(i, s) },
      t.replace(index, subs))
  }

  // substitutes %0 for arg in this
  def withArgument(arg: Expr): Expr = {
    replace(0, arg.shifted((1, 0, 0), (0, 0, 0)))
      .shifted((-1, 0, 0), (0, 0, 0))
  }

  // substitutes %n0 for arg in this
  def withNatArgument(arg: Nat): Expr = {
    replace(0, arg.shifted(1, 0))
      .shifted((0, -1, 0), (0, 0, 0))
  }
 */
}

object Expr {
  /** Shift expr, nat, datatype, address, and natToNat indices */
  type Shift = (Int, Int, Int, Int, Int)

  // TODO: could split into ToNamedScope and FromNamedScope
  trait Scope {
    def getExpr(i: Int): core.Identifier
    def getNat(i: Int): rct.NatIdentifier
    def getData(i: Int): rcdt.DataTypeIdentifier
    def getAddr(i: Int): rct.AddressSpaceIdentifier
    def getN2N(i: Int): rct.NatToNatIdentifier

    def indexOf(i: core.Identifier): Int
    def indexOf(i: rct.NatIdentifier): Int
    def indexOf(i: rcdt.DataTypeIdentifier): Int
    def indexOf(i: rct.AddressSpaceIdentifier): Int
    def indexOf(i: rct.NatToNatIdentifier): Int

    def +(i: core.Identifier): Scope
    def +(i: rct.NatIdentifier): Scope
    def +(i: rcdt.DataTypeIdentifier): Scope
    def +(i: rct.AddressSpaceIdentifier): Scope
    def +(i: rct.NatToNatIdentifier): Scope

    def bindExpr(t: rct.ExprType): (core.Identifier, Scope)
    def bindNat(): (rct.NatIdentifier, Scope)
    def bindData(): (rcdt.DataTypeIdentifier, Scope)
    def bindAddr(): (rct.AddressSpaceIdentifier, Scope)
    def bindN2N(): (rct.NatToNatIdentifier, Scope)
  }

  object Bound {
    def empty: Bound =
      new Bound(Seq(), Seq(), Seq(), Seq(), Seq(), allowFreeIndices = false)
  }

  sealed trait NotBoundException extends Exception
  case class IndexNotBound(i: Int) extends NotBoundException {
    override def toString: String = s"%$i was not bound"
  }
  case class NameNotBound(name: String) extends NotBoundException {
    override def toString: String = s"identifier $name was not bound"
  }
  
  trait ScopeWithBound extends Scope {
    val expr: Seq[core.Identifier]
    val nat: Seq[rct.NatIdentifier]
    val data: Seq[rcdt.DataTypeIdentifier]
    val addr: Seq[rct.AddressSpaceIdentifier]
    val n2n: Seq[rct.NatToNatIdentifier]
    val allowFreeIndices: Boolean

    private def get[T](s: Seq[T], i: Int, free: => T): T =
      s.lift(i).getOrElse {
        if (allowFreeIndices) { free } else { throw IndexNotBound(i) }
      }
    def getExpr(i: Int): core.Identifier =
      get(expr, i, core.Identifier(s"%$i")(rct.TypePlaceholder))
    def getNat(i: Int): rct.NatIdentifier =
      get(nat, i, rct.NatIdentifier(s"%n$i"))
    def getData(i: Int): rcdt.DataTypeIdentifier =
      get(data, i, rcdt.DataTypeIdentifier(s"%dt$i"))
    def getAddr(i: Int): rct.AddressSpaceIdentifier =
      get(addr, i, rct.AddressSpaceIdentifier(s"%a$i"))
    def getN2N(i: Int): rct.NatToNatIdentifier =
      get(n2n, i, rct.NatToNatIdentifier(s"%n2n$i"))

    private def indexOf[T](s: Seq[T], x: T): Int = {
      val i = s.indexOf(x)
      if (i >= 0) { i } else { throw NameNotBound(x.toString) }
    }
    def indexOf(i: core.Identifier): Int = indexOf(expr, i)
    def indexOf(i: rct.NatIdentifier): Int = indexOf(nat, i)
    def indexOf(i: rcdt.DataTypeIdentifier): Int = indexOf(data, i)
    def indexOf(i: rct.AddressSpaceIdentifier): Int = indexOf(addr, i)
    def indexOf(i: rct.NatToNatIdentifier): Int = indexOf(n2n, i)

    def makeIdent(): rct.ExprType => core.Identifier =
      core.Identifier(s"x${expr.size}") _
    def makeNatIdent(): rct.NatIdentifier =
      rct.NatIdentifier(s"n${nat.size}")
    def makeDataIdent(): rcdt.DataTypeIdentifier =
      rcdt.DataTypeIdentifier(s"dt${data.size}")
    def makeAddrIdent(): rct.AddressSpaceIdentifier =
      rct.AddressSpaceIdentifier(s"a${addr.size}")
    def makeN2NIdent(): rct.NatToNatIdentifier =
      rct.NatToNatIdentifier(s"n2n${n2n.size}")

    def bindExpr(t: rct.ExprType): (core.Identifier, Scope) = {
      val i = makeIdent()(t)
      (i, this + i)
    }
    def bindNat(): (rct.NatIdentifier, Scope) = {
      val i = makeNatIdent()
      (i, this + i)
    }
    def bindData(): (rcdt.DataTypeIdentifier, Scope) = {
      val i = makeDataIdent()
      (i, this + i)
    }
    def bindAddr(): (rct.AddressSpaceIdentifier, Scope) = {
      val i = makeAddrIdent()
      (i, this + i)
    }
    def bindN2N(): (rct.NatToNatIdentifier, Scope) = {
      val i = makeN2NIdent()
      (i, this + i)
    }
  }

  case class Bound(
    expr: Seq[core.Identifier],
    nat: Seq[rct.NatIdentifier],
    data: Seq[rcdt.DataTypeIdentifier],
    addr: Seq[rct.AddressSpaceIdentifier],
    n2n: Seq[rct.NatToNatIdentifier],
    allowFreeIndices: Boolean
  ) extends ScopeWithBound {
    def +(i: core.Identifier): Bound =
      this.copy(expr = i +: expr)
    def +(i: rct.NatIdentifier): Bound =
      this.copy(nat = i +: nat)
    def +(i: rcdt.DataTypeIdentifier): Bound =
      this.copy(data = i +: data)
    def +(i: rct.AddressSpaceIdentifier): Bound =
      this.copy(addr = i +: addr)
    def +(i: rct.NatToNatIdentifier): Bound =
      this.copy(n2n = i +: n2n)
  }

  def fromNamed(expr: core.Expr, scope: Scope = Bound.empty): Expr = {
    Expr(expr match {
      case i: core.Identifier => Var(scope.indexOf(i))
      case core.App(f, e) => App(fromNamed(f, scope), fromNamed(e, scope))
      case core.Lambda(i, e) => Lambda(fromNamed(e, scope + i))
      case core.DepApp(rct.NatKind, f, n: rct.Nat) =>
        NatApp(fromNamed(f, scope), Nat.fromNamed(n, scope))
      case core.DepApp(rct.DataKind, f, dt: rct.DataType) =>
        DataApp(fromNamed(f, scope), DataType.fromNamed(dt, scope))
      case core.DepApp(rct.AddressSpaceKind, f, a: rct.AddressSpace) =>
        AddrApp(fromNamed(f, scope), Address.fromNamed(a, scope))
      case core.DepApp(rct.NatToNatKind, f, n2n: rct.NatToNat) =>
        AppNatToNat(fromNamed(f, scope), NatToNat.fromNamed(n2n, scope))
      case core.DepApp(k, _, _) => throw new Exception(s"missing DepApp case for $k")
      case core.DepLambda(rct.NatKind, n: rct.NatIdentifier, e) =>
        NatLambda(fromNamed(e, scope + n))
      case core.DepLambda(rct.DataKind, dt: rcdt.DataTypeIdentifier, e) =>
        DataLambda(fromNamed(e, scope + dt))
      case core.DepLambda(rct.AddressSpaceKind, a: rct.AddressSpaceIdentifier, e) =>
        AddrLambda(fromNamed(e, scope + a))
      case core.DepLambda(rct.NatToNatKind, n2n: rct.NatToNatIdentifier, e) =>
        LambdaNatToNat(fromNamed(e, scope + n2n))
      case core.DepLambda(k, _, _) => throw new Exception(s"missing DepLambda case for $k")
      case core.Literal(core.semantics.NatData(n)) => NatLiteral(Nat.fromNamed(n, scope))
      case core.Literal(core.semantics.IndexData(i, n)) => IndexLiteral(Nat.fromNamed(i, scope), Nat.fromNamed(n, scope))
      case core.Literal(d) => Literal(d)
      // note: we set the primitive type to a place holder here,
      // because we do not want type information at the node level
      case p: core.Primitive => Primitive(p.setType(core.types.TypePlaceholder))
      case _: core.Opaque | _: core.TypeAnnotation | _: core.TypeAssertion =>
        throw new Exception("this should not happen")
    }, Type.fromNamed(expr.t, scope))
  }

  def toNamed(expr: Expr, scope: Scope = Bound.empty): core.Expr = {
    (expr.node match {
      case Var(index) => scope.getExpr(index).setType _
      case App(f, e) => core.App(toNamed(f, scope), toNamed(e, scope)) _
      case Lambda(e) =>
        val funT = expr.t.node.asInstanceOf[FunType[Type]]
        val (i, scope2) = scope.bindExpr(Type.toNamed(funT.inT, scope))
        core.Lambda(i, toNamed(e, scope2)) _
      case NatApp(f, x) =>
        core.DepApp(rct.NatKind, toNamed(f, scope), Nat.toNamed(x, scope)) _
      case NatLambda(e) =>
        val (i, scope2) = scope.bindNat()
        core.DepLambda(rct.NatKind, i, toNamed(e, scope2)) _
      case DataApp(f, x) =>
        core.DepApp(rct.DataKind, toNamed(f, scope), DataType.toNamed(x, scope)) _
      case DataLambda(e) =>
        val (i, scope2) = scope.bindData()
        core.DepLambda(rct.DataKind, i, toNamed(e, scope2)) _
      case AddrApp(f, x) =>
        core.DepApp(rct.AddressSpaceKind, toNamed(f, scope), Address.toNamed(x, scope)) _
      case AddrLambda(e) =>
        val (i, scope2) = scope.bindAddr()
        core.DepLambda(rct.AddressSpaceKind, i, toNamed(e, scope2)) _
      case AppNatToNat(f, x) =>
        core.DepApp(rct.NatToNatKind, toNamed(f, scope), NatToNat.toNamed(x, scope)) _
      case LambdaNatToNat(e) =>
        val (i, scope2) = scope.bindN2N()
        core.DepLambda(rct.NatToNatKind, i, toNamed(e, scope2)) _
      case Literal(d) => core.Literal(d).setType _
      case Primitive(p) => p.setType _
      case NatLiteral(n) => core.Literal(core.semantics.NatData(Nat.toNamed(n, scope))).setType _
      case IndexLiteral(i, n) => core.Literal(core.semantics.IndexData(
        Nat.toNamed(i, scope), Nat.toNamed(n, scope))).setType _

      case Composition(f, g) => /*
        val f2 = f.shifted((1, 0, 0), (0, 0, 0))
        val g2 = g.shifted((1, 0, 0), (0, 0, 0))
        val argT: Type = f2.t.node match {
          case FunType(inT, _) => inT
          case _ => throw new Exception("this should not happen")
        }
        return toNamed(ExprDSL.lam(argT, ExprDSL.app(g2, ExprDSL.app(f2, Expr(Var(0), argT)))), scope)
        */
        val ft = Type.toNamed(f.t, scope)
        val gt = Type.toNamed(g.t, scope)
        val t = Type.toNamed(expr.t, scope)
        core.App(core.App(
          NamedRewriteDSL.Composition(rct.FunType(ft, rct.FunType(gt, t))),
          toNamed(f, scope))(rct.FunType(gt, t)),
          toNamed(g, scope)) _
    })(Type.toNamed(expr.t, scope))
  }

  // instead of generating the same names in different scopes, generate globally unique names
  // useful for Elevate and DPIA assumptions
  def toNamedUnique(
    expr: Expr,
    counterExpr: Int = 0,
    counterNat: Int = 0,
    counterData: Int = 0,
    counterAddr: Int = 0,
    counterN2N: Int = 0
  ): core.Expr = {
    var cE = counterExpr
    var cN = counterNat
    var cD = counterData
    var cA = counterAddr
    var cN2N = counterN2N

    case class UniqueScope(
      expr: Seq[core.Identifier],
      nat: Seq[rct.NatIdentifier],
      data: Seq[rcdt.DataTypeIdentifier],
      addr: Seq[rct.AddressSpaceIdentifier],
      n2n: Seq[rct.NatToNatIdentifier],
    ) extends ScopeWithBound {
      val allowFreeIndices = false

      def +(i: core.Identifier): UniqueScope =
        this.copy(expr = i +: expr)
      def +(i: rct.NatIdentifier): UniqueScope =
        this.copy(nat = i +: nat)
      def +(i: rcdt.DataTypeIdentifier): UniqueScope =
        this.copy(data = i +: data)
      def +(i: rct.AddressSpaceIdentifier): UniqueScope =
        this.copy(addr = i +: addr)
      def +(i: rct.NatToNatIdentifier): UniqueScope =
        this.copy(n2n = i +: n2n)

      override def makeIdent(): rct.ExprType => core.Identifier = {
        cE += 1
        core.Identifier(s"x${cE}") _
      }
      override def makeNatIdent(): rct.NatIdentifier = {
        cN += 1
        rct.NatIdentifier(s"n${cN}")
      }
      override def makeDataIdent(): rcdt.DataTypeIdentifier = {
        cD += 1
        rcdt.DataTypeIdentifier(s"dt${cD}")
      }
      override def makeAddrIdent(): rct.AddressSpaceIdentifier = {
        cA += 1
        rct.AddressSpaceIdentifier(s"a${cA}")
      }
      override def makeN2NIdent(): rct.NatToNatIdentifier = {
        cN2N += 1
        rct.NatToNatIdentifier(s"n2n${cN2N}")
      }
    }

    toNamed(expr, UniqueScope(Seq(), Seq(), Seq(), Seq(), Seq()))
  }

  def simplifyNats(e: Expr): Expr =
    Expr(e.node.map(simplifyNats, Nat.simplify, DataType.simplifyNats, a => a),
      Type.simplifyNats(e.t))
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
  val int: DataType = DataType(ScalarType(rcdt.int))
  val f32: DataType = DataType(ScalarType(rcdt.f32))

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
