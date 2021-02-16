package rise.eqsat

import rise.{core => rc}
import rise.core.semantics._
import rise.core.types._

import scala.collection.mutable

// TODO: does it make sense use a Set/Map of alternatives?
final case class ExprSet(alternatives: mutable.ArrayBuffer[Expr], t: Type) {
  import ExprSet._

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)
  override def toString(): String = s"ExprSet ${System.identityHashCode(this)}"

  def add(other: ExprSet, visited: mutable.Set[ExprSet] = mutable.Set.empty): Unit = {
    if (!visited.contains(this)) {
      visited += this
      if (this == other) { return }
      assert(this.t == other.t)
      other.alternatives.foreach { a =>
        if (!alternatives.exists(_.merge(a, visited))) {
          alternatives += a
        }
      }
    }
  }

  def replace(other: ExprSet): Unit = {
    if (this == other) { return }
    assert(this.t == other.t)
    alternatives.clear()
    alternatives ++= other.alternatives
  }

  def filtered(predicate: Expr => Boolean): ExprSet = {
    val filtered = alternatives.filter(predicate)
    if (filtered.size < alternatives.size) {
      ExprSet(filtered, t)
    } else {
      this
    }
  }

  def represents(b: rc.Expr, env: AlphaEqEnv = AlphaEqEnv.empty): Boolean = {
    typeEquals(this.t, b.t, env) && alternatives.exists(_.represents(b, env))
  }

  def containsIdent(name: String, visited: mutable.Set[ExprSet] = mutable.Set.empty): Boolean = {
    if (!visited.contains(this)) {
      visited += this
      alternatives.exists(_.containsIdent(name, visited))
    } else {
      false // pretend that the ident is not here
    }
  }

  def substituteIdent(x: Identifier, v: ExprSet,
                      visited: mutable.Map[ExprSet, ExprSet] = mutable.Map.empty): ExprSet = {
    if (!visited.contains(this)) {
      if (containsIdent(x.name)) {
        val alts = mutable.ArrayBuffer[Expr]()
        visited += this -> ExprSet(alts, t)
        alternatives.foreach {
          _.substituteIdent(x, v, visited) match {
            case Left(es) => alts ++= es.alternatives
            case Right(e) => alts += e
          }
        }
      } else { // avoid rebuilding expressions that are not impacted
        visited += this -> this
      }
    }
    visited(this)
  }

  def substituteTypeIdent[K <: Kind](x: K#I, v: K#T,
                                     visited: mutable.Map[ExprSet, ExprSet] = mutable.Map.empty): ExprSet = {
    if (!visited.contains(this)) {
      visited += this -> ExprSet.one(Identifier("dummy"), f32) // FIXME
      ExprSet(alternatives.map(_.substituteTypeIdent(x, v, visited)), rc.substitute.kindInType(v, x, t))
    } else {
      ??? // TODO
    }
  }

  def applyEverywhere(r: rules.Rule, visited: mutable.Set[ExprSet] = mutable.Set.empty): Unit = {
    if (!visited.contains(this)) {
      visited += this
      r(this)
      alternatives.foreach { _.applyEverywhere(r, visited) }
    }
  }

  def countRepresentations(visited: mutable.Map[ExprSet, Double] = mutable.Map.empty): Double = {
    if (!visited.contains(this)) {
      visited += this -> (1.0/0.0) // recursion creates infinite representations
      visited(this) = alternatives.foldRight(0.0) { case (e, acc) =>
        acc + e.countRepresentations(visited)
      }
    }
    visited(this)
  }

  def countNodes(visited: mutable.Set[ExprSet] = mutable.Set.empty): Int = {
    if (!visited.contains(this)) {
      visited += this
      alternatives.foldRight(0) { case (e, acc) =>
        acc + e.countNodes(visited)
      }
    } else {
      0
    }
  }

  def generateDotString(): String = {
    val visited = mutable.Map.empty[ExprSet, String]
    s"""digraph g
       |{
       |graph [fontname = "FiraCode"]
       |node [fontname = "FiraCode"]
       |edge [fontname = "FiraCode"]
       |node [shape="record", style="rounded, filled"]
       |${generateDotRec(visited)}
       |}
       |""".stripMargin
  }

  def generateDotRec(visited: mutable.Map[ExprSet, String]): String = {
    if (!visited.contains(this)) {
      val id = s"es${visited.size}"
      visited += this -> id
      s"""$id [fillcolor=black, shape=point]
         |${alternatives.zipWithIndex.map { case (a, i) => a.generateDotRec(id, i, visited) }.mkString("") }
         |""".stripMargin
    } else {
      s""
    }
  }
}

sealed abstract class Expr {
  import ExprSet._

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)

  def represents(b: rc.Expr, env: AlphaEqEnv = AlphaEqEnv.empty): Boolean = {
    (this, b) match {
      case (ia: Identifier, ib: rc.Identifier) => env.identifiers.get(ia.name).contains(ib)
      case (Lambda(xa, ta, ea), rc.Lambda(xb, eb)) =>
        typeEquals(ta, xb.t, env) && ea.represents(eb, env withIdent (xa.name -> xb))
      case (App(fa, ea), rc.App(fb, eb)) => fa.represents(fb, env) && ea.represents(eb, env)
      case (DepLambda(xa, ea), rc.DepLambda(xb, eb)) => ea.represents(eb, env withTIdent (xa -> xb))
      case (DepApp(fa, xa), rc.DepApp(fb, xb)) => (xa == xb) && fa.represents(fb, env)
      case (Literal(da), rc.Literal(db)) => da == db
      case (Primitive(pa), pb: rc.Primitive) => pa == pb
      case _ => false
    }
  }

  def containsIdent(name: String, visited: mutable.Set[ExprSet] = mutable.Set.empty): Boolean = {
    this match {
      case ia: Identifier => ia.name == name
      case Lambda(_, _, ea) => ea.containsIdent(name, visited)
      case App(fa, ea) => fa.containsIdent(name, visited) || ea.containsIdent(name, visited)
      case DepLambda(_, ea) => ea.containsIdent(name, visited)
      case DepApp(fa, xa) => fa.containsIdent(name, visited) // TODO: xa could contain ident?
      case Literal(da) => false // TODO: da could contain ident?
      case Primitive(_) => false
    }
  }

  def substituteIdent(x: Identifier, v: ExprSet, visited: mutable.Map[ExprSet, ExprSet]): Either[ExprSet, Expr] = {
    this match {
      case ia: Identifier =>
        if (ia.name == x.name) {
          Left(v)
        } else {
          Right(ia)
        }
      case Lambda(xa, t, ea) =>
        Right(Lambda(xa, t, ea.substituteIdent(x, v, visited)))
      case App(fa, ea) =>
        Right(App(fa.substituteIdent(x, v, visited), ea.substituteIdent(x, v, visited)))
      case DepLambda(xa: NatIdentifier, ea) =>
        Right(DepLambda[NatKind](xa, ea.substituteIdent(x, v, visited)))
      case DepLambda(xa: DataTypeIdentifier, ea) =>
        Right(DepLambda[DataKind](xa, ea.substituteIdent(x, v, visited)))
      case DepLambda(xa: AddressSpaceIdentifier, ea) =>
        Right(DepLambda[AddressSpaceKind](xa, ea.substituteIdent(x, v, visited)))
      case DepLambda(_, ea) => ???
      case DepApp(fa, xa) => Right(DepApp(fa.substituteIdent(x, v, visited), xa))
      case Literal(da) => Right(this)
      case Primitive(_) => Right(this)
    }
  }

  def substituteTypeIdent(x: Kind#I, v: Kind#T, visited: mutable.Map[ExprSet, ExprSet]): Expr = {
    this match {
      case ia: Identifier => ia
      case Lambda(xa, t, ea) =>
        Lambda(xa, rc.substitute.kindInType(v, x, t), ea.substituteTypeIdent(x, v, visited))
      case App(fa, ea) =>
        App(fa.substituteTypeIdent(x, v, visited), ea.substituteTypeIdent(x, v, visited))
      case DepLambda(xa: NatIdentifier, ea) =>
        DepLambda[NatKind](xa, ea.substituteTypeIdent(x, v, visited))
      case DepLambda(xa: DataTypeIdentifier, ea) =>
        DepLambda[DataKind](xa, ea.substituteTypeIdent(x, v, visited))
      case DepLambda(xa: AddressSpaceIdentifier, ea) =>
        DepLambda[AddressSpaceKind](xa, ea.substituteTypeIdent(x, v, visited))
      case DepLambda(_, ea) => ???
      case DepApp(fa, xa) => DepApp(fa.substituteTypeIdent(x, v, visited), if (x == xa) { v } else { xa })
      case Literal(da) => Literal(
        rise.core.substitute.kindInExpr(v, x, rc.Literal(da)).asInstanceOf[rc.Literal].d)
      case Primitive(_) => this
    }
  }

  // returns how many alternatives were added
  def applyEverywhere(r: rules.Rule, visited: mutable.Set[ExprSet]): Unit = {
    this match {
      case _: Identifier => ()
      case Lambda(_, _, ea) => ea.applyEverywhere(r, visited)
      case App(fa, ea) => fa.applyEverywhere(r, visited); ea.applyEverywhere(r, visited)
      case DepLambda(_, ea) => ea.applyEverywhere(r, visited)
      case DepApp(fa, _) => fa.applyEverywhere(r, visited)
      case Literal(da) => ()
      case Primitive(_) => ()
    }
  }

  def merge(other: Expr, visited: mutable.Set[ExprSet]): Boolean = {
    (this, other) match {
      case (ia: Identifier, ib: Identifier) if ia.name == ib.name => true
      case (Lambda(xa, ta, ea), Lambda(xb, tb, eb)) if ta == tb =>
         ea.add(eb.substituteIdent(xb, ExprSet.one(xa, ta)), visited)
         true
      case (App(fa, ea), App(fb, eb)) if ea.t == eb.t =>
        // implies fa.t == fb.t
        fa.add(fb, visited)
        ea.add(eb, visited)
        true
      case (DepLambda(xa: NatIdentifier, ea), DepLambda(xb: NatIdentifier, eb)) =>
        ea.add(eb.substituteTypeIdent[NatKind](xb, xa), visited)
        true
      case (DepLambda(xa: DataTypeIdentifier, ea), DepLambda(xb: DataTypeIdentifier, eb)) =>
        ea.add(eb.substituteTypeIdent[DataKind](xb, xa), visited)
        true
      case (DepLambda(xa: AddressSpaceIdentifier, ea), DepLambda(xb: AddressSpaceIdentifier, eb)) =>
        ea.add(eb.substituteTypeIdent[AddressSpaceKind](xb, xa), visited)
        true
      case (DepApp(fa, xa), DepApp(fb, xb)) if (xa == xb) =>
        fa.add(fb, visited)
        true
      case (Literal(da), Literal(db)) if da == db => true
      case (Primitive(pa), Primitive(pb)) if pa == pb => true
      case _ => false
    }
  }

  def countRepresentations(visited: mutable.Map[ExprSet, Double]): Double = {
    this match {
      case _: Identifier => 1.0
      case Lambda(_, _, ea) => ea.countRepresentations(visited)
      case App(fa, ea) =>
        val a = fa.countRepresentations(visited)
        val b = ea.countRepresentations(visited)
        val r = a * b
        if (r.isNaN) {
          assert((a == 0.0 && b.isPosInfinity) || (b == 0.0 && a.isPosInfinity))
          0.0
        } else {
          r
        }
      case DepLambda(_, ea) => ea.countRepresentations(visited)
      case DepApp(fa, _) => fa.countRepresentations(visited)
      case Literal(_) => 1.0
      case Primitive(_) => 1.0
    }
  }

  def countNodes(visited: mutable.Set[ExprSet]): Int = {
    this match {
      case _: Identifier => 1
      case Lambda(_, _, ea) => ea.countNodes(visited)
      case App(fa, ea) => fa.countNodes(visited) + ea.countNodes(visited)
      case DepLambda(_, ea) => ea.countNodes(visited)
      case DepApp(fa, _) => fa.countNodes(visited)
      case Literal(_) => 1
      case Primitive(_) => 1
    }
  }

  def generateDotRec(pid: String, i: Int, visited: mutable.Map[ExprSet, String]): String = {
    val id = s"${pid}_$i"
    s"$pid -> $id\n" + (this match {
      case Identifier(name) =>
        s"$id [fillcolor=white, label=$name]"
      case Lambda(Identifier(name), _, e) =>
        s"""$id [fillcolor=white, label=λ$name]
           |${e.generateDotRec(visited)}
           |$id -> ${visited(e)}
           |""".stripMargin
      case App(f, e) =>
        s"""$id [fillcolor=white, label=app]
           |${f.generateDotRec(visited)}
           |${e.generateDotRec(visited)}
           |$id -> ${visited(f)} [label=fun]
           |$id -> ${visited(e)} [label=arg]
           |""".stripMargin
      case DepLambda(x, e) =>
        s"""$id [fillcolor=white, label=Λ${x.name}]
           |${e.generateDotRec(visited)}
           |$id -> ${visited(e)}
           |""".stripMargin
      case DepApp(f, v) =>
        val vid = s"${id}_v"
        s"""$id [fillcolor=white, label=depApp]
           |${f.generateDotRec(visited)}
           |$vid [fillcolor=white, label=$v]
           |$id -> ${visited(f)} [label=fun]
           |$id -> $vid [label=arg]
           |""".stripMargin
      case Literal(d) =>
        s"$id [fillcolor=white, label=<$d>]"
      case Primitive(p) =>
        s"$id [fillcolor=gray, label=${p.toString.trim}]"
    })
  }
}

final case class Identifier(name: String) extends Expr
final case class Lambda(x: Identifier, t: Type, e: ExprSet) extends Expr
final case class App(f: ExprSet, e: ExprSet) extends Expr
final case class DepLambda[K <: Kind: KindName](
                                                 x: K#I with Kind.Explicitness,
                                                 e: ExprSet
                                               ) extends Expr
final case class DepApp[K <: Kind](f: ExprSet, x: K#T) extends Expr
final case class Literal(d: Data) extends Expr
final case class Primitive(p: rc.Primitive) extends Expr

object ExprSet {
  def init(e: rc.Expr): ExprSet = {
    val expr = e match {
      case i: rc.Identifier => Identifier(i.name)
      case rc.Lambda(x, e) => Lambda(Identifier(x.name), x.t, init(e))
      case rc.App(f, e) => App(init(f), init(e))
      case rc.DepLambda(x: NatIdentifier, e) => DepLambda[NatKind](x, init(e))
      case rc.DepLambda(x: DataTypeIdentifier, e) => DepLambda[DataKind](x, init(e))
      case rc.DepLambda(x: AddressSpaceIdentifier, e) => DepLambda[AddressSpaceKind](x, init(e))
      case rc.DepLambda(_, ea) => ???
      case rc.DepApp(f, x) => DepApp(init(f), x)
      case rc.Literal(d) => Literal(d)
      case p: rc.Primitive => Primitive(p)
    }
    ExprSet.one(expr, e.t)
  }

  def one(e: Expr, t: Type): ExprSet =
    ExprSet(mutable.ArrayBuffer(e), t)

  case class AlphaEqEnv(identifiers: Map[String, rc.Identifier],
                        typeIdentifiers: Map[Kind.Identifier, Kind.Identifier]) {
    def withIdent(p: (String, rc.Identifier)): AlphaEqEnv =
      this.copy(identifiers = identifiers + p)
    def withTIdent(p: (Kind.Identifier, Kind.Identifier)): AlphaEqEnv =
      this.copy(typeIdentifiers = typeIdentifiers + p)
  }

  object AlphaEqEnv {
    def empty: AlphaEqEnv = AlphaEqEnv(Map.empty, Map.empty)
  }

  def typeEquals(a: Type, b: Type, env: AlphaEqEnv): Boolean = {
    (a, b) match {
      case (TypePlaceholder, _) | (_, TypePlaceholder) => true
      case (FunType(inTa, outTa), FunType(inTb, outTb)) =>
        typeEquals(inTa, inTb, env) && typeEquals(outTa, outTb, env)
      case (DepFunType(xa, ta), DepFunType(xb, tb)) =>
        typeEquals(ta, tb, env withTIdent (xa -> xb))
      case (dta: DataTypeIdentifier, dtb: DataTypeIdentifier) =>
        env.typeIdentifiers.get(dta).contains(dtb)
      case (sa: ScalarType, sb: ScalarType) =>
        sa == sb
      case (NatType, NatType) =>
        true
      case (VectorType(sa, ea), VectorType(sb, eb)) =>
        natEquals(sa, sb, env) && typeEquals(ea, eb, env)
      case (IndexType(sa), IndexType(sb)) =>
        natEquals(sa, sb, env)
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        natEquals(sa, sb, env) && typeEquals(ea, eb, env)
      case (PairType(dt1a, dt2a), PairType(dt1b, dt2b)) =>
        typeEquals(dt1a, dt1b, env) && typeEquals(dt2a, dt2b, env)
      case _ => false
    }
  }

  def natEquals(a: Nat, b: Nat, env: AlphaEqEnv): Boolean = {
    // TODO: avoid substitution?
    rc.substitute.natsInNat(env.typeIdentifiers.collect { case (k: NatIdentifier, v: Nat) => (k, v) }, a) == b
  }
}