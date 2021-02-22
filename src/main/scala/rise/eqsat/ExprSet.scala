package rise.eqsat

import rise.{core => rc}
import rise.core.semantics._
import rise.core.types._

import scala.collection.mutable

// TODO: we could use something like a Map of alternatives/Exprs to quickly find a pattern (e.g. all app nodes)
// TODO: could we avoid re-applying rules that already triggered?
// TODO: investigate benefits of hashconsing or full-fledged e-graph
final case class ExprSet(alternatives: mutable.ArrayBuffer[Expr], t: Type) {
  import ExprSet._

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)
  override def toString(): String = s"ExprSet ${System.identityHashCode(this)}"

  // returns whether something was added to the set
  def add(other: ExprSet, visited: mutable.Set[ExprSet] = mutable.Set.empty): Boolean = {
    var modified = false
    if (!visited.contains(this)) {
      visited += this
      if (this == other) { return false }
      assert(this.t == other.t)
      other.alternatives.foreach { oa =>
        val merged = alternatives.exists { ta =>
          val (merge_success, set_modified) = ta.merge(oa, visited)
          if (set_modified) modified = true
          merge_success
        }
        if (!merged) {
          alternatives += oa
          modified = true
        }
      }
    }
    modified
  }

  // calling this can create dead-ends (empty program sets)
  def remove(trace: Seq[Expr]): Unit = {
    def exprSetRec(es: ExprSet, trace: Seq[Expr]): Boolean = {
      trace match {
        case alternative +: rest =>
          val idx = es.alternatives.indexOf(alternative)
          if (idx < 0) {
            false
          } else {
            val e = es.alternatives(idx)
            // TODO: is this safe to do?
            es.alternatives.remove(idx)
            exprRec(e, rest)
          }
        case Nil => true
      }
    }
    def exprRec(e: Expr, trace: Seq[Expr]): Boolean = {
      e match {
        case _: Identifier | Literal(_) | Primitive(_) => trace.isEmpty
        case Lambda(_, _, e) => exprSetRec(e, trace)
        case App(f, e) => exprSetRec(f, trace) || exprSetRec(e, trace)
        case DepLambda(_, e) => exprSetRec(e, trace)
        case DepApp(f, _) => exprSetRec(f, trace)
      }
    }
    assert(exprSetRec(this, trace))
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

  def substituteIdent(x: Identifier, v: ExprSet): ExprSet = {
    // collect free identifiers upfront, we will need them
    val freeIdents = mutable.Map[ExprSet, Set[String]]()
    def exprSetRec(es: ExprSet): Set[String] = {
      if (!freeIdents.contains(es)) {
        freeIdents += es -> Set()
        freeIdents(es) = es.alternatives.iterator.flatMap(exprRec).toSet
      }
      freeIdents(es)
    }
    def exprRec: Expr => Set[String] = {
      case i: Identifier => Set(i.name)
      case Lambda(x, _, e) => exprSetRec(e) - x.name
      case App(f, e) =>
        exprSetRec(f) ++ exprSetRec(e)
      case DepLambda(_, e) => exprSetRec(e)
      case DepApp(f, _) => exprSetRec(f)
      case Literal(_) => Set()
      case Primitive(_) => Set()
    }
    exprSetRec(this)
    substituteIdentRec(x, v, freeIdents, mutable.Map.empty)
  }

  def substituteIdentRec(x: Identifier, v: ExprSet,
                         freeIdents: mutable.Map[ExprSet, Set[String]],
                         visited: mutable.Map[ExprSet, ExprSet]): ExprSet = {
    if (!visited.contains(this)) {
      if (!freeIdents(this)(x.name)) { // avoid rebuilding expressions that are not impacted
        visited += this -> this
      } else {
        val alts = mutable.ArrayBuffer[Expr]()
        visited += this -> ExprSet(alts, t)
        alternatives.foreach {
          _.substituteIdentRec(x, v, freeIdents, visited) match {
            case Left(es) => alts ++= es.alternatives
            case Right(e) => alts += e
          }
        }
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

  def countRepresentations(visited: mutable.Map[ExprSet, Double] = mutable.Map.empty): Double = {
    if (!visited.contains(this)) {
      visited += this -> (1.0/0.0) // recursion creates infinite representations
      visited(this) = alternatives.foldRight(0.0) { case (e, acc) =>
        acc + e.countRepresentations(visited)
      }
    }
    visited(this)
  }

  // FIXME: this starts to look a lot like e-graphs rebuilding,
  //  but less efficient / formal
  def removeDuplicates(): Unit = {
    val (eParents, esParents) = collectParents()
    def rec(): Unit = {
      val toUnify = eParents.iterator.flatMap { case (e, eps) =>
        if (eps.size > 1) { Some(e, eps.toSeq) } else { None }
      }.toSeq
      if (toUnify.isEmpty) { return }

      toUnify.foreach { case (expr, eps) =>
        // all the `eps` are the same, we unify them to `eps.head`
        val unified = eps.head
        eps.tail.foreach { es =>
          esParents(es).foreach { e =>
            val r = e match {
              case Lambda(x, xt, e) =>
                assert(e == es)
                Lambda(x, xt, unified)
              case App(f, e) =>
                assert(f == es || e == es)
                App(if (f == es) unified else f, if (e == es) unified else e)
              case DepLambda(x, e) => ??? // DepLambda(x, if (e == es) eps.head else e)
              case DepApp(f, x) =>
                assert(f == es)
                DepApp(unified, x)
              case _ => throw new Exception(s"did not expect $e")
            }
            eParents(e).foreach { es =>
              val idx = es.alternatives.indexOf(e)
              assert(idx > 0)
              es.alternatives.remove(idx)
            }
            eParents.getOrElseUpdate(r, mutable.Set.empty[ExprSet]) ++= eParents(e)
          }
          esParents(unified) ++= esParents(es)
          esParents(es).clear()
          eParents(expr) -= es
        }
      }

      rec()
    }
    util.dotPrintTmp("before", this)
    rec()
    util.dotPrintTmp("after", this)
  }

  private def collectParents(): (
    mutable.Map[Expr, mutable.Set[ExprSet]],
    mutable.Map[ExprSet, mutable.Set[Expr]],
    ) = {
    val eParents = mutable.Map.empty[Expr, mutable.Set[ExprSet]]
    val esParents = mutable.Map.empty[ExprSet, mutable.Set[Expr]]
    def EToES(e: Expr, es: ExprSet): Unit = {
      val ps = esParents.getOrElseUpdate(es, mutable.Set.empty)
      ps += e
    }
    def ESToE(es: ExprSet, e: Expr): Unit = {
      val ps = eParents.getOrElseUpdate(e, mutable.Set.empty)
      ps += es
    }

    val visited = mutable.Set.empty[ExprSet]

    def exprSetRec(es: ExprSet): Unit = {
      if (!visited.contains(es)) {
        visited += es
        es.alternatives.foreach(exprRec(es))
      }
    }

    def exprRec(parent: ExprSet): Expr => Unit = { expr =>
      ESToE(parent, expr)
      expr match {
        case _: Identifier => ()
        case Lambda(_, _, e) =>
          EToES(expr, e)
          exprSetRec(e)
        case App(f, e) =>
          EToES(expr, f)
          EToES(expr, e)
          exprSetRec(f); exprSetRec(e)
        case DepLambda(_, e) =>
          EToES(expr, e)
          exprSetRec(e)
        case DepApp(f, _) =>
          EToES(expr, f)
          exprSetRec(f)
        case Literal(_) => ()
        case Primitive(_) => ()
      }
    }

    exprSetRec(this)
    (eParents, esParents)
  }

  def removeEmptySets(): Unit = {
    val counts = mutable.Map.empty[ExprSet, Double]
    countRepresentations(counts)

    val visited = mutable.Set.empty[ExprSet]
    def exprSetRec(es: ExprSet): Unit = {
      if (!visited.contains(es)) {
        visited += es
        es.alternatives.filterInPlace { e =>
          e.countRepresentations(counts) > 0.0
        }
        es.alternatives.foreach(exprRec)
      }
    }
    def exprRec: Expr => Unit = {
      case _: Identifier => ()
      case Lambda(_, _, e) => exprSetRec(e)
      case App(f, e) => exprSetRec(f); exprSetRec(e)
      case DepLambda(_, e) => exprSetRec(e)
      case DepApp(f, _) => exprSetRec(f)
      case Literal(_) => ()
      case Primitive(_) => ()
    }
    exprSetRec(this)
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
    val visitedES = mutable.Map.empty[ExprSet, String]
    // FIXME: the fact that we need to keep track of visitedE
    //  shows an issue for rewriting
    val visitedE = mutable.Map.empty[Expr, String]
    s"""digraph g
       |{
       |graph [fontname = "FiraCode"]
       |node [fontname = "FiraCode"]
       |edge [fontname = "FiraCode"]
       |node [shape="record", style="rounded, filled"]
       |${generateDotRec(visitedES, visitedE)}
       |}
       |""".stripMargin
  }

  def generateDotRec(visitedES: mutable.Map[ExprSet, String],
                     visitedE: mutable.Map[Expr, String]): String = {
    if (visitedES.contains(this)) {
      return ""
    }
    val id = s"es${visitedES.size}"
    visitedES += this -> id
    s"""$id [fillcolor=black, shape=point]
       |${alternatives.zipWithIndex.map {
            case (a, i) => a.generateDotRec(id, i, visitedES, visitedE)
          }.mkString("")}
       |""".stripMargin
  }
}

sealed abstract class Expr {
  import ExprSet._
/*
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)
*/
  def represents(b: rc.Expr, env: AlphaEqEnv = AlphaEqEnv.empty): Boolean = {
    (this, b) match {
      case (ia: Identifier, ib: rc.Identifier) =>
        (ia.name == ib.name) || env.identifiers.get(ia.name).contains(ib)
      case (Lambda(xa, ta, ea), rc.Lambda(xb, eb)) =>
        typeEquals(ta, xb.t, env) && ea.represents(eb, env withIdent (xa.name -> xb))
      case (App(fa, ea), rc.App(fb, eb)) => fa.represents(fb, env) && ea.represents(eb, env)
      case (DepLambda(xa, ea), rc.DepLambda(xb, eb)) => ea.represents(eb, env withTIdent (xa -> xb))
      case (DepApp(fa, xa), rc.DepApp(fb, xb)) => (xa == xb) && fa.represents(fb, env)
      case (Literal(da), rc.Literal(db)) => da == db
      case (Primitive(pa), pb: rc.Primitive) =>
        typeEquals(pa.t, pb.t, env) && pa == pb.setType(TypePlaceholder)
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

  def substituteIdentRec(x: Identifier, v: ExprSet,
                         freeIdents: mutable.Map[ExprSet, Set[String]],
                         visited: mutable.Map[ExprSet, ExprSet]): Either[ExprSet, Expr] = {
    this match {
      case ia: Identifier =>
        if (ia.name == x.name) {
          Left(v)
        } else {
          Right(ia)
        }
      case Lambda(xa, t, ea) =>
        Right(Lambda(xa, t, ea.substituteIdentRec(x, v, freeIdents, visited)))
      case App(fa, ea) =>
        Right(App(fa.substituteIdentRec(x, v, freeIdents, visited),
          ea.substituteIdentRec(x, v, freeIdents, visited)))
      case DepLambda(xa: NatIdentifier, ea) =>
        Right(DepLambda[NatKind](xa, ea.substituteIdentRec(x, v, freeIdents, visited)))
      case DepLambda(xa: DataTypeIdentifier, ea) =>
        Right(DepLambda[DataKind](xa, ea.substituteIdentRec(x, v, freeIdents, visited)))
      case DepLambda(xa: AddressSpaceIdentifier, ea) =>
        Right(DepLambda[AddressSpaceKind](xa, ea.substituteIdentRec(x, v, freeIdents, visited)))
      case DepLambda(_, ea) => ???
      case DepApp(fa, xa) => Right(DepApp(fa.substituteIdentRec(x, v, freeIdents, visited), xa))
      case Literal(_) => Right(this)
      case Primitive(_) => Right(this)
    }
  }

  def substituteTypeIdent(x: Kind#I, v: Kind#T,
                          visited: mutable.Map[ExprSet, ExprSet]): Expr = {
    this match {
      case ia: Identifier => ia
      case Lambda(xa, t, ea) =>
        Lambda(xa, rc.substitute.kindInType(v, x, t), ea.substituteTypeIdent(x, v, visited))
      case App(fa, ea) =>
        App(fa.substituteTypeIdent(x, v, visited),
          ea.substituteTypeIdent(x, v, visited))
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

  // returns (merge_success, set modified)
  def merge(other: Expr, visited: mutable.Set[ExprSet]): (Boolean, Boolean) = {
    (this, other) match {
      case (ia: Identifier, ib: Identifier) if ia.name == ib.name => (true, false)
      case (Lambda(xa, ta, ea), Lambda(xb, tb, eb)) if ta == tb =>
        (true, ea.add(eb.substituteIdent(xb, ExprSet.one(xa, ta)), visited))
      case (App(fa, ea), App(fb, eb)) if ea.t == eb.t =>
        // implies fa.t == fb.t
        (true, fa.add(fb, visited) | ea.add(eb, visited))
      case (DepLambda(xa: NatIdentifier, ea), DepLambda(xb: NatIdentifier, eb)) =>
        (true, ea.add(eb.substituteTypeIdent[NatKind](xb, xa), visited))
      case (DepLambda(xa: DataTypeIdentifier, ea), DepLambda(xb: DataTypeIdentifier, eb)) =>
        (true, ea.add(eb.substituteTypeIdent[DataKind](xb, xa), visited))
      case (DepLambda(xa: AddressSpaceIdentifier, ea), DepLambda(xb: AddressSpaceIdentifier, eb)) =>
        (true, ea.add(eb.substituteTypeIdent[AddressSpaceKind](xb, xa), visited))
      case (DepApp(fa, xa), DepApp(fb, xb)) if (xa == xb) =>
        (true, fa.add(fb, visited))
      case (Literal(da), Literal(db)) if da == db => (true, false)
      case (Primitive(pa), Primitive(pb)) if pa == pb =>
        (true, false)
      case _ => (false, false)
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

  def generateDotRec(pid: String, i: Int,
                     visitedES: mutable.Map[ExprSet, String],
                     visitedE: mutable.Map[Expr, String]): String = {
    if (visitedE.contains(this)) {
      return s"$pid -> ${visitedE(this)}\n"
    }
    val id = s"${pid}_$i"
    visitedE += this -> id
    s"$pid -> $id\n" + (this match {
      case Identifier(name) =>
        s"$id [fillcolor=white, label=$name]"
      case Lambda(Identifier(name), _, e) =>
        s"""$id [fillcolor=white, label=λ$name]
           |${e.generateDotRec(visitedES, visitedE)}
           |$id -> ${visitedES(e)}
           |""".stripMargin
      case App(f, e) =>
        s"""$id [fillcolor=white, label=app]
           |${f.generateDotRec(visitedES, visitedE)}
           |${e.generateDotRec(visitedES, visitedE)}
           |$id -> ${visitedES(f)} [label=fun]
           |$id -> ${visitedES(e)} [label=arg]
           |""".stripMargin
      case DepLambda(x, e) =>
        s"""$id [fillcolor=white, label=Λ${x.name}]
           |${e.generateDotRec(visitedES, visitedE)}
           |$id -> ${visitedES(e)}
           |""".stripMargin
      case DepApp(f, v) =>
        s"""$id [fillcolor=white, label=<depApp $v>]
           |${f.generateDotRec(visitedES, visitedE)}
           |$id -> ${visitedES(f)}
           |""".stripMargin
      case Literal(d) =>
        s"$id [fillcolor=white, label=<$d>]"
      case Primitive(p) =>
        s"$id [fillcolor=${"\"#e6e2de\""}, label=${p.toString.trim}]"
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
        (dta == dtb) || env.typeIdentifiers.get(dta).contains(dtb)
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