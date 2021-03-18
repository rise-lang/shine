package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}

object Pattern {
  def fromExpr(e: Expr): Pattern =
    Pattern(Left(e.node.mapChildren(fromExpr)))
}

case class PatternVar(name: String)
case class Pattern(node: Either[Node[Pattern], PatternVar]) {
  def patternVars(): Vec[PatternVar] = {
    val vec = Vec.empty[PatternVar]
    def rec(n: Either[Node[Pattern], PatternVar]): Unit = {
      n match {
        case Left(node) => node.children().foreach { child =>
          rec(child.node)
        }
        case Right(pv) =>
          if (!vec.contains(pv)) { vec += pv }
      }
    }
    rec(node)
    vec
  }

  def compile(): CompiledPattern =
    CompiledPattern(this, ematching.Program.compileFromPattern(this))
}

case class CompiledPattern(pat: Pattern, prog: ematching.Program)

object CompiledPattern {
  import scala.language.implicitConversions

  implicit def patternToSearcher[D](cpat: CompiledPattern): Searcher[D] = new Searcher[D] {
    override def patternVars(): Vec[PatternVar] = cpat.pat.patternVars()

    override def search(egraph: EGraph[D]): Vec[SearchMatches] = {
      cpat.pat.node match {
        case Left(node) =>
          egraph.classesByMatch.get(node.matchHash()) match {
            case None => Vec.empty
            case Some(ids) =>
              ids.iterator.flatMap(id => searchOne(egraph, id)).to(Vec)
          }
        case Right(_) => egraph.classes.valuesIterator
          .flatMap(e => searchOne(egraph, e.id)).to(Vec)
      }
    }

    override def searchOne(egraph: EGraph[D], eclass: EClassId): Option[SearchMatches] = {
      val substs = cpat.prog.run(egraph, eclass)
      if (substs.isEmpty) { None } else { Some(SearchMatches(eclass, substs)) }
    }
  }

  implicit def patternToApplier[D](cpat: CompiledPattern): Applier[D] = new Applier[D] {
    override def patternVars(): Vec[PatternVar] = cpat.pat.patternVars()

    override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
      def rec(pat: Pattern): EClassId = {
        pat.node match {
          case Right(w) => subst(w)
          case Left(n) =>
            val enode = n.mapChildren(rec)
            egraph.add(enode)
        }
      }

      Vec(rec(cpat.pat))
    }

    // TODO? could override `apply` to avoid temporary vecs
  }
}

object PatternDSL {
  import scala.language.implicitConversions

  case class Pick[A, B](a: A, b: B)
  implicit def pickA[A, B](p: Pick[A, B]): A = p.a
  implicit def pickB[A, B](p: Pick[A, B]): B = p.b

  def ?(name: String): Pick[PatternVar, Pattern] = Pick(PatternVar(name), Pattern(Right(PatternVar(name))))
  def %(index: Int): Pick[Var, Pattern] = Pick(Var(index), Pattern(Left(Var(index))))
  def app(a: Pattern, b: Pattern): Pattern = Pattern(Left(App(a, b)))
  def lam(e: Pattern): Pattern = Pattern(Left(Lambda(e)))
  def map: Pattern = Pattern(Left(Primitive(rcp.map.primitive)))
  def add: Pattern = Pattern(Left(Primitive(rcp.add.primitive)))
  def mul: Pattern = Pattern(Left(Primitive(rcp.mul.primitive)))
  def div: Pattern = Pattern(Left(Primitive(rcp.div.primitive)))
  def l(d: semantics.Data): Pattern = Pattern(Left(Literal(d)))
}