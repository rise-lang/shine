package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}

import scala.language.implicitConversions

object Pattern {
  def fromExpr(e: Expr): Pattern =
    Pattern(Left(e.node.mapChildren(fromExpr)))

  implicit def patternToApplier[D](pat: Pattern): Applier[D] = new Applier[D] {
    override def patternVars(): Vec[PatternVar] = pat.patternVars()

    override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
      def rec(pat: Pattern): EClassId = {
        pat.node match {
          case Right(w) => subst(w)
          case Left(n) =>
            val enode = n.mapChildren(rec)
            egraph.add(enode)
        }
      }

      Vec(rec(pat))
    }
  }
}

// TODO? interned string in egg
/** A variable used in [[Pattern]]s or [[Subst]]s */
case class PatternVar(name: String)

/** A pattern is a Rise expression which can contain pattern variables.
  * It can be used both as a [[Searcher]] or [[Applier]] for rewriting.
  * Searching for a pattern yields variable substitutions for each match in the [[EGraph]].
  * Applying a pattern performs a given variable substitution and adds the result to the [[EGraph]].
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.Pattern.html]]
  */
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

/** A [[Pattern]] which has been compiled to an [[ematching.Program]] */
case class CompiledPattern(pat: Pattern, prog: ematching.Program)

object CompiledPattern {
  implicit def patternToSearcher[D](cpat: CompiledPattern): Searcher[D] = new Searcher[D] {
    override def patternVars(): Vec[PatternVar] = cpat.pat.patternVars()

    override def search(egraph: EGraph[D]): Vec[SearchMatches] = {
      cpat.pat.node match {
        case Left(node) =>
          egraph.classesByMatch.get(node.matchHash()) match {
            case None => Vec.empty
            case Some(ids) =>
              ids.iterator.flatMap(id => searchEClass(egraph, id)).to(Vec)
          }
        case Right(_) => egraph.classes.keysIterator
          .flatMap(id => searchEClass(egraph, id)).to(Vec)
      }
    }

    override def searchEClass(egraph: EGraph[D], eclass: EClassId): Option[SearchMatches] = {
      val substs = cpat.prog.run(egraph, eclass)
      if (substs.isEmpty) { None } else { Some(SearchMatches(eclass, substs)) }
    }
  }

  implicit def patternToApplier[D](cpat: CompiledPattern): Applier[D] =
    Pattern.patternToApplier(cpat.pat)
}

object PatternDSL {
  import scala.language.implicitConversions

  case class Pick[A, B](a: A, b: B)
  implicit def pickA[A, B](p: Pick[A, B]): A = p.a
  implicit def pickB[A, B](p: Pick[A, B]): B = p.b

  def ?(name: String): Pick[PatternVar, Pattern] =
    Pick(PatternVar(name), Pattern(Right(PatternVar(name))))
  def %(index: Int): Pick[Var, Pattern] = Pick(Var(index), Pattern(Left(Var(index))))
  def app(a: Pattern, b: Pattern): Pattern = Pattern(Left(App(a, b)))
  def lam(e: Pattern): Pattern = Pattern(Left(Lambda(e)))
  def l(d: semantics.Data): Pattern = Pattern(Left(Literal(d)))

  def prim(p: rise.core.Primitive): Pattern = Pattern(Left(Primitive(p)))
  def slide: Pattern = prim(rcp.slide.primitive)
  def map: Pattern = prim(rcp.map.primitive)
  def reduce: Pattern = prim(rcp.reduce.primitive)
  def transpose: Pattern = prim(rcp.transpose.primitive)
  def zip: Pattern = prim(rcp.zip.primitive)
  def join: Pattern = prim(rcp.join.primitive)
  def fst: Pattern = prim(rcp.fst.primitive)
  def snd: Pattern = prim(rcp.snd.primitive)
  def add: Pattern = prim(rcp.add.primitive)
  def mul: Pattern = prim(rcp.mul.primitive)
  def div: Pattern = prim(rcp.div.primitive)
}
