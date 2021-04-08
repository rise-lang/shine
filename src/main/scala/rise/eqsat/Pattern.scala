package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}
import rise.core.{types => rct}

import scala.language.implicitConversions

object Pattern {
  def fromExpr(e: Expr): Pattern = {
    val pnode = e.node.map(fromExpr, NatPattern.fromNat, DataTypePattern.fromDataType)
    Pattern(Left(pnode), TypePattern.fromType(e.t))
  }

  implicit def patternToApplier[D](pat: Pattern): Applier[D] = new Applier[D] {
    override def patternVars(): Vec[PatternVar] = pat.patternVars()

    override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
      def rec(pat: Pattern): EClassId = {
        pat.node match {
          case Right(w) => subst(w)
          case Left(n) =>
            val enode = n.map(e => rec(e), { n =>
              ??? : Nat//ArithExpr.substitute(n, subst.nats.vec.toMap)
            }, dt => ??? : DataType)
            egraph.add(enode, ???)
        }
      }

      Vec(rec(pat))
    }
  }
}

/** A variable used in [[Pattern]]s or [[Subst]]s */
case class PatternVar(index: Int)

/** A pattern is a Rise expression which can contain pattern variables.
  * It can be used both as a [[Searcher]] or [[Applier]] for rewriting.
  * Searching for a pattern yields variable substitutions for each match in the [[EGraph]].
  * Applying a pattern performs a given variable substitution and adds the result to the [[EGraph]].
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.Pattern.html]]
  */
case class Pattern(node: Either[PNode, PatternVar], t: TypePattern) {
  def patternVars(): Vec[PatternVar] = {
    val vec = Vec.empty[PatternVar]
    def rec(n: Either[PNode, PatternVar]): Unit = {
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
  implicit def pickA2[A, B, C](p: Pick[Pick[A, B], C]): A = p.a
  implicit def pickB2[A, B, C](p: Pick[Pick[A, B], C]): B = p.a

  def ?(index: Int): PatternVar = PatternVar(index)
  def ?(index: Int, t: TypePattern): Pattern = Pattern(Right(PatternVar(index)), t)
  def %(index: Int): Var = Var(index)
  def %(index: Int, t: TypePattern): Pattern = Pattern(Left(Var(index)), t)

  def app(a: Pattern, b: Pattern): PNode = App(a, b)
  def lam(e: Pattern): PNode = Lambda(e)
  def nApp(f: Pattern, x: NatPattern): PNode = NatApp(f, x)
  def nLam(e: Pattern): PNode = NatLambda(e)
  def l(d: semantics.Data): Pattern = Pattern(Left(Literal(d)), ???)

  def prim(p: rise.core.Primitive): PNode = Primitive(p)
  def slide: PNode = prim(rcp.slide.primitive)
  def map: PNode = prim(rcp.map.primitive)
  def reduce: PNode = prim(rcp.reduce.primitive)
  def transpose: PNode = prim(rcp.transpose.primitive)
  def zip: PNode = prim(rcp.zip.primitive)
  def join: PNode = prim(rcp.join.primitive)
  def fst: PNode = prim(rcp.fst.primitive)
  def snd: PNode = prim(rcp.snd.primitive)
  def add: PNode = prim(rcp.add.primitive)
  def mul: PNode = prim(rcp.mul.primitive)
  def div: PNode = prim(rcp.div.primitive)
  def drop: PNode = prim(rcp.drop.primitive)
  def take: PNode = prim(rcp.take.primitive)

  implicit final class WithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(n: PNode): Pattern = Pattern(Left(n), t)
  }

  def `?n`(index: Int): NatPattern = NatPatternVar(index)
  def `%n`(index: Int): NatPattern = NatPatternNode(NatVar(index))

  def cst(value: Long): NatPattern = NatPatternNode(NatCst(value))

  def `?t`(index: Int): TypePattern = TypePatternVar(index)
  def `?dt`(index: Int): DataTypePattern = DataTypePatternVar(index)
  def `%dt`(index: Int): DataTypePattern = DataTypePatternNode(DataTypeVar(index))

  val int: DataTypePattern = DataTypePatternNode(ScalarType(rct.int))
  val f32: DataTypePattern = DataTypePatternNode(ScalarType(rct.f32))

  def nFunT(t: TypePattern): TypePattern = TypePatternNode(NatFunType(t))
  implicit final class FunConstructorT(private val r: TypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern = TypePatternNode(FunType(t, r))
  }
  implicit final class FunConstructorDT(private val r: DataTypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern = TypePatternNode(FunType(t, r: TypePattern))
  }
  implicit final class ArrayConstructor(private val s: NatPattern) extends AnyVal {
    @inline def `.`(et: DataTypePattern): DataTypePattern = DataTypePatternNode(ArrayType(s, et))
  }
}