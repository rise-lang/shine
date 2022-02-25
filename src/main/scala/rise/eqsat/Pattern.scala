package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}
import rise.core.{types => rct}
import rise.core.types.{DataType => rcdt}

import scala.language.implicitConversions

object Pattern {
  def fromExpr(e: Expr): Pattern = {
    val pnode = e.node.map(fromExpr, NatPattern.fromNat, DataTypePattern.fromDataType, AddressPattern.fromAddress)
    Pattern(PatternNode(pnode), TypePattern.fromType(e.t))
  }

  implicit def patternToApplier(pattern: Pattern): Applier = new Applier {
    override def toString: String = pattern.toString

    override def patternVars(): Set[Any] = pattern.patternVars()

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(), Set())

    override def applyOne(egraph: EGraph,
                          eclass: EClassId,
                          shc: Substs)(
                          subst: shc.Subst): Vec[EClassId] = {
      def missingRhsTy[T](): T = throw new Exception("unknown type on right-hand side")
      def pat(p: Pattern): EClassId = {
        p.p match {
          case w: PatternVar => shc.get(w, subst)
          case PatternNode(n) =>
            val enode = n.map(pat, nat, data, addr)
            egraph.add(enode, `type`(p.t))
        }
      }
      def nat(p: NatPattern): NatId = {
        p match {
          case w: NatPatternVar => shc.get(w, subst)
          case NatPatternNode(n) => egraph.add(n.map(nat))
          case NatPatternAny => missingRhsTy()
        }
      }
      def data(pat: DataTypePattern): DataTypeId = {
        pat match {
          case w: DataTypePatternVar => shc.get(w, subst)
          case DataTypePatternNode(n) => egraph.add(n.map(nat, data))
          case DataTypePatternAny => missingRhsTy()
        }
      }
      def `type`(pat: TypePattern): TypeId = {
        pat match {
          case w: TypePatternVar => shc.get(w, subst)
          case TypePatternNode(n) => egraph.add(n.map(`type`, nat, data))
          case TypePatternAny => missingRhsTy()
          case dtp: DataTypePattern => data(dtp)
        }
      }
      def addr(pat: AddressPattern): Address = {
        pat match {
          case w: AddressPatternVar => shc.get(w, subst)
          case AddressPatternNode(n) => n
          case AddressPatternAny => missingRhsTy()
        }
      }

      Vec(pat(pattern))
    }
  }
}

sealed trait PatternVarOrNode
/** A variable used in [[Pattern]]s or [[Subst]]s */
case class PatternVar(index: Int) extends PatternVarOrNode {
  override def toString: String = s"?$index"
}
case class PatternNode(node: PNode) extends PatternVarOrNode {
  override def toString: String = node.toString
}

/** A pattern is a Rise expression which can contain pattern variables.
  * It can be used both as a [[Searcher]] or [[Applier]] for rewriting.
  * Searching for a pattern yields variable substitutions for each match in the [[EGraph]].
  * Applying a pattern performs a given variable substitution and adds the result to the [[EGraph]].
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.Pattern.html]]
  */
case class Pattern(p: PatternVarOrNode, t: TypePattern) {
  override def toString: String = s"($p : $t)"

  def patternVars(): Set[Any] = {
    (this.p match {
      case PatternNode(n) =>
        Node.collect(n.map(_.patternVars(), _.patternVars(), _.patternVars(), _.patternVars())).flatten.toSet
      case pv: PatternVar => Set(pv)
    }) ++ this.t.patternVars()
  }

  def compile(): CompiledPattern =
    CompiledPattern(this, ematching.Program.compileFromPattern(this))
}

/** A [[Pattern]] which has been compiled to an [[ematching.Program]] */
case class CompiledPattern(pat: Pattern, prog: ematching.Program) {
  override def toString: String = s"CompiledPattern($pat)"
}

object CompiledPattern {
  implicit def patternToSearcher(cpat: CompiledPattern)
  : Searcher = new Searcher {
    override def toString: String = cpat.toString

    override def patternVars(): Set[Any] = cpat.pat.patternVars()

    override def search(egraph: EGraph,
                        shc: Substs,
                       ): Vec[SearchMatches[shc.Subst]] = {
      cpat.pat.p match {
        case PatternNode(node) =>
          egraph.classesByMatch.get(node.matchHash()) match {
            case None => Vec.empty
            case Some(ids) =>
              ids.iterator.flatMap(id => searchEClass(egraph, shc, id)).to(Vec)
          }
        case PatternVar(_) => egraph.classes.keysIterator
          .flatMap(id => searchEClass(egraph, shc, id)).to(Vec)
      }
    }

    override def searchEClass(egraph: EGraph,
                              shc: Substs,
                              eclass: EClassId,
                             ): Option[SearchMatches[shc.Subst]] = {
      val substs = cpat.prog.run(egraph, eclass, shc)
      if (substs.isEmpty) { None } else { Some(SearchMatches(eclass, substs)) }
    }
  }

  implicit def patternToApplier(cpat: CompiledPattern): Applier =
    Pattern.patternToApplier(cpat.pat)
}

object PatternDSL {
  import scala.language.implicitConversions

  implicit final class RewriteArrowPattern(private val lhs: Pattern) extends AnyVal {
    @inline def -->(rhs: Pattern): (Pattern, Pattern) = lhs -> rhs
  }
  implicit final class RewriteArrowCPattern(private val lhs: CompiledPattern) extends AnyVal {
    @inline def -->(rhs: Pattern): (Searcher, Applier) =
      (lhs: Searcher) -> rhs
    @inline def -->(rhs: Applier): (Searcher, Applier) =
      (lhs: Searcher) -> rhs
  }
  implicit final class RewriteArrowPNode(private val lhs: PNode) extends AnyVal {
    @inline def -->(rhs: Pattern): (Pattern, Pattern) = (lhs: Pattern) -> rhs
  }

  def ?(index: Int): PatternVar = PatternVar(index)
  def %(index: Int): Var = Var(index)

  def app(a: Pattern, b: Pattern): PNode = App(a, b)
  def lam(e: Pattern): PNode = Lambda(e)
  def nApp(f: Pattern, x: NatPattern): PNode = NatApp(f, x)
  def nLam(e: Pattern): PNode = NatLambda(e)
  def l(d: semantics.Data): PNode = Literal(d)

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

  implicit def pvarWithoutType(v: PatternVar): Pattern =
    Pattern(v, TypePatternAny)
  implicit def pnodeWithoutType(n: PNode): Pattern =
    Pattern(PatternNode(n), TypePatternAny)
  implicit final class PVarWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(v: PatternVar): Pattern = Pattern(v, t)
  }
  implicit final class PNodeWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(n: PNode): Pattern = Pattern(PatternNode(n), t)
  }
  implicit final class PatternComposition(private val f: Pattern) extends AnyVal {
    @inline def >>(g: Pattern): Pattern = Pattern(PatternNode(Composition(f, g)), TypePatternAny)
  }
  implicit final class PVarComposition(private val f: PatternVar) extends AnyVal {
    @inline def >>(g: Pattern): Pattern = (f: Pattern) >> g
  }
  implicit final class PNodeComposition(private val f: PNode) extends AnyVal {
    @inline def >>(g: Pattern): Pattern = (f: Pattern) >> g
  }

  def `?n`(index: Int): NatPatternVar = NatPatternVar(index)
  val `?n`: NatPattern = NatPatternAny
  def `%n`(index: Int): NatPattern = NatPatternNode(NatVar(index))

  def cst(value: Long): NatPattern = NatPatternNode(NatCst(value))

  // not supported in all cases, so no DSL for it
  // def `?t`(index: Int): TypePattern = TypePatternVar(index)
  val `?t`: TypePattern = TypePatternAny
  def `?dt`(index: Int): DataTypePattern = DataTypePatternVar(index)
  val `?dt`: DataTypePattern = DataTypePatternAny
  def `%dt`(index: Int): DataTypePattern = DataTypePatternNode(DataTypeVar(index))

  val int: DataTypePattern = DataTypePatternNode(ScalarType(rcdt.int))
  val f32: DataTypePattern = DataTypePatternNode(ScalarType(rcdt.f32))

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