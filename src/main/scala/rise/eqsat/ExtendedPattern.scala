package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}
import rise.core.{types => rct}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.language.implicitConversions
import scala.xml.NodeSeq

object ExtendedPattern {
  type PNode = Node[ExtendedPattern, NatPattern, DataTypePattern]

  def fromExpr(e: Expr): ExtendedPattern = {
    val pnode = e.node.map(fromExpr, NatPattern.fromNat, DataTypePattern.fromDataType)
    ExtendedPatternNode(pnode, TypePattern.fromType(e.t))
  }

  /* shares type hash-conses between the old and new e-graph */
  def matchesToGraph[ED, ND, TD](matches: Vec[ExtendedPatternMatch],
                                 egraph: EGraph[ED, ND, TD],
                                 analysis: Analysis[ED, ND, TD]): (EGraph[ED, ND, TD], EClassId) = {
    val newEGraph = EGraph.emptyWithAnalysis(analysis)
    newEGraph.hashConses = egraph.hashConses

    val oldToNew = HashMap.empty[EClassId, EClassId]

    val newUnion: (EClassId, EClassId) => EClassId = { case (a, b) =>
      newEGraph.union(a, b)._1
    }

    def addEClass(id: EClassId): EClassId = {
      oldToNew.get(id) match {
        case Some(nid) => nid
        case None =>
          val eclass = egraph.get(id)
          val newId = newEGraph.makeEmptyEClass(eclass.t)
          oldToNew(id) = newId
          eclass.nodes.map { n =>
            newEGraph.add(n.mapChildren(addEClass), eclass.t)
          }.foldLeft(newId)(newUnion)
      }
    }

    def addMatch(m: ExtendedPatternMatch): EClassId = m match {
      case ExtendedPatternMatch.EClass(id) => addEClass(id)
      case ExtendedPatternMatch.Product(node, t) =>
        def rec(remaining: Seq[Vec[ExtendedPatternMatch]], acc: Seq[ExtendedPatternMatch]): EClassId =
          remaining match {
            case Nil =>
              var i = -1
              newEGraph.add(node.mapChildren {_ =>
                i += 1
                addMatch(acc(i))
              }, t)
            case matches +: rest =>
              matches.map(m => rec(rest, acc :+ m))
                .reduce(newUnion)
          }
        rec(node.children().toSeq, Seq())
      case ExtendedPatternMatch.TraversalProduct(node, traverse, t) =>
        traverse.indices.flatMap { i =>
          traverse(i).iterator.map { m =>
            var j = -1
            val newNode = node.mapChildren { c =>
              j += 1
              if (i == j) {
                addMatch(m)
              } else {
                addEClass(c)
              }
            }
            newEGraph.add(newNode, t)
          }
        }.reduce(newUnion)
    }

    (newEGraph, matches.map(addMatch).reduce(newUnion))
  }
}

sealed trait ExtendedPatternMatch
object ExtendedPatternMatch {
  type MNode[T] = Node[T, NatId, DataTypeId]

  /** The entire e-class is a match */
  case class EClass(id: EClassId) extends ExtendedPatternMatch

  /** Each node child has multiple matches, creating a cartesian product of matches.
    *
    *   P(f({a, b}, {c, d})) = {f(a, c), f(a, d), f(b, c), f(b, d)}
    */
  case class Product(node: MNode[Vec[ExtendedPatternMatch]], t: TypeId) extends ExtendedPatternMatch

  /** Traversing each node child gives multiple matches.
    * For each possible traversal match from `traverse`,
    * the non-traversed children from `node` should remain the original e-classes.
    *
    *   TP(f(x, y), {{a, b}, {c, d}}) = {f(x, c), f(x, d), f(a, y), f(b, y)}
    */
  case class TraversalProduct(node: ENode, traverse: Vec[Vec[ExtendedPatternMatch]], t: TypeId) extends ExtendedPatternMatch
}

/** An extended pattern */
sealed trait ExtendedPattern {
  def searchEClass(egraph: EGraph[_, _, _], id: EClassId): Vec[ExtendedPatternMatch] = {
    searchEClass(egraph, egraph.get(id), Set())
  }

  // TODO: investigate similarities with e-matching
  private def searchEClass(egraph: EGraph[_, _, _], eclass: EClass[_], visited: Set[EClassId]): Vec[ExtendedPatternMatch] = {
    if (visited(eclass.id)) { // avoid cyclic expressions
      return Vec.empty
    }

    this match {
      case ExtendedPatternAny(t) =>
        if (typeIsMatch(egraph, t, eclass.t)) { Vec(ExtendedPatternMatch.EClass(eclass.id)) } else { Vec.empty }
      case ExtendedPatternVar(index, t) => ??? // TODO
      case ExtendedPatternNode(node, t) =>
        // egraph.classesByMatch.get(node.matchHash())
        val res = Vec.empty[ExtendedPatternMatch]
        if (typeIsMatch(egraph, t, eclass.t)) {
          ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => ()), matched => {
            val typesMatch =
              node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
              node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) }
            if (typesMatch) {
              val childrenMatches = node.children().zip(matched.children()).map { case (p, id) =>
                p.searchEClass(egraph, egraph.get(id), visited + eclass.id)
              }.to(Vec)
              if (childrenMatches.forall(_.nonEmpty)) {
                var i = -1
                res += ExtendedPatternMatch.Product(matched.mapChildren { _ =>
                  i += 1
                  childrenMatches(i)
                }, eclass.t)
              }
            }
          })
        }
        res
      case ExtendedPatternContains(contained) =>
        contained.searchEClass(egraph, eclass, visited) ++ eclass.nodes.iterator.flatMap { n =>
          val childrenMatches = n.children()
            .map(c => this.searchEClass(egraph, egraph.get(c), visited + eclass.id)).to(Vec)
          if (childrenMatches.exists(_.nonEmpty)) {
            Some(ExtendedPatternMatch.TraversalProduct(n, childrenMatches, eclass.t))
          } else {
            None
          }
        }
    }
  }

  private def typeIsMatch(egraph: EGraph[_, _, _], p: TypePattern, t: TypeId): Boolean = {
    p match {
      case TypePatternNode(pn) => typeNodeIsMatch(egraph, pn, t)
      case TypePatternVar(index) => ??? // TODO
      case TypePatternAny => true
      case dtp: DataTypePattern =>
        t match {
          case dt: DataTypeId => dataTypeIsMatch(egraph, dtp, dt)
          case _: NotDataTypeId => false
        }
    }
  }
  private def dataTypeIsMatch(egraph: EGraph[_, _, _], p: DataTypePattern, t: DataTypeId): Boolean = {
    p match {
      case DataTypePatternNode(pn) => typeNodeIsMatch(egraph, pn, t)
      case DataTypePatternVar(index) => ??? // TODO
      case DataTypePatternAny => true
    }
  }
  private def typeNodeIsMatch(egraph: EGraph[_, _, _],
                              pn: TypeNode[TypePattern, NatPattern, DataTypePattern],
                              t: TypeId): Boolean = {
    val n = egraph(t)._1
    (pn.map(_ => (), _ => (), _ => ()) == n.map(_ => (), _ => (), _ => ())) && {
      val ns = Vec.empty[NatId]
      val dts = Vec.empty[DataTypeId]
      val ts = Vec.empty[TypeId]
      n.map(
        t => ts += t,
        n => ns += n,
        dt => dts += dt,
      )
      val pns = Vec.empty[NatPattern]
      val pdts = Vec.empty[DataTypePattern]
      val pts = Vec.empty[TypePattern]
      pn.map(
        pt => pts += pt,
        pn => pns += pn,
        pdt => pdts += pdt,
      )
      ns.zip(pns).forall { case (n, pn) => natIsMatch(egraph, pn, n) } &&
      dts.zip(pdts).forall { case (dt, pdt) => dataTypeIsMatch(egraph, pdt, dt) } &&
      ts.zip(pts).forall { case (t, pt) => typeIsMatch(egraph, pt, t) }
    }
  }
  private def natIsMatch(egraph: EGraph[_, _, _], p: NatPattern, t: NatId): Boolean = {
    p match {
      case NatPatternNode(pn) =>
        val n = egraph(t)._1
        (pn.map(_ => ()) == n.map(_ => ())) && {
          val ns = Vec.empty[NatId]
          n.map(n => ns += n)
          val pns = Vec.empty[NatPattern]
          pn.map(pn => pns += pn)
          ns.zip(pns).forall { case (n, pn) => natIsMatch(egraph, pn, n) }
        }
      case NatPatternVar(index) => ??? // TODO
      case NatPatternAny => true
    }
  }
}
case class ExtendedPatternAny(t: TypePattern) extends ExtendedPattern {
  override def toString: String = s"(? : $t)"
}
case class ExtendedPatternVar(index: Int, t: TypePattern) extends ExtendedPattern {
  override def toString: String = s"(?$index : $t)"
}
case class ExtendedPatternNode(node: ExtendedPattern.PNode, t: TypePattern) extends ExtendedPattern {
  override def toString: String = s"(${node.toString} : $t)"
}
case class ExtendedPatternContains(contained: ExtendedPattern) extends ExtendedPattern {
  override def toString: String = s"contains($contained)"
}

object ExtendedPatternDSL {
  import scala.language.implicitConversions

  def contains(p: ExtendedPattern): ExtendedPattern =
    ExtendedPatternContains(p)

  case object ?
  case class ?(index: Int)
  def %(index: Int): Var = Var(index)

  def app(a: ExtendedPattern, b: ExtendedPattern): ExtendedPattern.PNode = App(a, b)
  def lam(e: ExtendedPattern): ExtendedPattern.PNode = Lambda(e)
  def nApp(f: ExtendedPattern, x: NatPattern): ExtendedPattern.PNode = NatApp(f, x)
  def nLam(e: ExtendedPattern): ExtendedPattern.PNode = NatLambda(e)
  def l(d: semantics.Data): ExtendedPattern.PNode = Literal(d)

  def prim(p: rise.core.Primitive): ExtendedPattern.PNode = Primitive(p)
  def slide: ExtendedPattern.PNode = prim(rcp.slide.primitive)
  def map: ExtendedPattern.PNode = prim(rcp.map.primitive)
  def reduce: ExtendedPattern.PNode = prim(rcp.reduce.primitive)
  def reduceSeq: ExtendedPattern.PNode = prim(rcp.reduceSeq.primitive)
  def transpose: ExtendedPattern.PNode = prim(rcp.transpose.primitive)
  def zip: ExtendedPattern.PNode = prim(rcp.zip.primitive)
  def join: ExtendedPattern.PNode = prim(rcp.join.primitive)
  def fst: ExtendedPattern.PNode = prim(rcp.fst.primitive)
  def snd: ExtendedPattern.PNode = prim(rcp.snd.primitive)
  def add: ExtendedPattern.PNode = prim(rcp.add.primitive)
  def mul: ExtendedPattern.PNode = prim(rcp.mul.primitive)
  def div: ExtendedPattern.PNode = prim(rcp.div.primitive)
  def drop: ExtendedPattern.PNode = prim(rcp.drop.primitive)
  def take: ExtendedPattern.PNode = prim(rcp.take.primitive)

  implicit def panyWithoutType(v: ?.type): ExtendedPatternAny =
    ExtendedPatternAny(TypePatternAny)
  implicit def pvarWithoutType(v: ?): ExtendedPatternVar =
    ExtendedPatternVar(v.index, TypePatternAny)
  implicit def pnodeWithoutType(n: ExtendedPattern.PNode): ExtendedPattern =
    ExtendedPatternNode(n, TypePatternAny)
  implicit final class PAnyWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(v: ?.type): ExtendedPattern = ExtendedPatternAny(t)
  }
  implicit final class PVarWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(v: ?): ExtendedPattern = ExtendedPatternVar(v.index, t)
  }
  implicit final class PNodeWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(n: ExtendedPattern.PNode): ExtendedPattern = ExtendedPatternNode(n, t)
  }

  // FIXME: redundancy with the regular PatternDSL

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