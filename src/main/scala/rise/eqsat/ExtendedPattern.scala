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
  def matchesToGraph(matches: Vec[ExtendedPatternMatch],
                     egraph: EGraph,
                     analysis: Analysis): (EGraph, EClassId) = {
    val newEGraph = EGraph.empty()
    newEGraph.hashConses = egraph.hashConses

    sealed trait EClassStatus
    case object PartialEClass extends EClassStatus
    case object CompleteEClass extends EClassStatus

    val oldToNew = HashMap.empty[EClassId, (EClassId, EClassStatus)]
    // we rely on the fact that matches have been memoized during construction,
    // meaning that redundant matches are reference-equal
    val memo = new java.util.IdentityHashMap[ExtendedPatternMatch, EClassId]()

    val newUnion: (EClassId, EClassId) => EClassId = { case (a, b) =>
      newEGraph.union(a, b)._1
    }

    def addEClass(oldId: EClassId): EClassId = {
      val status = oldToNew.get(oldId)
      status match {
        case Some((nid, CompleteEClass)) => nid
        case _ =>
          val eclass = egraph.get(oldId)
          val newId = status match {
            case Some((_, CompleteEClass)) => throw new Exception("unreachable")
            case Some((nid, PartialEClass)) =>
              nid
            case None =>
              newEGraph.makeEmptyEClass(eclass.t)
          }
          oldToNew(oldId) = (newId, CompleteEClass)
          eclass.nodes.map { n =>
            newEGraph.add(n.mapChildren(addEClass), eclass.t)
          }.foldLeft(newId)(newUnion)
      }
    }

    // NOTE: this restores previously known equalities,
    // why is that a good idea?
    def addNode(n: ENode, oldId: EClassId): EClassId = {
      oldToNew.get(oldId) match {
        case Some((nid, CompleteEClass)) => nid // nothing to do
        case Some((nid, PartialEClass)) =>
          newUnion(newEGraph.add(n, egraph.get(oldId).t), nid)
        case None =>
          val id = newEGraph.add(n, egraph.get(oldId).t)
          oldToNew(oldId) = (id, PartialEClass)
          id
      }
    }

    def addMatch(m: ExtendedPatternMatch): EClassId = {
      memo.get(m) match {
        case null =>
        case value => return value
      }

      val res = m match {
        case ExtendedPatternMatch.EClass(id) => addEClass(id)
        case ExtendedPatternMatch.Product(node, id) =>
          def rec(remaining: Seq[Vec[ExtendedPatternMatch]], acc: Seq[ExtendedPatternMatch]): EClassId =
            remaining match {
              case Nil =>
                var i = -1
                addNode(node.mapChildren {_ =>
                  i += 1
                  addMatch(acc(i))
                }, id)
              case matches +: rest =>
                matches.map(m => rec(rest, acc :+ m))
                  .reduce(newUnion)
            }
          rec(node.children().toSeq, Seq())
        case ExtendedPatternMatch.TraversalProduct(node, traverse, id) =>
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
              addNode(newNode, id)
            }
          }.reduce(newUnion)
      }

      memo.put(m, res)

      res
    }

    (newEGraph, matches.map(addMatch).reduce(newUnion))
  }

  def beamSearch[Cost](pattern: ExtendedPattern,
                       beamSize: Int,
                       costFunction: CostFunction[Cost],
                       egraph: EGraph,
                       id: EClassId): Seq[(Cost, ExprWithHashCons)] = {
    beamSearch(pattern, beamSize, costFunction, egraph).getOrElse(id, Seq())
  }

  def beamSearch[Cost](pattern: ExtendedPattern,
                       beamSize: Int,
                       costFunction: CostFunction[Cost],
                       egraph: EGraph)
  : Map[EClassId, Seq[(Cost, ExprWithHashCons)]] =
  {
    val beamExtractMap = egraph.getAnalysisMap(BeamExtract2(beamSize, costFunction))
    // val beamAnalyser = util.printTime("beam extract",
    //  Analyser.init(egraph, BeamExtract(beamSize, costFunction)))
    val memo = HashMap.empty[ExtendedPattern, Map[EClassId, Seq[(Cost, ExprWithHashCons)]]]

    def searchRec(p: ExtendedPattern): Map[EClassId, Seq[(Cost, ExprWithHashCons)]] = {
      memo.get(p) match {
        case Some(value) => return value
        case None =>
      }

      val res: Map[EClassId, Seq[(Cost, ExprWithHashCons)]] = p match {
        case ExtendedPatternAny(t) =>
          beamExtractMap.iterator.filter { case (id, beam) =>
            beam.nonEmpty && typeIsMatch(egraph, t, egraph.get(id).t)
          }.toMap
        case ExtendedPatternVar(index, t) => ???
        case ExtendedPatternNode(node, t) =>
          val childrenMatches = node.children().map(searchRec).toSeq
          val resBeams = HashMap.empty[EClassId, Vec[(Cost, ExprWithHashCons)]]

          egraph.classesByMatch(node.matchHash()).foreach { id =>
            val eclass = egraph.get(id)

            if (typeIsMatch(egraph, t, eclass.t)) {
              ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => ()), { matched =>
                val typesMatch =
                  node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
                  node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) }
                if (typesMatch) {
                  traverse(
                    childrenMatches.iterator.zip(matched.children())
                  ) { case (matches, id) =>
                    matches.get(id)
                  } match {
                    case None => ()
                    case Some(childrenBeams) =>
                      val children = matched.children().toSeq

                      def productRec(i: Int,
                                     selected: Map[EClassId, (Cost, ExprWithHashCons)]): Unit = {
                        if (i < childrenBeams.size) {
                          childrenBeams(i).foreach { x =>
                            productRec(i + 1, selected + (children(i) -> x))
                          }
                        } else {
                          val beam = resBeams.getOrElseUpdate(id, Vec.empty)
                          beam += ((
                            costFunction.cost(matched, c => selected(c)._1),
                            ExprWithHashCons(matched.mapChildren(c => selected(c)._2), eclass.t)))
                        }
                      }

                      productRec(0, Map.empty)
                  }
                }
              })
            }

            resBeams.get(id).foreach { beam =>
              assert(beam.nonEmpty)
              beam.sortInPlaceBy(_._1)(costFunction.ordering).take(beamSize)
            }
          }

          resBeams.iterator.map { case (k, v) => k -> v.toSeq }.toMap
        case ExtendedPatternContains(contained) =>
          val containedMatches = searchRec(contained)
          // TODO: skip if empty contained?

          val initialData = egraph.classes.values.map { eclass =>
            eclass.id -> (containedMatches.get(eclass.id) match {
              case None => Nil
              case Some(beam) => beam
            })
          }.to(HashMap)

          val analyser = Analyser.init(egraph, new Analyser.Analysis[Seq[(Cost, ExprWithHashCons)]] {
            override def make(enode: ENode, t: TypeId,
                              analysisOf: EClassId => Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
              val childrenMatchingBeams = enode.children().map(c => (c, analysisOf(c))).toSeq
              // TODO: skip if empty matches
              val childrenAnyBeams = enode.children().map(c => (c, beamExtractMap(egraph.find(c)))).toSeq

              val tmp = childrenMatchingBeams.flatMap { case (matchingChild, matchingBeam) =>
                def productRec(remaining: Seq[(EClassId, Seq[(Cost, ExprWithHashCons)])],
                               selected: Map[EClassId, (Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
                  remaining match {
                    case Nil =>
                      Seq((
                        costFunction.cost(enode, c => selected(c)._1),
                        ExprWithHashCons(enode.mapChildren(c => selected(c)._2), t)))
                    case (child, childBeam) +: rest =>
                      childBeam.flatMap { x =>
                        productRec(rest, selected + (child -> x))
                      }
                  }
                }

                val productTodo = childrenAnyBeams.map { case (child, beam) =>
                  (child, if (child == matchingChild) {
                    matchingBeam
                  } else {
                    beam
                  })
                }
                productRec(productTodo, Map.empty)
              }.sortBy(_._1)(costFunction.ordering).take(beamSize)

              tmp.distinct // FIXME: why is .distinct necessary here?
            }

            override def merge(a: Seq[(Cost, ExprWithHashCons)], b: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
              val sorted = (a ++ b).sortBy(_._1)(costFunction.ordering)
              val dedup = sorted.distinct // FIXME: why is .distinct necessary here?
              dedup.take(beamSize)
            }

            override def update(existing: Seq[(Cost, ExprWithHashCons)], computed: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
              val sorted = (existing ++ computed).sortBy(_._1)(costFunction.ordering)
              // TODO: hash-cons the exprs for faster .distinct?
              val dedup = sorted.distinct
                // sorted.headOption ++ sorted.iterator.sliding(2, 1).withPartial(false)
                // .flatMap { nbh => if (nbh(0) == nbh(1)) { None } else { Some(nbh(1)) } }
              dedup.take(beamSize)
            }
          }, initialData)

          analyser.data.iterator.filter { case (_, beam) => beam.nonEmpty }.toMap
      }

      assert(res.values.forall(beam => beam == beam.sortBy(_._1)(costFunction.ordering).distinct))
      memo(p) = res
      res
    }

    searchRec(pattern)
  }

  private def typeIsMatch(egraph: EGraph, p: TypePattern, t: TypeId): Boolean = {
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
  private def dataTypeIsMatch(egraph: EGraph, p: DataTypePattern, t: DataTypeId): Boolean = {
    p match {
      case DataTypePatternNode(pn) => typeNodeIsMatch(egraph, pn, t)
      case DataTypePatternVar(index) => ??? // TODO
      case DataTypePatternAny => true
    }
  }
  private def typeNodeIsMatch(egraph: EGraph,
                              pn: TypeNode[TypePattern, NatPattern, DataTypePattern],
                              t: TypeId): Boolean = {
    val n = egraph(t)
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
  private def natIsMatch(egraph: EGraph, p: NatPattern, t: NatId): Boolean = {
    p match {
      case NatPatternNode(pn) =>
        val n = egraph(t)
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

  def traverse[A, B](iterator: Iterator[A])(f: A => Option[B]): Option[Vec[B]] = {
    val acc = Vec.empty[B]
    while (true) {
      iterator.nextOption() match {
        case Some(a) => f(a) match {
          case Some(b) => acc += b
          case None => return None
        }

        case None => return Some(acc)
      }
    }

    throw new Exception("unreachable")
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
  case class Product(node: MNode[Vec[ExtendedPatternMatch]], id: EClassId) extends ExtendedPatternMatch

  /** Traversing each node child gives multiple matches.
    * For each possible traversal match from `traverse`,
    * the non-traversed children from `node` should remain the original e-classes.
    *
    *   TP(f(x, y), {{a, b}, {c, d}}) = {f(x, c), f(x, d), f(a, y), f(b, y)}
    */
  case class TraversalProduct(node: ENode, traverse: Vec[Vec[ExtendedPatternMatch]], id: EClassId) extends ExtendedPatternMatch
}

/** An extended pattern */
sealed trait ExtendedPattern {
  import ExtendedPattern.{typeIsMatch, dataTypeIsMatch, natIsMatch, traverse}

  def searchEClass(egraph: EGraph, id: EClassId): Vec[ExtendedPatternMatch] = {
    searchEClass(egraph, egraph.get(id), Set(), HashMap.empty/*, None*/)
  }

  // TODO: investigate similarities with e-matching
  private def searchEClass(egraph: EGraph,
                           eclass: EClass,
                           visited: Set[EClassId],
                           memo: HashMap[(EClassId, ExtendedPattern), Vec[ExtendedPatternMatch]],
                           /*parent: Option[ENode]*/): Vec[ExtendedPatternMatch] = {
    memo.get((eclass.id, this)) match {
      case Some(value) => return value
      case None =>
    }
    if (visited(eclass.id)) { // avoid cyclic expressions
      return Vec.empty
    }

    /* val isFunctionOfApp: Boolean = parent.contains { p: ENode =>
      p.matches(App((), ())) && (p.children().next() == eclass.id)
    } */

    val res: Vec[ExtendedPatternMatch] = this match {
      case ExtendedPatternAny(t) =>
        if (typeIsMatch(egraph, t, eclass.t)) { Vec(ExtendedPatternMatch.EClass(eclass.id)) } else { Vec.empty }
      case ExtendedPatternVar(index, t) => ??? // TODO
      case ExtendedPatternNode(node, t) =>
        // if (isFunctionOfApp) { assert(!node.matches(Lambda(()))) } // avoid some non-BENF paths
        // egraph.classesByMatch.get(node.matchHash())
        val res = Vec.empty[ExtendedPatternMatch]
        if (typeIsMatch(egraph, t, eclass.t)) {
          ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => ()), matched => {
            val typesMatch =
              node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
              node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) }
            if (typesMatch) {
              val childrenMatches = traverse(node.children().zip(matched.children())) { case (p, id) =>
                val ms = p.searchEClass(egraph, egraph.get(id), visited + eclass.id, memo/*, Some(matched)*/)
                if (ms.isEmpty) { None } else { Some(ms) } // short circuit if no match
              }
              childrenMatches match {
                case None => ()
                case Some(cm) =>
                  var i = -1
                  res += ExtendedPatternMatch.Product(matched.mapChildren { _ =>
                    i += 1
                    cm(i)
                  }, eclass.id)
              }
            }
          })
        }
        res
      case ExtendedPatternContains(contained) =>
        contained.searchEClass(egraph, eclass, visited, memo/*, parent*/) ++ eclass.nodes.iterator.flatMap { n =>
          if (false) {//(isFunctionOfApp && n.matches(Lambda(()))) { // avoid some non-BENF paths
            None
          } else {
            val childrenMatches = n.children().map(c =>
              this.searchEClass(egraph, egraph.get(c), visited + eclass.id, memo/*, Some(n)*/)
            ).to(Vec)
            if (childrenMatches.exists(_.nonEmpty)) {
              Some(ExtendedPatternMatch.TraversalProduct(n, childrenMatches, eclass.id))
            } else {
              None
            }
          }
        }
    }

    memo((eclass.id, this)) = res

    res
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
  def mapSeq: ExtendedPattern.PNode = prim(rcp.mapSeq.primitive)
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