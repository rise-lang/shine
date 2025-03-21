package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}
import rise.core.types.{DataType => rcdt}

import scala.language.implicitConversions

object Sketch {
  type PNode = Node[Sketch, NatPattern, DataTypePattern, AddressPattern]

  def fromExpr(e: Expr): Sketch = {
    val pnode = e.node.map(fromExpr, NatPattern.fromNat, DataTypePattern.fromDataType, AddressPattern.fromAddress)
    SketchNode(pnode, TypePattern.fromType(e.t))
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

  def exists(pattern: Sketch,
             egraph: EGraph,
             id: EClassId): Boolean = {
    val ex = exists(pattern, egraph)(id)
    /* assert {
      ex == util.printTime("search", beamSearch(pattern, 1, AstSize, egraph, id).nonEmpty)
    } */
    ex
  }

  def exists(pattern: Sketch,
             egraph: EGraph): Set[EClassId] = {
    assert(egraph.clean)
    val memo = HashMap.empty[Sketch, Set[EClassId]]

    def searchRec(p: Sketch): Set[EClassId] = {
      memo.get(p) match {
        case Some(value) => return value
        case None =>
      }

      val res: Set[EClassId] = p match {
        case SketchAny(t) =>
          egraph.classes.valuesIterator.flatMap { eclass =>
            assert(eclass.id == egraph.find(eclass.id))
            if (typeIsMatch(egraph, t, eclass.t)) { Some(eclass.id) } else { None }
          }.toSet
        case SketchVar(index, t) => ???
        case SketchNode(node, t) =>
          val childrenMatches = node.children().map(searchRec).toSeq
          // TODO: skip if any child has empty matches?

          egraph.classesByMatch.getOrElse(node.matchHash(), HashSet.empty).filter { id =>
            val eclass = egraph.get(id)
            assert(id == eclass.id)
            var isMatch = false

            if (typeIsMatch(egraph, t, eclass.t)) {
              // TODO: could short-circuit
              ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => (), _ => ()), { matched =>
                val typesMatch =
                  node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
                  node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) } &&
                  node.addresses().zip(matched.addresses()).forall { case (p, a) => addressIsMatch(p, a) }
                if (typesMatch) {
                  val childrenMatch = childrenMatches.iterator.zip(matched.children())
                    .forall { case (matches, id) =>
                      assert(id == egraph.find(id))
                      matches(id)
                    }

                  if (childrenMatch) {
                    isMatch = true
                  }
                }
              })
            }

            isMatch
          }.toSet
        case SketchContains(contained) =>
          val containedMatches = searchRec(contained)
          // TODO: skip if empty contained?

          val dataMap = egraph.classes.values.map { eclass =>
            assert(eclass.id == egraph.find(eclass.id))
            eclass.id -> containedMatches(eclass.id)
          }.to(HashMap)

          SemiLatticeAnalysis.oneShot(new SemiLatticeAnalysis {
            type Data = Boolean

            override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())

            override def make(egraph: EGraph, enode: ENode, t: TypeId,
                              analysisOf: EClassId => Boolean): Boolean =
              enode.children().exists { c =>
                assert(c == egraph.find(c))
                analysisOf(c)
              }

            override def merge(a: Boolean, b: Boolean): MergeResult = {
              val r = a || b
              MergeResult(r, r != a, r != b)
            }
          }, egraph)(dataMap)

          dataMap.iterator.flatMap { case (id, isMatch) =>
            if (isMatch) { Some(id) } else { None }
          }.toSet
        case SketchOr(a, b) =>
          val aMatches = searchRec(a)
          val bMatches = searchRec(b)
          aMatches union bMatches
        case SketchAnd(a, b) => ???
          // following would be wrong:
          // searchRec(a) intersect searchRec(b)
        case SketchWithType(p, t) =>
          searchRec(p).filter(id => typeIsMatch(egraph, t, egraph.get(id).t))
      }

      memo(p) = res
      res
    }

    val r = searchRec(pattern)
    r
  }

  def beamSearch[Cost](pattern: Sketch,
                       beamSize: Int,
                       costFunction: CostFunction[Cost],
                       egraph: EGraph,
                       id: EClassId): Seq[(Cost, ExprWithHashCons)] = {
    beamSearch(pattern, beamSize, costFunction, egraph).getOrElse(id, Seq())
  }

  def beamSearch[Cost](pattern: Sketch,
                       beamSize: Int,
                       costFunction: CostFunction[Cost],
                       egraph: EGraph)
  : Map[EClassId, Seq[(Cost, ExprWithHashCons)]] =
  {
    assert(egraph.clean)
    val beamExtractMap = Analysis.oneShot(BeamExtract(beamSize, costFunction), egraph)
    val memo = HashMap.empty[Sketch, Map[EClassId, Seq[(Cost, ExprWithHashCons)]]]

    def searchRec(p: Sketch): Map[EClassId, Seq[(Cost, ExprWithHashCons)]] = {
      memo.get(p) match {
        case Some(value) => return value
        case None =>
      }

      val res: Map[EClassId, Seq[(Cost, ExprWithHashCons)]] = p match {
        case SketchAny(t) =>
          beamExtractMap.iterator.filter { case (id, beam) =>
            beam.nonEmpty && typeIsMatch(egraph, t, egraph.get(id).t)
          }.toMap
        case SketchVar(index, t) => ???
        case SketchNode(node, t) =>
          val childrenMatches = node.children().map(searchRec).toSeq
          // TODO: skip if any child has empty matches?

          egraph.classesByMatch.getOrElse(node.matchHash(), HashSet.empty).flatMap { id =>
            val eclass = egraph.get(id)
            var beam = Seq.empty[(Cost, ExprWithHashCons)]

            if (typeIsMatch(egraph, t, eclass.t)) {
              ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => (), _ => ()), { matched =>
                val typesMatch =
                  node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
                  node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) } &&
                  node.addresses().zip(matched.addresses()).forall { case (p, a) => addressIsMatch(p, a) }
                if (typesMatch) {
                  traverse(
                    childrenMatches.iterator.zip(matched.children())
                  ) { case (matches, id) =>
                    matches.get(id)
                    // TODO: skip if any child has empty matches?
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
                          beam = beam :+ ((
                            costFunction.cost(matched, c => selected(c)._1),
                            ExprWithHashCons(matched.mapChildren(c => selected(c)._2), eclass.t)))
                        }
                      }

                      productRec(0, Map.empty)
                  }
                }
              })
            }

            if (beam.isEmpty) {
              None
            } else {
              Some(id -> beam.sortBy(_._1)(costFunction.ordering).take(beamSize))
            }
          }.toMap
        case SketchContains(contained) =>
          val containedMatches = searchRec(contained)
          // TODO: skip if empty contained?

          val dataMap = egraph.classes.values.map { eclass =>
            eclass.id -> (containedMatches.get(eclass.id) match {
              case None => Nil
              case Some(beam) => beam
            })
          }.to(HashMap)

          val analyser = SemiLatticeAnalysis.oneShot(new SemiLatticeAnalysis {
            type Data = Seq[(Cost, ExprWithHashCons)]

            override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())

            override def make(egraph: EGraph, enode: ENode, t: TypeId,
                              analysisOf: EClassId => Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
              val childrenMatchingBeams = enode.children().map(c => (c, analysisOf(c))).toSeq
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

            override def merge(a: Data, b: Data): MergeResult = {
              val (r, mayNotBeA, mayNotBeB) = Beam.merge2(beamSize, costFunction, a, b)
              MergeResult(r, mayNotBeA, mayNotBeB)
            }
          }, egraph)(dataMap)

          dataMap.iterator.filter { case (_, beam) => beam.nonEmpty }.toMap
        case SketchOr(a, b) =>
          val aMatches = searchRec(a)
          val bMatches = searchRec(b)
          (aMatches.keySet union bMatches.keySet).map { id =>
            val aBeam = aMatches.getOrElse(id, Nil)
            val bBeam = bMatches.getOrElse(id, Nil)
            id -> Beam.merge(beamSize, costFunction, aBeam, bBeam)
          }.toMap
        case SketchAnd(a, b) => ???
        case SketchWithType(p, t) =>
          searchRec(p).filter { case (id, _) =>
            typeIsMatch(egraph, t, egraph.get(id).t)
          }
      }

      /* assert(res.values.forall(beam =>
        (beam.size <= beamSize) &&
        (beam == beam.sortBy(_._1)(costFunction.ordering).distinct))) */
      memo(p) = res
      res
    }

    searchRec(pattern)
  }

  def beamSearchRW[Cost](pattern: Sketch,
                         beamSize: Int,
                         costFunction: CostFunction[Cost],
                         egraph: EGraph,
                         id: EClassId): Seq[(Cost, ExprWithHashCons)] = {
    beamSearchRW(pattern, beamSize, costFunction, egraph).getOrElse(id, Seq())
  }

  def beamSearchRW[Cost](pattern: Sketch,
                         beamSize: Int,
                         costFunction: CostFunction[Cost],
                         egraph: EGraph)
  : Map[EClassId, Seq[(Cost, ExprWithHashCons)]] =
  {
    ??? // TODO
  }

  def typeIsMatch(egraph: EGraph, p: TypePattern, t: TypeId): Boolean = {
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
  def dataTypeIsMatch(egraph: EGraph, p: DataTypePattern, t: DataTypeId): Boolean = {
    p match {
      case DataTypePatternNode(pn) => typeNodeIsMatch(egraph, pn, t)
      case DataTypePatternVar(index) => ??? // TODO
      case DataTypePatternAny => true
    }
  }
  def typeNodeIsMatch(egraph: EGraph,
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
  def natIsMatch(egraph: EGraph, p: NatPattern, t: NatId): Boolean = {
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
  def addressIsMatch(p: AddressPattern, t: Address): Boolean = {
    p match {
      case AddressPatternNode(pn) => pn == t
      case AddressPatternVar(index) => ??? // TODO
      case AddressPatternAny => true
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
  type MNode[T] = Node[T, NatId, DataTypeId, Address]

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

/** A sketch is a program pattern that leaves some details unspecified */
sealed trait Sketch {
  import Sketch.{typeIsMatch, dataTypeIsMatch, natIsMatch, addressIsMatch, traverse}

  def searchEClass(egraph: EGraph, id: EClassId): Vec[ExtendedPatternMatch] = {
    assert(egraph.clean)
    searchEClass(egraph, egraph.get(id), Set(), HashMap.empty/*, None*/)
  }

  // TODO: investigate similarities with e-matching
  private def searchEClass(egraph: EGraph,
                           eclass: EClass,
                           visited: Set[EClassId],
                           memo: HashMap[(EClassId, Sketch), Vec[ExtendedPatternMatch]],
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
      case SketchAny(t) =>
        if (typeIsMatch(egraph, t, eclass.t)) { Vec(ExtendedPatternMatch.EClass(eclass.id)) } else { Vec.empty }
      case SketchVar(index, t) => ??? // TODO
      case SketchNode(node, t) =>
        // if (isFunctionOfApp) { assert(!node.matches(Lambda(()))) } // avoid some non-BENF paths
        // egraph.classesByMatch.get(node.matchHash())
        val res = Vec.empty[ExtendedPatternMatch]
        if (typeIsMatch(egraph, t, eclass.t)) {
          ematching.forEachMatchingNode(eclass, node.map(_ => (), _ => (), _ => (), _ => ()), matched => {
            val typesMatch =
              node.nats().zip(matched.nats()).forall { case (p, n) => natIsMatch(egraph, p, n) } &&
              node.dataTypes().zip(matched.dataTypes()).forall { case (p, dt) => dataTypeIsMatch(egraph, p, dt) } &&
              node.addresses().zip(matched.addresses()).forall { case (p, a) => addressIsMatch(p, a) }
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
      case SketchContains(contained) =>
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
      case SketchOr(a, b) => ???
      case SketchAnd(a, b) => ???
      case SketchWithType(p, t) => ???
    }

    memo((eclass.id, this)) = res

    res
  }
}
case class SketchAny(t: TypePattern) extends Sketch {
  override def toString: String = s"(? : $t)"
}
case class SketchVar(index: Int, t: TypePattern) extends Sketch {
  override def toString: String = s"(?$index : $t)"
}
case class SketchNode(node: Sketch.PNode, t: TypePattern) extends Sketch {
  override def toString: String = s"(${node.toString} : $t)"
}
case class SketchContains(contained: Sketch) extends Sketch {
  override def toString: String = s"contains($contained)"
}
case class SketchOr(a: Sketch, b: Sketch) extends Sketch {
  override def toString: String = s"$a ∨ $b"
}
case class SketchAnd(a: Sketch, b: Sketch) extends Sketch {
  override def toString: String = s"$a ∧ $b"
}
// FIXME: redundancy with other nodes above
case class SketchWithType(p: Sketch, t: TypePattern) extends Sketch {
  override def toString: String = s"($p : $t)"
}

object SketchDSL {
  import scala.language.implicitConversions

  def contains(p: Sketch): Sketch =
    SketchContains(p)

  implicit final class SketchOperators(private val p: Sketch) extends AnyVal {
    @inline def and(q: Sketch): Sketch = SketchAnd(p, q)
    @inline def or(q: Sketch): Sketch = SketchOr(p, q)

    @inline def >>(f: Sketch): Sketch.PNode = Composition(p, f)
    @inline def |>(f: Sketch): Sketch = app(f, p)

    @inline def +(rhs: Sketch): Sketch = app(app(add, p), rhs)
    @inline def -(rhs: Sketch): Sketch = app(app(sub, p), rhs)
    @inline def *(rhs: Sketch): Sketch = app(app(mul, p), rhs)
    @inline def /(rhs: Sketch): Sketch = app(app(div, p), rhs)
    @inline def %(rhs: Sketch): Sketch = app(app(mod, p), rhs)
    @inline def >(rhs: Sketch): Sketch = app(app(gt, p), rhs)
    @inline def <(rhs: Sketch): Sketch = app(app(lt, p), rhs)
    @inline def =:=(rhs: Sketch): Sketch = app(app(equal, p), rhs)
    @inline def >=(rhs: Sketch): Sketch = app(not, p < rhs)
    @inline def <=(rhs: Sketch): Sketch = app(not, p > rhs)
  }

  case object `?`
  case class `?`(index: Int)
  def %(index: Int): Var = Var(index)

  def app(a: Sketch, b: Sketch): Sketch.PNode = App(a, b)
  def lam(e: Sketch): Sketch.PNode = Lambda(e)
  def nApp(f: Sketch, x: NatPattern): Sketch.PNode = NatApp(f, x)
  def nLam(e: Sketch): Sketch.PNode = NatLambda(e)
  def dtApp(f: Sketch, x: DataTypePattern): Sketch.PNode = DataApp(f, x)
  def dtLam(e: Sketch): Sketch.PNode = DataLambda(e)
  def aApp(f: Sketch, x: AddressPattern): Sketch.PNode = AddrApp(f, x)
  def aLam(e: Sketch): Sketch.PNode = AddrLambda(e)
  def l(d: semantics.Data): Sketch.PNode = Literal(d)
  def larr(a: Seq[semantics.Data]): Sketch.PNode = l(semantics.ArrayData(a))

  def prim(p: rise.core.Primitive): Sketch.PNode = Primitive(p)
  def slide: Sketch.PNode = prim(rcp.slide.primitive)
  def map: Sketch.PNode = prim(rcp.map.primitive)
  def mapSeq: Sketch.PNode = prim(rcp.mapSeq.primitive)
  def mapSeqUnroll: Sketch.PNode = prim(rcp.mapSeqUnroll.primitive)
  def reduce: Sketch.PNode = prim(rcp.reduce.primitive)
  def reduceSeq: Sketch.PNode = prim(rcp.reduceSeq.primitive)
  def reduceSeqUnroll: Sketch.PNode = prim(rcp.reduceSeqUnroll.primitive)
  def transpose: Sketch.PNode = prim(rcp.transpose.primitive)
  def makePair: Sketch.PNode = prim(rcp.makePair.primitive)
  def zip: Sketch.PNode = prim(rcp.zip.primitive)
  def join: Sketch.PNode = prim(rcp.join.primitive)
  def fst: Sketch.PNode = prim(rcp.fst.primitive)
  def snd: Sketch.PNode = prim(rcp.snd.primitive)
  def add: Sketch.PNode = prim(rcp.add.primitive)
  def sub: Sketch.PNode = prim(rcp.sub.primitive)
  def mod: Sketch.PNode = prim(rcp.mod.primitive)
  def gt: Sketch.PNode = prim(rcp.gt.primitive)
  def lt: Sketch.PNode = prim(rcp.lt.primitive)
  def equal: Sketch.PNode = prim(rcp.equal.primitive)
  def not: Sketch.PNode = prim(rcp.not.primitive)
  def mul: Sketch.PNode = prim(rcp.mul.primitive)
  def div: Sketch.PNode = prim(rcp.div.primitive)
  def drop: Sketch.PNode = prim(rcp.drop.primitive)
  def take: Sketch.PNode = prim(rcp.take.primitive)
  def let: Sketch.PNode = prim(rcp.let.primitive)
  def toMem: Sketch.PNode = prim(rcp.toMem.primitive)
  def iterateStream: Sketch.PNode = prim(rcp.iterateStream.primitive)

  def global: AddressPattern = AddressPatternNode(Global)
  def `private`: AddressPattern = AddressPatternNode(Private)

  object ocl {
    import rise.openCL.{primitives => p}
    def mapGlobal(dim: Int): Sketch.PNode = prim(p.mapGlobal(dim).primitive)
    def circularBuffer: Sketch.PNode = prim(p.oclCircularBuffer.primitive)
    def reduceSeqUnroll: Sketch.PNode = prim(p.oclReduceSeqUnroll.primitive)
    def toMem: Sketch.PNode = prim(p.oclToMem.primitive)
  }

  object omp {
    def mapPar: Sketch.PNode = prim(rise.openMP.primitives.mapPar.primitive)
  }

  implicit def panyWithoutType(v: `?`.type): SketchAny =
    SketchAny(TypePatternAny)
  implicit def pvarWithoutType(v: `?`): SketchVar =
    SketchVar(v.index, TypePatternAny)
  implicit def pnodeWithoutType(n: Sketch.PNode): Sketch =
    SketchNode(n, TypePatternAny)
  implicit final class PWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(p: Sketch): Sketch = SketchWithType(p, t)
  }
  implicit final class PAnyWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(v: `?`.type): Sketch = SketchAny(t)
  }
  implicit final class PVarWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(v: `?`): Sketch = SketchVar(v.index, t)
  }
  implicit final class PNodeWithType(private val t: TypePattern) extends AnyVal {
    @inline def ::(n: Sketch.PNode): Sketch = SketchNode(n, t)
  }

  // FIXME: redundancy with the regular PatternDSL

  def `?n`(index: Int): NatPatternVar = NatPatternVar(index)
  val `?n`: NatPattern = NatPatternAny
  def `%n`(index: Int): NatPattern = NatPatternNode(NatVar(index))

  def cst(value: Long): NatPattern = NatPatternNode(NatCst(value))
  implicit final class NatPatternOps(private val n: NatPattern) extends AnyVal {
    // FIXME: need to pivot or canonicalize nats while matching
    def /^(m: NatPattern): NatPattern =
      NatPatternNode(NatMul(NatPatternNode(NatPow(m, NatPatternNode(NatCst(-1)))), n))
  }

  // not supported in all cases, so no DSL for it
  // def `?t`(index: Int): TypePattern = TypePatternVar(index)
  val `?t`: TypePattern = TypePatternAny
  def `?dt`(index: Int): DataTypePattern = DataTypePatternVar(index)
  val `?dt`: DataTypePattern = DataTypePatternAny
  def `%dt`(index: Int): DataTypePattern = DataTypePatternNode(DataTypeVar(index))

  val int: DataTypePattern = DataTypePatternNode(ScalarType(rcdt.int))
  val f32: DataTypePattern = DataTypePatternNode(ScalarType(rcdt.f32))

  def nFunT(t: TypePattern): TypePattern = TypePatternNode(NatFunType(t))
  def vecT(n: NatPattern, dt: DataTypePattern): DataTypePattern = DataTypePatternNode(VectorType(n, dt))
  implicit final class FunConstructorT(private val r: TypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern = TypePatternNode(FunType(t, r))
  }
  implicit final class FunConstructorDT(private val r: DataTypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern = TypePatternNode(FunType(t, r: TypePattern))
  }
  implicit final class ArrayConstructor(private val s: NatPattern) extends AnyVal {
    @inline def `.`(et: DataTypePattern): DataTypePattern = DataTypePatternNode(ArrayType(s, et))
  }
  implicit final class PairConstructor(private val s: DataTypePattern) extends AnyVal {
    @inline def x(et: DataTypePattern): DataTypePattern = DataTypePatternNode(PairType(s, et))
  }
}