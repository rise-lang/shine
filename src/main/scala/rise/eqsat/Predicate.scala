package rise.eqsat

import rise.eqsat.ematching.MNode

// TODO: think about what the predicate interface should be
trait Predicate[ED, ND, TD] {
  def start(egraph: EGraph[ED, ND, TD],
            roots: Seq[EClassId]): Unit = {}
  def stop(): Unit = {}

  def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean = true
  def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED], en: ENode): Boolean = true
}

object PredicateDSL {
  import scala.language.implicitConversions

  implicit final class Operators[ED, ND, TD](private val a: Predicate[ED, ND, TD])
    extends AnyVal
  {
    @inline def &&(b: Predicate[ED, ND, TD]): Predicate[ED, ND, TD] = new AndPredicate(a, b)
    @inline def ||(b: Predicate[ED, ND, TD]): Predicate[ED, ND, TD] = new OrPredicate(a, b)
  }
}

case class NoPredicate[ED, ND, TD]() extends Predicate[ED, ND, TD]

abstract class PairPredicate[ED, ND, TD](val a: Predicate[ED, ND, TD],
                                         val b: Predicate[ED, ND, TD]) extends Predicate[ED, ND, TD] {
  override def start(egraph: EGraph[ED, ND, TD],
                     roots: Seq[EClassId]): Unit = {
    a.start(egraph, roots)
    b.start(egraph, roots)
  }

  override def stop(): Unit = {
    a.stop()
    b.stop()
  }
}

class AndPredicate[ED, ND, TD](override val a: Predicate[ED, ND, TD],
                               override val b: Predicate[ED, ND, TD]) extends PairPredicate[ED, ND, TD](a, b) {
  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean =
    a(egraph, ec) && b(egraph, ec)

  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED], en: ENode): Boolean =
    a(egraph, ec, en) && b(egraph, ec, en)
}

class OrPredicate[ED, ND, TD](override val a: Predicate[ED, ND, TD],
                              override val b: Predicate[ED, ND, TD]) extends PairPredicate[ED, ND, TD](a, b) {
  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean =
    a(egraph, ec) || b(egraph, ec)

  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED], en: ENode): Boolean =
    a(egraph, ec, en) || b(egraph, ec, en)
}

case class ArrayDimensionPredicate[ED, ND, TD](limit: Int) extends Predicate[ED, ND, TD] {
  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean = {
    // TODO: e-class analysis?
    def countArrayDims(t: TypeId): Int = {
      egraph(t)._1 match {
        case FunType(inT, outT) =>
          countArrayDims(inT) max countArrayDims(outT)
        case NatFunType(t) => countArrayDims(t)
        case DataFunType(t) => countArrayDims(t)
        case ArrayType(_, et) => 1 + countArrayDims(et)
        case PairType(dt1, dt2) =>
          countArrayDims(dt1) max countArrayDims(dt2)
        case DataTypeVar(_) | ScalarType(_) | NatType |
             VectorType(_, _) | IndexType(_) => 0
      }
    }

    countArrayDims(ec.t) <= limit
  }
}

object ASTSizePredicate {
  def apply(limit: Int): ASTSizePredicate = new ASTSizePredicate(
    limit = limit,
    minimumUpstreamSize = HashMap.empty
  )
}

class ASTSizePredicate(limit: Int,
                       // minimum size of the surrounding trees
                       // leading up to a given e-class;
                       // in other words minimum size that must be produced
                       // before reaching this e-class.
                       minimumUpstreamSize: HashMap[EClassId, Double])
  extends DefaultAnalysis.Predicate
{
  override def start(egraph: DefaultAnalysis.EGraph,
                     roots: Seq[EClassId]): Unit = {
    import Node.eclassIdOrdering
    // find the minimum upstream sizes using Dijkstra

    val rootsCanonical = roots.map(egraph.findMut)

    val todo = scala.collection.mutable.TreeSet(rootsCanonical.map(r => (0.0, r)): _*)
    assert(minimumUpstreamSize.isEmpty)
    minimumUpstreamSize ++= rootsCanonical.map(r => r -> 0.0)

    while (todo.nonEmpty) {
      val (mus, id) = todo.firstKey
      todo.remove((mus, id))

      val eclass = egraph.get(id)
      assert(mus == minimumUpstreamSize(id))

      // for (n <- eclass.nodes) {
      var i = 0
      while (i < eclass.nodes.size) {
        val n = eclass.nodes(i)

        val children = n.children().toSeq
        val childrenMds = children.map { c =>
          minimumDownstreamSize(egraph.get(c))
        }
        val nodeMds = 1 + childrenMds.sum

        // for ((child, childMds) <- n.children().zip(childrenMds)) {
        var j = 0
        while (j < children.size) {
          val child = children(j)
          val childMds = childrenMds(j)

          // the added minimum upstream size for the child is:
          // 1 for the newly constructed node
          // + the minimum downstream size of all the surrounding children
          // ;
          // which is equal to the minimum downstream size of the entire node
          // minus the minimum downstream size of the selected child
          val childMus = mus + nodeMds - childMds
          if (minimumUpstreamSize.get(child).forall(childMus < _)) {
            minimumUpstreamSize.get(child).foreach(prevChildMus =>
              todo -= ((prevChildMus, child))
            )
            minimumUpstreamSize(child) = childMus
            todo += ((childMus, child))
          }

          j += 1
        }

        i += 1
      }
    }
  }

  override def stop(): Unit = minimumUpstreamSize.clear()

  private def minimumDownstreamSize(ec: DefaultAnalysis.EClass): Double =
    ec.data.extracted.map(_._2.toDouble).getOrElse(Double.PositiveInfinity)

  override def apply(egraph: DefaultAnalysis.EGraph, ec: DefaultAnalysis.EClass): Boolean = {
    val mus = minimumUpstreamSize.getOrElse(ec.id, Double.PositiveInfinity)
    val mds = minimumDownstreamSize(ec)
    val minimumSize = mus + mds
    minimumSize <= limit
  }
}

object Prototype {
  object Cost {
    val zero: Cost = Cost(0, HashMap.empty)

    def ofNode(n: ENode): Cost =
      Cost(1, HashMap(n.map(_ => (), _ => (), _ => ()) -> 1))
  }

  case class Cost(size: Int,
                  nodes: HashMap[MNode, Int]) {
    def +(other: Cost): Cost = {
      val psk = this.nodes.keySet ++ other.nodes.keySet
      val psm = psk
        .map { p => p -> (this.nodes(p) + other.nodes(p)) }
        .to(HashMap)
      Cost(this.size + other.size, psm)
    }

    def merge(other: Cost): Cost = {
      val psk = this.nodes.keySet intersect other.nodes.keySet
      val psm = psk
        .map { p => p -> (this.nodes(p) min other.nodes(p)) }
        .to(HashMap)
      Cost(this.size min other.size, psm)
    }
  }

  def countLimits(
    egraph: DefaultAnalysis.EGraph,
    roots: Seq[EClassId],
    // limit: Cost
  ): Unit = {
    ???
  }
}
