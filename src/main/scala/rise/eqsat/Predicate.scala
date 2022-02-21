package rise.eqsat

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import rise.eqsat.ematching.MNode

trait Predicate {
  def start(egraph: EGraph,
            roots: Seq[EClassId]): Unit = {}
  def stop(): Unit = {}

  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])

  def apply(egraph: EGraph, ec: EClass): Boolean = true
  def apply(egraph: EGraph, ec: EClass, en: ENode): Boolean = true
}

object PredicateDSL {
  import scala.language.implicitConversions

  implicit final class PredicateOperators(private val a: Predicate)
    extends AnyVal
  {
    @inline def &&(b: Predicate): Predicate = new AndPredicate(a, b)
    @inline def ||(b: Predicate): Predicate = new OrPredicate(a, b)
  }
}

case class NoPredicate() extends Predicate {
  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())
}

abstract class PairPredicate(val a: Predicate,
                                         val b: Predicate) extends Predicate {
  override def start(egraph: EGraph,
                     roots: Seq[EClassId]): Unit = {
    a.start(egraph, roots)
    b.start(egraph, roots)
  }

  override def stop(): Unit = {
    a.stop()
    b.stop()
  }

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    Analysis.mergeRequired(a.requiredAnalyses(), b.requiredAnalyses())
}

class AndPredicate(override val a: Predicate,
                               override val b: Predicate) extends PairPredicate(a, b) {
  override def apply(egraph: EGraph, ec: EClass): Boolean =
    a(egraph, ec) && b(egraph, ec)

  override def apply(egraph: EGraph, ec: EClass, en: ENode): Boolean =
    a(egraph, ec, en) && b(egraph, ec, en)
}

class OrPredicate(override val a: Predicate,
                              override val b: Predicate) extends PairPredicate(a, b) {
  override def apply(egraph: EGraph, ec: EClass): Boolean =
    a(egraph, ec) || b(egraph, ec)

  override def apply(egraph: EGraph, ec: EClass, en: ENode): Boolean =
    a(egraph, ec, en) || b(egraph, ec, en)
}

case class ArrayDimensionPredicate(limit: Int) extends Predicate {
  override def apply(egraph: EGraph, ec: EClass): Boolean = {
    // TODO: e-class analysis?
    def countArrayDims(t: TypeId): Int = {
      egraph(t) match {
        case FunType(inT, outT) =>
          countArrayDims(inT) max countArrayDims(outT)
        case NatFunType(t) => countArrayDims(t)
        case DataFunType(t) => countArrayDims(t)
        case AddrFunType(t) => countArrayDims(t)
        case ArrayType(_, et) => 1 + countArrayDims(et)
        case PairType(dt1, dt2) =>
          countArrayDims(dt1) max countArrayDims(dt2)
        case DataTypeVar(_) | ScalarType(_) | NatType |
             VectorType(_, _) | IndexType(_) => 0
      }
    }

    countArrayDims(ec.t) <= limit
  }

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())
}

// TODO: nats should have no real part (1 divides n)
// TODO: VectorType(n, ) --> RangeConstraint(n, RangeMul(2, 16, 2))
// TODO: num /^ denum    --> RangeConstraint(num, RangeAdd(0, PosInf, denum)) [denum divides num]
// TODO: x % _           --> ArithPredicate(x, 0, >=) [x >= 0]
object StandardConstraintsPredicate extends Predicate {
  override def apply(egraph: EGraph, ec: EClass): Boolean = {
    // TODO: e-class analysis?
    def checkType(t: TypeId): Boolean = {
      egraph(t) match {
        case FunType(inT, outT) =>
          checkType(inT) && checkType(outT)
        case NatFunType(t) => checkType(t)
        case DataFunType(t) => checkType(t)
        case AddrFunType(t) => checkType(t)
        case ArrayType(n, et) => checkArraySize(n) && checkType(et)
        case VectorType(n, _) => checkArraySize(n)
        case IndexType(n) => checkArraySize(n)
        case PairType(dt1, dt2) =>
          checkType(dt1) && checkType(dt2)
        case DataTypeVar(_) | ScalarType(_) | NatType => true
      }
    }

    def checkArraySize(n: NatId): Boolean = {
      val named = Nat.toNamedGeneric(ExprWithHashCons.nat(egraph)(n),
        i => rise.core.types.NatIdentifier(s"n$i"))
      !ArithPredicate(named, 1, ArithPredicate.Operator.>=).evaluate.contains(false)
    }

    checkType(ec.t)
  }

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())
}

object ASTSizePredicate {
  def apply(limit: Int): ASTSizePredicate = new ASTSizePredicate(
    limit = limit,
    minimumUpstreamSize = HashMap.empty,
    minimumDownstreamSize = _ => throw new Exception("this should not happen")
  )
}

class ASTSizePredicate(limit: Int,
                       // minimum size of the surrounding trees
                       // leading up to a given e-class;
                       // in other words minimum size that must be produced
                       // before reaching this e-class.
                       minimumUpstreamSize: HashMap[EClassId, Double],
                       var minimumDownstreamSize: EClassId => Double)
  extends Predicate
{
  override def start(egraph: EGraph,
                     roots: Seq[EClassId]): Unit = {
    import Node.eclassIdOrdering
    // find the minimum upstream sizes using Dijkstra

    val rootsCanonical = roots.map(egraph.findMut)
    val smallestSizeOf = egraph.getAnalysis(SmallestSizeAnalysis)

    minimumDownstreamSize = { (id: EClassId) =>
      smallestSizeOf(id)._2.toDouble
    }

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
          minimumDownstreamSize(c)
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

  override def apply(egraph: EGraph, ec: EClass): Boolean = {
    val mus = minimumUpstreamSize.getOrElse(ec.id, Double.PositiveInfinity)
    val mds = minimumDownstreamSize(ec.id)
    val minimumSize = mus + mds
    minimumSize <= limit
  }

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(SmallestSizeAnalysis), Set())
}

/* TODO: generic algorithm for top-down + bottom-up predicates?
object GenericPredicate {
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
 */