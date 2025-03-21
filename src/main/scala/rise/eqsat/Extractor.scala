package rise.eqsat
import rise.core
import rise.core.traverse

object Extractor {
  def findBestOf[C](egraph: EGraph, costFunction: CostFunction[C], id: EClassId): (ExprWithHashCons, C) = {
    Analysis.oneShot(
      SmallestCostAnalysis(costFunction),
      egraph)(egraph.find(id))
  }

  def randomOf(egraph: EGraph, id: EClassId): ExprWithHashCons = {
    val random = new scala.util.Random

    def rec(id: EClassId): ExprWithHashCons = {
      val eclass = egraph.get(id)
      val node = eclass.nodes(random.nextInt(eclass.nodes.length))
      ExprWithHashCons(node.mapChildren(rec), eclass.t)
    }

    rec(id)
  }

  def printRandom(egraph: EGraph, id: EClassId, n: Int): Unit = {
    for (_ <- 0 until n) {
      println(Expr.toNamed(
        ExprWithHashCons.expr(egraph)(randomOf(egraph, id)),
        Expr.Bound.empty.copy(allowFreeIndices = true)))
    }
  }
}

/*
/** Extracts a single expression from an [[EGraph]],
  * which minimizes the given [[CostFunction]].
  */
class Extractor[Cost](val costFunction: CostFunction[Cost],
                      val costs: HashMap[EClassId, (Cost, ENode)],
                      val egraph: EGraph) {
  def findBestOf(eclass: EClassId): (Cost, ExprWithHashCons) =
    findBestRec(eclass, HashMap.empty)

  def findBestCostOf(eclass: EClassId): Cost =
    costs(egraph.find(eclass))._1

  private def findBestRec(eclass: EClassId,
                          addedMemo: HashMap[EClassId, ExprWithHashCons]
                         ): (Cost, ExprWithHashCons) = {
    val id = egraph.find(eclass)
    val (bestCost, bestNode) = costs(id)
    (bestCost, addedMemo.get(id) match {
      case Some(idExpr) => idExpr
      case None =>
        def childF(id: EClassId): ExprWithHashCons =
          findBestRec(id, addedMemo)._2
        val expr = ExprWithHashCons(bestNode.mapChildren(childF), egraph.get(id).t)
        assert(!addedMemo.contains(id))
        addedMemo += id -> expr
        expr
    })
  }
}
*/

case class SmallestCostAnalysis[Cost](costFunction: CostFunction[Cost])
  extends SemiLatticeAnalysis
{
  // TODO: would (ENode, Int) be more efficient?
  type Data = (ExprWithHashCons, Cost)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    smallestOf: EClassId => Data): Data =
    (ExprWithHashCons(enode.mapChildren(c => smallestOf(c)._1), t),
      costFunction.cost(enode, c => smallestOf(c)._2))

  override def merge(a: Data, b: Data): MergeResult = {
    (a, b) match {
      case ((_, aCost), (_, bCost)) =>
        if (costFunction.ordering.gt(aCost, bCost)) {
          MergeResult(b, mayNotBeA = true, mayNotBeB = false)
        } else {
          MergeResult(a, mayNotBeA = false, mayNotBeB = true)
        }
    }
  }
}

/** A cost function which can be used by an [[Extractor]] */
trait CostFunction[Cost] {
  val ordering: math.Ordering[Cost]
  // TODO: remove this option?
  def cost(enode: ENode, costs: EClassId => Cost): Cost =
    ???
  def cost(egraph: EGraph, enode: ENode, t: TypeId, costs: EClassId => Cost): Cost =
    cost(enode, costs)
}

object AstSize extends CostFunction[Int] {
  val ordering = implicitly
  override def cost(enode: ENode, costs: EClassId => Int): Int =
    enode.children().foldLeft(1) { case (acc, eclass) => acc + costs(eclass) }

  def ofNamedExpr(e: rise.core.Expr): Int = {
    rise.core.traverse.traverse(e, new traverse.PureAccumulatorTraversal[Int] {
      override val accumulator = util.monads.AddMonoid
      override def expr: core.Expr => Pair[core.Expr] = super.expr
    })._1
  }

  def ofExpr(e: Expr): Int =
    e.node.children().foldLeft(1) { case (acc, c) => acc + ofExpr(c) }
}

object AstDepth extends CostFunction[Int] {
  val ordering = implicitly
  override def cost(enode: ENode, costs: EClassId => Int): Int =
    1 + enode.children().foldLeft(0) { case (acc, eclass) => acc.max(costs(eclass)) }
}

object AppCount extends CostFunction[Int] {
  val ordering = implicitly
  override def cost(enode: ENode, costs: EClassId => Int): Int = {
    val thisCount = enode match {
      case App(_, _) => 1
      case _ => 0
    }
    enode.children().foldLeft(thisCount) { case (acc, eclass) => acc + costs(eclass) }
  }
}

case class LexicographicCost[A, B](a: CostFunction[A], b: CostFunction[B])
  extends CostFunction[(A, B)] {
  val ordering = {
    implicit val aOrd = a.ordering
    implicit val bOrd = b.ordering
    implicitly
  }
  override def cost(enode: ENode, costs: EClassId => (A, B)): (A, B) =
    (a.cost(enode, id => costs(id)._1), b.cost(enode, id => costs(id)._2))
}

object BENFRedexCount {
  case class Data(redexes: Int,
                  free: Set[Int],
                  isEtaApp: Boolean,
                  isLam: Boolean,
                  isNatLam: Boolean)
}

case class BENFRedexCount(/*egraph: EGraph*/) extends CostFunction[BENFRedexCount.Data] {
  import BENFRedexCount._

  override val ordering = new Ordering[Data] {
    override def compare(x: Data, y: Data): Int =
      x.redexes compare y.redexes
  }

  override def cost(enode: ENode, costs: EClassId => Data): Data = {
    // val (freeOfNat, freeOfType) = egraph.getTypeAnalysis(FreeAnalysis)

    enode match {
      case Var(index) =>
        Data(0, Set(index), isEtaApp = false, isLam = false, isNatLam = false)
      case App(f, e) =>
        val fd = costs(f)
        val ed = costs(e)
        val isRedex = if (fd.isLam) { 1 } else { 0 }
        Data(fd.redexes + ed.redexes + isRedex, fd.free ++ ed.free,
          isEtaApp = !fd.free.contains(0), isLam = false, isNatLam = false)
      case Lambda(e) =>
        val ed = costs(e)
        val isRedex = if (ed.isEtaApp) { 1 } else { 0 }
        Data(
          ed.redexes + isRedex,
          ed.free.filter(idx => idx != 0).map(idx => idx - 1),
          isEtaApp = false, isLam = true, isNatLam = false
        )
      case NatApp(f, _) =>
        val fd = costs(f)
        val isRedex = if (fd.isNatLam) { 1 } else { 0 }
        Data(fd.redexes + isRedex, fd.free,
          isEtaApp = false, isLam = false, isNatLam = false)
      case DataApp(f, _) =>
        val fd = costs(f)
        val isRedex = 0 // TODO: if (fd.isDataLam) { 1 } else { 0 }
        Data(fd.redexes + isRedex, fd.free,
          isEtaApp = false, isLam = false, isNatLam = false)
      case AddrApp(f, _) =>
        val fd = costs(f)
        val isRedex = 0 // TODO: if (fd.isDataLam) { 1 } else { 0 }
        Data(fd.redexes + isRedex, fd.free,
          isEtaApp = false, isLam = false, isNatLam = false)
      case NatLambda(e) =>
        val ed = costs(e)
        Data(ed.redexes, ed.free, isEtaApp = false, isLam = false, isNatLam = true)
      case DataLambda(e) =>
        val ed = costs(e)
        Data(ed.redexes, ed.free, isEtaApp = false, isLam = false, isNatLam = false)
      case AddrLambda(e) =>
        val ed = costs(e)
        Data(ed.redexes, ed.free, isEtaApp = false, isLam = false, isNatLam = false)
      case Literal(_) | NatLiteral(_) | IndexLiteral(_, _) | Primitive(_) =>
        Data(0, Set(), isEtaApp = false, isLam = false, isNatLam = false)
      case Composition(f, g) =>
        val fd = costs(f)
        val gd = costs(g)
        Data(fd.redexes + gd.redexes, fd.free ++ gd.free,
          isEtaApp = false, isLam = false, isNatLam = false)
    }

    // TODO: freeOfType(t) for free nat/data
  }
}