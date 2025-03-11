package rise.autotune

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import arithexpr.arithmetic.{ArithExpr, BoolExpr, RangeAdd, RangeMul}
import rise.core.{DepApp, DepLambda, Expr, Lambda, traverse}
import rise.core.types._
import rise.core.types.DataType._
import util.monads
import scala.annotation.tailrec

object constraints {

  sealed trait Constraint {
    def substitute(map: Map[ArithExpr, ArithExpr]): Constraint = this match {
      case PredicateConstraint(n) =>
        PredicateConstraint(n.substitute(map).getOrElse(n))
      case RangeConstraint(n, r) =>
        RangeConstraint(n.substitute(map).getOrElse(n), r.substitute(map).getOrElse(r))
    }

    def isSatisfied(): Boolean = this match {
      case PredicateConstraint(n: ArithPredicate) =>
        n.evaluate.contains(true)
      // TODO: it feels like checking if a nat is inside a range should be part of arithexpr
      case RangeConstraint(n, RangeAdd(start, stop, step)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
          ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
          n % step == (0: Nat)
      case RangeConstraint(n, RangeMul(start, stop, mul)) =>
        ArithPredicate(start, n.min, ArithPredicate.Operator.<=).evaluate.contains(true) &&
          ArithPredicate(n.max, stop, ArithPredicate.Operator.<=).evaluate.contains(true) &&
          isPowerOf(n.eval, mul.eval)
      case _ =>
        throw new Exception(s"no support for checking $this")
    }
  }
  case class PredicateConstraint(n: BoolExpr) extends Constraint {
    override def toString: String = n.toString
  }
  case class RangeConstraint(n: Nat, r: arithexpr.arithmetic.Range) extends Constraint {
    override def toString: String = s"($n) in $r"
  }

  def checkConstraints(constraints: Set[Constraint], values: Map[NatIdentifier, Nat]): Boolean = {
    val map = values.asInstanceOf[Map[ArithExpr, ArithExpr]]
    constraints.forall(c => c.substitute(map).isSatisfied())
  }

  def collectParameters(e: Expr): Parameters = {
    var params = scala.collection.mutable.Set[NatIdentifier]()
    traverse.traverse(e, new traverse.PureTraversal {
      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild({
          case n@TuningParameter() =>
            params += n
            n
          case ae => ae
        }))
    })
    params.toSet
  }

  private def collectInputNats(e: Expr): Set[NatIdentifier] = {
    @tailrec
    def iter(e: Expr, inputs: Set[NatIdentifier]): Set[NatIdentifier] = {
      e match {
        case DepLambda(NatKind, x: NatIdentifier, e) => iter(e, inputs + x)
        case DepLambda(_, _, e) => iter(e, inputs)
        case Lambda(_, e) => iter(e, inputs)
        case _ => inputs
      }
    }
    iter(e, Set.empty)
  }

  private def isPowerOf(a: Int, b: Int): Boolean = {
    val p: Int = Math.round(Math.log(a) / Math.log(b)).toInt
    Math.round(Math.pow(b, p)) == a
  }

  // we only look at constraints on top-level nats
  def collectConstraints(e: Expr, parameters: Parameters): Set[Constraint] = {
    import arithexpr.arithmetic._
    import BoolExpr._

    val paramOrInput = (parameters ++ collectInputNats(e)).map(_.name)
    val cs = collection.mutable.Set[Constraint]()

    def addPredicate(p: ArithPredicate): Unit = {
      if (!p.evaluate.contains(true)) {
        cs += PredicateConstraint(p)
      }
    }

    traverse.traverse(e, new traverse.PureTraversal {
      override def expr: Expr => monads.Pure[Expr] = { e =>
        e match {
          case DepApp(NatKind, DepApp(NatKind, DepApp(NatKind,
          DepApp(NatKind, DepApp(NatKind, DepApp(NatKind,
          rise.openCL.primitives.oclRunPrimitive(),
          ls0: Nat), ls1: Nat), ls2: Nat),
          gs0: Nat), gs1: Nat), gs2: Nat)
          =>
            for (s <- Seq(ls0, ls1, ls2, gs0, gs1, gs2)) {
              addPredicate(ArithPredicate(s, 1, ArithPredicate.Operator.>=))
            }
            for ((ls, gs) <- Seq((ls0, gs0), (ls1, gs1), (ls2, gs2))) {
              cs += RangeConstraint(gs, RangeAdd(0, PosInf, ls))
            }
          case _ =>
        }
        super.expr(e)
      }

      override def datatype: DataType => monads.Pure[DataType] = { t =>
        t match {
          case ArrayType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            addPredicate(ArithPredicate(n, 1, ArithPredicate.Operator.>=))
          case VectorType(n, _) if n.varList.forall(v => paramOrInput(v.name)) =>
            cs += RangeConstraint(n, RangeMul(2, 16, 2))
          case _ =>
        }
        super.datatype(t)
      }

      override def nat: Nat => monads.Pure[Nat] = n =>
        return_(n.visitAndRebuild { m =>
          if (m.varList.forall(v => paramOrInput(v.name))) { m match {
            case Prod(parts) =>
              var (num, denum) = parts.partition {
                case Pow(_, Cst(-1)) => false
                case _ => true
              }
              denum = denum.map {
                case Pow(b, Cst(-1)) => b
                case _ => ???
              }
              if (denum.nonEmpty) { // num /^ denum
                val aNum = num.fold(1: ArithExpr)(_ * _)
                val aDenum = denum.fold(1: ArithExpr)(_ * _)
                if (aNum % aDenum != Cst(0)) {
                  cs += RangeConstraint(aNum, RangeAdd(0, PosInf, aDenum))
                }
              }
            case Mod(x, _) =>
              addPredicate(ArithPredicate(x, 0, ArithPredicate.Operator.>=))
            case _ => ()
          }}
          m
        })
    })

    cs.toSet
  }
}
