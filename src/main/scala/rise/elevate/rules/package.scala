package rise.elevate

import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.core.{Failure, Strategy, Success}
import elevate.core.macros.rule
import rise.core.DSL._
import rise.core._
import rise.core.types._

package object rules {

  //TODO @rule
  def betaReduction: Strategy[Rise] = {
    case App(Lambda(x, b), v) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case DepApp(k1, DepLambda(k2, x, b), v) if k1 == k2 =>
      Success(substitute.kindInExpr(k2, v, `for` = x, in = b))
    case _ => Failure(betaReduction)
  }

  //TODO @rule
  def containsAtLeast(n: Int, x: Rise)(implicit ev: Traversable[Rise]): Strategy[Rise] =
    skip(n)(isEqualTo(x))

  // TODO: express as a combination of strategies
  // TODO @rule
  def gentleBetaReduction()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case App(Lambda(x, b), v: Identifier) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case App(Lambda(x, b), v @ App(App(primitives.makePair(), _), _)) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case App(Lambda(x, b), v) if !containsAtLeast(1, x)(ev)(b) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case DepApp(k1, DepLambda(k2, x, b), v) if k1 == k2 =>
      Success(substitute.kindInExpr(k2, v, `for` = x, in = b))
    case _ => Failure(gentleBetaReduction())
  }

  //TODO @rule
  def etaReduction()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e@Lambda(x1, App(f, x2)) if x1 == x2 && !contains[Rise](x1).apply(f) => Success(f !: e.t)
    case _ => Failure(etaReduction())
  }

  def etaAbstraction: Strategy[Rise] = rule("etaAbstraction", f => f.t match {
    case FunType(_, _) =>
      val x = identifier(freshName("η"))
      Success(lambda(x, app(f, x)) !: f.t)
    case _ => Failure(etaAbstraction)
  })

  def idxReduction: Strategy[Rise] = rule("idxReduction", e => {
    import arithexpr.arithmetic._
    import rise.core.primitives._
    import rise.core.semantics._

    @scala.annotation.tailrec
    def isMakeArray(e: Rise): Boolean = e match {
      case makeArray(_) => true
      case App(f, _) => isMakeArray(f)
      case _ => false
    }

    @scala.annotation.tailrec
    def indexMakeArray(e: Rise, i: Long, n: Long): Rise = e match {
      case App(_, v) if i == (n - 1) => v
      case App(f, _) => indexMakeArray(f, i, n - 1)
      case _ => throw new Exception("index out of bounds")
    }

    e match {
      case App(App(idx(), Literal(IndexData(Cst(i), Cst(n)))), mka)
        if isMakeArray(mka) =>
        Success(indexMakeArray(mka, i, n))
      case _ =>
        Failure(idxReduction)
    }
  })

  def checkType(msg: String = ""): Strategy[Rise] = rule("checkType", e => {
    types.check(e) match {
      case scala.util.Success(_) => Success(e)
      case scala.util.Failure(exception) =>
        Failure(checkType(exception.getMessage))
    }
  })
}
