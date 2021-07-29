package rise.elevate.strategies

import elevate.core._
import elevate.core.strategies.predicate._
import elevate.core.strategies.{Traversable, basic}
import rise.core.DSL.ToBeTyped
import rise.core._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate._
import rise.elevate.rules.lowering.isComputation

object predicate {

  // Matching Single Nodes

  case class is(pred: Expr => Boolean, name: String) extends Strategy[Rise] {
    override def apply(e: Rise): RewriteResult[Rise] =
      if (pred(e)) {
        Success(e)
      } else {
        Failure(this)
      }
    override def toString(): String = s"is($name)"
  }

  def isLambda: is = is(_.isInstanceOf[Lambda], "Lambda")
  def isDepLambda: is = is(_.isInstanceOf[DepLambda[_, _]], "DepLambda")
  def isIdentifier: is = is(_.isInstanceOf[Identifier], "Identifier")
  def isApply: is = is(_.isInstanceOf[App], "Apply")

  def isArray: is = is(_.t.isInstanceOf[ArrayType], "Array")

  def isPrimitive(p: Builder): is = is(p.unapply, p.toString)

  def isLet: is = isPrimitive(let)

  def isMap: is = isPrimitive(map)

  def isGenerate: is = isPrimitive(generate)

  def isReduce: is = isPrimitive(reduce)
  def isReduceSeq: is = isPrimitive(reduceSeq)
  def isReduceSeqUnroll: is = isPrimitive(reduceSeqUnroll)
  def isReduceX: Strategy[Rise] = isReduce <+ isReduceSeq <+ isReduceSeqUnroll

  def isTranspose: is = isPrimitive(transpose)

  def isMakeArray: is = is({  case _@makeArray(_) => true
                              case _ => false }, "MakeArray" )

  // Matching Applied Primitives

  def isApplied(fp: Strategy[Rise]): Strategy[Rise] =
    isApp(fp, basic.id)

  case class isApp(fp: Strategy[Rise], ep: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(f, e) =>
        fp(f).flatMapSuccess(_ =>
          ep(e).mapSuccess(_ =>
            a))
      case _          => Failure(isApp(fp, ep))
    }
  }

  def isAppliedBinaryFun(pred: Expr => Boolean,
                         name: String): is =
    is({
      case App(App(f, _), _) if pred(f) => true
      case _ => false
    }, s"Applied$name")

  def isAppliedTernaryFun(pred: Expr => Boolean,
                          name: String): is =
    is({
      case App(App(App(f, _), _), _) if pred(f) => true
      case _ => false
    }, s"Applied$name")

  def isAppliedLet: is = isAppliedBinaryFun(let.unapply, "Let")

  def isAppliedMap: is = isAppliedBinaryFun(map.unapply, "Map")

  def isAppliedZip: is = isAppliedBinaryFun(zip.unapply, "Zip")

  def isAppliedReduce: is = isAppliedBinaryFun(reduce.unapply, "Reduce")

  case class isVectorizeablePrimitive()(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(map(), f), input) if isComputation()(ev)(f) && !isVectorArray(a.t) => Success(a)
      case r@App(App(App(reduce(), op), init), input) if isComputation()(ev)(op) => Success(r)
      case _ => Failure(isVectorizeablePrimitive())
    }
  }

  def isVectorArray(t: Type): Boolean = t match {
    case ArrayType(_, VectorType(_,_)) => true
    case _ => false
  }

  case class isEqualToUntyped(x: ToBeTyped[Expr]) extends Strategy[Rise] {
    override def apply(e: Rise): RewriteResult[Rise] = {
      val p = x.toUntypedExpr
      if (p =~~= e) Success(e) else Failure(isEqualToUntyped(x))
    }
  }
}
