package FSmooth

import FSmooth.DSL.freshName

import scala.collection.immutable

//noinspection DuplicatedCode
object TypeInference {
  type Environment = immutable.Map[String, Type]

  private def annotate(e: Expr, env: Environment): Expr = e match {
    case i: Identifier =>
      env.get(i.name) match {
        case Some(t) => Identifier(i.name, Some(t))
        case None => throw new Exception(s"Unbound Identifier: $i with env: $env")
      }
    case Abstraction(xs, e, None) =>
      val newXs = xs.map(i => Identifier(i.name, Some(TypeVar(freshName("T")))))
      val extendedEnv = env ++ newXs.map(i => (i.name, i.t.get))
      Abstraction(newXs, annotate(e, extendedEnv), Some(TypeVar(freshName("T"))))
    case Application(f, es, None) =>
      Application(annotate(f, env), es.map(annotate(_, env)), Some(TypeVar(freshName("T"))))
    case Conditional(cond, thenBranch, elseBranch, None) =>
      Conditional(annotate(cond, env), annotate(thenBranch, env), annotate(elseBranch, env), Some(TypeVar(freshName("T"))))
    case Let(x, init, e, None) =>
      val newX = Identifier(x.name, Some(TypeVar(freshName("T"))))
      val extendedEnv = env ++ Map((newX.name, newX.t.get))
      Let(newX, annotate(init, extendedEnv), annotate(e, extendedEnv), Some(TypeVar(freshName("T"))))
    case c: Constants => c.copy(TypeVar("freshName"))
    case _: CardinalityValue | _: IndexValue | _: ScalarValue => e
  }

  private case class Constraint(a: Type, b: Type)

  private def collect(e: Expr): Set[Constraint] = e match {
    case _: Identifier => Set.empty
    case Abstraction(xs, e, Some(t)) =>
      val ts = xs.flatMap(_.t)
      if (ts.length != xs.length) throw new Exception("This should not happen")
      e.t match {
        case Some(et: ExpressionType) =>
          collect(e) ++ Set(Constraint(t, FunType(ts, et)))
        case Some(tv: TypeVar) =>
          collect(e) ++ Set(Constraint(t, FunType(ts, ExpressionTypeVar(tv.name))))
        case _ => throw new Exception("This should not happen")
      }
    case Application(f, es, Some(t)) =>
      val et = t match {
        case et: ExpressionType => et
        case tv: TypeVar => ExpressionTypeVar(tv.name)
        case _ =>  throw new Exception("This should not happen")
      }
      val ts = es.flatMap(_.t)
      if (ts.length != es.length) throw new Exception("This should not happen")
      es.map(collect).foldLeft(collect(f))(_ ++ _) ++ Set(
        Constraint(f.t.get, FunType(ts, et))
      )
    case Conditional(cond, thenBranch, elseBranch, Some(t)) =>
      collect(cond) ++ collect(thenBranch) ++ collect(elseBranch) ++ Set(
        Constraint(cond.t.get, Bool),
        Constraint(thenBranch.t.get, t),
        Constraint(elseBranch.t.get, t)
      )
    case Let(x, init, e, Some(t)) =>
      collect(init) ++ collect(e) ++ Set(
        Constraint(t, e.t.get),
        Constraint(x.t.get, init.t.get)
      )
    case c: Constants => Set(Constraint(c.typeScheme, c.t.get))
    case _: CardinalityValue | _: IndexValue | _: ScalarValue => Set()
  }

  private case class Substitution(solutions: Map[TypeVar, Type]) {
    def apply(constraints: Set[Constraint]): Set[Constraint] = {
      constraints.map(c => apply(c))
    }

    def apply(constraint: Constraint): Constraint = {
      Constraint(
        apply(constraint.a),
        apply(constraint.b)
      )
    }

    def apply(t: Type): Type = {
      solutions.foldLeft(t) { (result, solution) =>
        val (tvar, solutionType) = solution
        substitute(result, tvar, solutionType)
      }
    }

    def apply(e: Expr): Expr = e match {
      case Identifier(name, t) => Identifier(name, t.map(apply))
      case Abstraction(xs, e, t) => Abstraction(xs.map(i => apply(i).asInstanceOf[Identifier]), apply(e), t.map(apply))
      case Application(f, es, t) => Application(apply(f), es.map(apply), t.map(apply))
      case _: ScalarValue | _: IndexValue | _: CardinalityValue => e
      case c: Constants => c.copy(apply(c.t.get))
      case Let(x, init, e, t) => Let(apply(x).asInstanceOf[Identifier], apply(init), apply(e), t.map(apply))
      case Conditional(cond, thenBranch, elseBranch, t) => Conditional(apply(cond), apply(thenBranch), apply(elseBranch), t.map(apply))
    }

    def substitute(ty: Type, tv: TypeVar, replacement: Type): Type = {
      ty match {
        case _: Num | Bool => ty
        case FunType(inTs, outT) =>
          FunType(
            inTs.map(substitute(_, tv, replacement)),
            substitute(outT, tv, replacement)
          )
        case Array(elemType) =>
          Array(substitute(elemType, tv, replacement).asInstanceOf[ExpressionType])
        case Pair(fst, snd) =>
          Pair(substitute(fst, tv, replacement).asInstanceOf[ExpressionType], substitute(snd, tv, replacement).asInstanceOf[ExpressionType])
        case tv2: TypeVar =>
          if (tv == tv2) replacement else ty
        case tv2: ExpressionTypeVar =>
          if (tv.name == tv2.name) replacement else ty
      }
    }

    def compose(other: Substitution): Substitution = {
      val substitutedThis = solutions.mapValues(s => other.apply(s))
      Substitution(substitutedThis ++ other.solutions)
    }
  }

  private object Substitution {
    def empty = Substitution(Map.empty)

    def fromPair(tvar: TypeVar, ty: Type): Substitution = {
      Substitution(Map((tvar, ty)))
    }
  }

  private def unify(constraints: Set[Constraint]): Substitution = {
    if (constraints.isEmpty) {
      Substitution.empty
    } else {
      val subst: Substitution = unifyOne(constraints.head)
      val substitutionTail = subst.apply(constraints.tail)
      val substTail: Substitution = unify(substitutionTail)
      subst.compose(substTail)
    }
  }

  private def unifyOne(constraint: Constraint): Substitution = {
    (constraint.a, constraint.b) match {
      case (Double, Double) => Substitution.empty
      case (Index, Index) => Substitution.empty
      case (Card, Card) => Substitution.empty
      case (Bool, Bool) => Substitution.empty
      case (FunType(ts1, r1), FunType(ts2, r2)) if ts1.length == ts2.length =>
        unify(
          (ts1 zip ts2).map(p => Constraint(p._1, p._2)).toSet ++
          Set(Constraint(r1, r2)))
      case (FunType(ts1, r1), FunType(Seq(tv: TypeVar), r2)) =>
        unify(
          Set(Constraint(
            ts1.tail.foldLeft(ts1.head){ (t1, t2) =>
              FunType(Seq(t1), t2)
            }, tv),
            Constraint(r1, r2))
        )
      case (Array(t1), Array(t2)) => unify(Set(Constraint(t1, t2)))
      case (Pair(f1, s1), Pair(f2, s2)) => unify(Set(Constraint(f1, f2), Constraint(s2, s2)))
      case (TypeVar(tv), ty) => unifyVar(tv, ty)
      case (ty, TypeVar(tv)) => unifyVar(tv, ty)
      case (ExpressionTypeVar(tv), ty) => unifyVar(tv, ty)
      case (ty, ExpressionTypeVar(tv)) => unifyVar(tv, ty)
      case (a, b) => throw new Exception(s"cannot unify $a with $b")
    }
  }

  private def unifyVar(tv: String, ty: Type): Substitution = {
    ty match {
      case TypeVar(tv2) if tv == tv2 => Substitution.empty
      case ExpressionTypeVar(tv2) if tv == tv2 => Substitution.empty
      case TypeVar(_) => Substitution.fromPair(TypeVar(tv), ty)
      case ExpressionTypeVar(_) => Substitution.fromPair(TypeVar(tv), ty)
      case ty if occurs(tv, ty) =>
        throw new Exception(s"circular use: $tv opccurs in $ty")
      case ty => Substitution.fromPair(TypeVar(tv), ty)
    }
  }

  private def occurs(tv: String, ty: Type): Boolean = {
    ty match {
      case FunType(ps, r) => ps.exists(occurs(tv, _)) || occurs(tv, r)
      case TypeVar(tv2) => tv == tv2
      case ExpressionTypeVar(tv2) => tv == tv2
      case _ => false
    }
  }

  def infer(e: Expr, gamma: Environment = Map()): Expr = {
    val annotatedExpr = annotate(e, gamma)
    val constraints = collect(annotatedExpr)
    val subst = unify(constraints)
    subst(annotatedExpr)
  }

//  def infer[E <: Expr](e: E, gamma: Environment): E = e match {
//    case i@Identifier(_, mt) =>
//      try {
//        val t = gamma(i)
//        if (mt.exists(_ != t)) {
//          throw new Exception(s"Type mismatch: $t vs. ${mt.get}")
//        }
//        Identifier(i.name, Some(t)).asInstanceOf[E]
//      } catch {
//        case _: NoSuchElementException => throw new Exception("Undefined Identifier")
//      }
//
//    case Abstraction(xs, e) =>
//      val newXs = xs.map(infer(_, gamma))
//      val newE = infer(e, gamma ++ newXs.map(i => (i, i.t.get) ))
//      Abstraction(newXs, newE).asInstanceOf[E]
//
//    case Application(f, es) =>
//      Application(infer(f, gamma), es.map(infer(_, gamma))).asInstanceOf[E]
//
//    case Conditional(cond, thenBranch, elseBranch) =>
//      Conditional(infer(cond, gamma), infer(thenBranch, gamma), infer(elseBranch, gamma)).asInstanceOf[E]
//
//    case c: Constants => ???
//
//    case Let(x, init, e) =>
//
//
//    case _: CardinalityValue | _: IndexValue | _: ScalarValue => e
//  }


}
