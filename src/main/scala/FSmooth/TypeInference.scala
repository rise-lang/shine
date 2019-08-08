package FSmooth

import scala.collection.immutable

//noinspection DuplicatedCode
object TypeInference {
  type Environment = immutable.Map[String, Type]

  private def checkIdentifierAreInBound(e: Expr, env: Environment): Unit = e match {
    case i: Identifier =>
      env.get(i.name) match {
        case Some(_) =>
        case None => throw new Exception(s"Unbound Identifier: $i with env: $env")
      }
    case Abstraction(xs, e, _) =>
      val extendedEnv = env ++ xs.map(i => (i.name, i.t))
      checkIdentifierAreInBound(e, extendedEnv)
    case Application(f, es, _) =>
      checkIdentifierAreInBound(f, env)
      es.foreach(checkIdentifierAreInBound(_, env))
    case Conditional(cond, thenBranch, elseBranch, _) =>
      checkIdentifierAreInBound(cond, env)
      checkIdentifierAreInBound(thenBranch, env)
      checkIdentifierAreInBound(elseBranch, env)
    case Let(x, init, e, _) =>
      val extendedEnv = env ++ Map((x.name, x.t))
      checkIdentifierAreInBound(init, extendedEnv)
      checkIdentifierAreInBound(e, extendedEnv)
    case _: Constants | _: CardinalityValue | _: IndexValue | _: ScalarValue =>
  }

  private case class Constraint(a: Type, b: Type)

  private def collect(e: Expr): Set[Constraint] = e match {
    case _: Identifier => Set.empty
    case Abstraction(xs, e, t) =>
      val ts = xs.map(_.t)
      if (ts.length != xs.length) throw new Exception("This should not happen")
      e.t match {
        case et: ExpressionType =>
          collect(e) ++ Set(Constraint(t, FunType(ts.reduce(IncompleteFunType), et)))
        case tv: TypeVar =>
          collect(e) ++ Set(Constraint(t, FunType(ts.reduce(IncompleteFunType), ExpressionTypeVar(tv.name))))
        case _ => throw new Exception("This should not happen")
      }
    case Application(f, es, t) =>
      val et = t match {
        case et: ExpressionType => et
        case tv: TypeVar => ExpressionTypeVar(tv.name)
        case _ =>  throw new Exception("This should not happen")
      }
      val ts = es.map(_.t)
      if (ts.length != es.length) throw new Exception("This should not happen")
      es.map(collect).foldLeft(collect(f))(_ ++ _) ++ Set(
        Constraint(f.t, FunType(ts.reduce(IncompleteFunType), et))
      )
    case Conditional(cond, thenBranch, elseBranch, t) =>
      collect(cond) ++ collect(thenBranch) ++ collect(elseBranch) ++ Set(
        Constraint(cond.t, Bool),
        Constraint(thenBranch.t, t),
        Constraint(elseBranch.t, t)
      )
    case Let(x, init, e, t) =>
      collect(init) ++ collect(e) ++ Set(
        Constraint(t, e.t),
        Constraint(x.t, init.t)
      )
    case c: Constants => Set(Constraint(c.typeScheme, c.t))
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
      case Identifier(name, t) => Identifier(name, apply(t))
      case Abstraction(xs, e, t) => Abstraction(xs.map(i => apply(i).asInstanceOf[Identifier]), apply(e), apply(t))
      case Application(f, es, t) => Application(apply(f), es.map(apply), apply(t))
      case _: ScalarValue | _: IndexValue | _: CardinalityValue => e
      case c: Constants => c.copyWithType(apply(c.t))
      case Let(x, init, e, t) => Let(apply(x).asInstanceOf[Identifier], apply(init), apply(e), apply(t))
      case Conditional(cond, thenBranch, elseBranch, t) => Conditional(apply(cond), apply(thenBranch), apply(elseBranch), apply(t))
    }

    def substitute(ty: Type, tv: TypeVar, replacement: Type): Type = {
      ty match {
        case _: Num | Bool => ty
        case FunType(inT, outT) =>
          FunType(
            substitute(inT, tv, replacement),
            substitute(outT, tv, replacement).asInstanceOf[ExpressionType])
        case IncompleteFunType(inT, outT) =>
          IncompleteFunType(
            substitute(inT, tv, replacement),
            substitute(outT, tv, replacement))
        case Array(elemType) =>
          Array(substitute(elemType, tv, replacement).asInstanceOf[ExpressionType])
        case Pair(fst, snd) =>
          Pair(substitute(fst, tv, replacement).asInstanceOf[ExpressionType], substitute(snd, tv, replacement).asInstanceOf[ExpressionType])
        case tv2: TypeVar =>
          if (tv == tv2) replacement else ty
        case tv2: ExpressionTypeVar =>
          if (tv.name == tv2.name) {
            replacement match {
              case e: ExpressionType => e
              case _ => ty
            }
          } else ty
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
      case (FunType(t1, r1), FunType(t2, r2)) =>
        unify(Set(
          Constraint(t1, t2),
          Constraint(r1, r2)))
      case (IncompleteFunType(t1, r1), IncompleteFunType(t2, r2)) =>
        unify(Set(
          Constraint(t1, t2),
          Constraint(r1, r2)))
      case (IncompleteFunType(t1, r1: ExpressionType), FunType(t2, r2)) =>
        unify(Set(
          Constraint(t1, t2),
          Constraint(r1, r2)))
      case (FunType(t1, r1), IncompleteFunType(t2, r2: ExpressionType)) =>
        unify(Set(
          Constraint(t1, t2),
          Constraint(r1, r2)))
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
      case FunType(p, r) => occurs(tv, p) || occurs(tv, r)
      case TypeVar(tv2) => tv == tv2
      case ExpressionTypeVar(tv2) => tv == tv2
      case _ => false
    }
  }

  def infer(e: Expr, gamma: Environment = Map()): Expr = {
    checkIdentifierAreInBound(e, gamma)
    val constraints = collect(e)
    val subst = unify(constraints)
    subst(e)
  }
}
