//package FSmooth
//
//object TypeOf {
//  def abstraction(xs: Seq[Identifier], e: Expr): Option[Type] = {
//    val ts = xs.flatMap(_.t)
//    if (ts.length != xs.length) return None // some types are not defined
//    e.t match {
//      case Some(et: ExpressionType) => Some( FunType(ts, et) )
//      case _ => None
//    }
//  }
//
//  def application(f: Expr, es: Seq[Expr]): Option[Type] = {
//    val ts = es.flatMap(_.t)
//    if (ts.length != es.length) return None // some types are not defined
//    (f.t, ts) match {
//      case (Some(FunType(paramTs, outT)), argTs) if paramTs == argTs =>
//        Some(outT)
//      case _ => None
//    }
//  }
//
//  def let(x: Identifier, init: Expr, e: Expr): Option[Type] = {
//    (init.t, e.t) match {
//      case (Some(t1), Some(t2)) if x.t.isDefined && x.t.get == t1 =>
//        Some(t2)
//      case _ => None
//    }
//  }
//
//  def conditional(cond: Expr, thenBranch: Expr, elseBranch: Expr): Option[Type] = {
//    (cond.t, thenBranch.t, elseBranch.t) match {
//      case (Some(Bool), Some(m1), Some(m2)) if m1 == m2 =>
//        Some(m1)
//      case _ => None
//    }
//  }
//}
