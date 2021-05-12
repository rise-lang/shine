package rise.eqsat

object NodeSubs {
  /** Shifts De-Bruijn indices up or down if they are >= cutoff
    * @todo some traversals could be avoided for 0-shifts? */
  def shifted[E](n: Node[E, Nat, DataType], shift: Expr.Shift, cutoff: Expr.Shift)
                (shiftedE: (E, Expr.Shift, Expr.Shift) => E): Node[E, Nat, DataType] =
    n match {
      case Var(index) =>
        val delta = if (index >= cutoff._1) shift._1 else 0
        Var(index + delta)
      case Lambda(e) =>
        Lambda(shiftedE(e, shift, cutoff.copy(_1 = cutoff._1 + 1)))
      case App(f, e) =>
        App(shiftedE(f, shift, cutoff), shiftedE(e, shift, cutoff))
      case NatLambda(e) =>
        NatLambda(shiftedE(e, shift, cutoff.copy(_2 = cutoff._2 + 1)))
      case NatApp(f, x) =>
        NatApp(shiftedE(f, shift, cutoff), x.shifted(shift._2, cutoff._2))
      case DataLambda(e) =>
        DataLambda(shiftedE(e, shift, cutoff.copy(_3 = cutoff._3 + 1)))
      case DataApp(f, x) =>
        DataApp(shiftedE(f, shift, cutoff), x.shifted((shift._2, shift._3), (cutoff._2, cutoff._3)))
      case Literal(_) | Primitive(_) => n

      case Composition(f, g) =>
        Composition(shiftedE(f, shift, cutoff), shiftedE(g, shift, cutoff))
    }

  def replace[E](n: Node[E, Nat, DataType], index: Int, subs: E)
                (makeE: Node[E, Nat, DataType] => E)
                (replaceE: (E, Int, E) => E)
                (shiftedE: (E, Expr.Shift, Expr.Shift) => E): E =
    n match {
      case Var(idx) if idx == index => subs
      case Var(_) | Literal(_) | Primitive(_) => makeE(n)
      case Lambda(e) =>
        // TODO: could shift lazily
        val e2 = replaceE(e, index + 1, shiftedE(subs, (1, 0, 0), (0, 0, 0)))
        makeE(Lambda(e2))
      case App(f, e) =>
        val f2 = replaceE(f, index, subs)
        val e2 = replaceE(e, index, subs)
        makeE(App(f2, e2))
      case NatLambda(e) =>
        // TODO: could shift lazily
        val subs2 = shiftedE(subs, (0, 1, 0), (0, 0, 0))
        makeE(NatLambda(replaceE(e, index, subs2)))
      case NatApp(f, x) =>
        makeE(NatApp(replaceE(f, index, subs), x))
      case DataLambda(e) =>
        // TODO: could shift lazily
        val subs2 = shiftedE(subs, (0, 0, 1), (0, 0, 0))
        makeE(DataLambda(replaceE(e, index, subs2)))
      case DataApp(f, x) =>
        makeE(DataApp(replaceE(f, index, subs), x))

      case Composition(f, g) =>
        makeE(Composition(replaceE(f, index, subs), replaceE(g, index, subs)))
    }

  def replace[E](n: Node[E, Nat, DataType], index: Int, subs: Nat)
                (replaceE: (E, Int, Nat) => E): Node[E, Nat, DataType] =
    n match {
      case _: Var | _: Literal | _: Primitive => n
      case Lambda(e) =>
        Lambda(replaceE(e, index, subs))
      case App(f, e) =>
        val f2 = replaceE(f, index, subs)
        val e2 = replaceE(e, index, subs)
        App(f2, e2)
      case NatLambda(e) =>
        // TODO: could shift lazily
        val e2 = replaceE(e, index + 1, subs.shifted(1, 0))
        NatLambda(e2)
      case NatApp(f, x) =>
        NatApp(replaceE(f, index, subs), x.replace(index, subs))
      case DataLambda(e) =>
        DataLambda(replaceE(e, index, subs))
      case DataApp(f, x) =>
        DataApp(replaceE(f, index, subs), x.replace(index, subs))

      case Composition(f, g) =>
        Composition(replaceE(f, index, subs), replaceE(g, index, subs))
    }
}
