package rise.eqsat

object NodeSubs {
  /** Shifts De-Bruijn indices up or down if they are >= cutoff
    *
    * @todo some traversals could be avoided for 0-shifts? */
  def shifted[E]
  (hc: HashConses, n: Node[E, NatId, DataTypeId],
   shift: Expr.Shift, cutoff: Expr.Shift)
  (shiftedE: (E, Expr.Shift, Expr.Shift) => E): Node[E, NatId, DataTypeId] =
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
        NatApp(shiftedE(f, shift, cutoff),
          Nat.shifted(hc, x, shift._2, cutoff._2))
      case DataLambda(e) =>
        DataLambda(shiftedE(e, shift, cutoff.copy(_3 = cutoff._3 + 1)))
      case DataApp(f, x) =>
        DataApp(shiftedE(f, shift, cutoff),
          DataType.shifted(hc, x, (shift._2, shift._3), (cutoff._2, cutoff._3)))
      case Literal(_) | Primitive(_) => n

      case Composition(f, g) =>
        Composition(shiftedE(f, shift, cutoff), shiftedE(g, shift, cutoff))
    }

  def replace[E](n: Node[E, NatId, DataTypeId], index: Int, subs: E)
                (makeE: Node[E, NatId, DataTypeId] => E)
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

  def replace[E, ED, ND, TD](hc: HashConses, n: Node[E, NatId, DataTypeId],
                             index: Int, subs: NatId)
                            (replaceE: (E, Int, NatId) => E): Node[E, NatId, DataTypeId] =
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
        val e2 = replaceE(e, index + 1, Nat.shifted(hc, subs, 1, 0))
        NatLambda(e2)
      case NatApp(f, x) =>
        NatApp(replaceE(f, index, subs), Nat.replace(hc, x, index, subs))
      case DataLambda(e) =>
        DataLambda(replaceE(e, index, subs))
      case DataApp(f, x) =>
        DataApp(replaceE(f, index, subs), DataType.replace(hc, x, index, subs))

      case Composition(f, g) =>
        Composition(replaceE(f, index, subs), replaceE(g, index, subs))
    }

  object Expr {
    type Shift = rise.eqsat.Expr.Shift

    def shifted(hc: HashConses, e: ExprId,
                shift: Shift, cutoff: Shift): ExprId = {
      val (n, t) = hc(e)
      val n2 = NodeSubs.shifted(hc, n, shift, cutoff){ case (e, s, c) => shifted(hc, e, s, c) }
      val t2 = Type.shifted(hc, t, (shift._2, shift._3), (cutoff._2, cutoff._3))
      hc.add(n2, t2)
    }

    def replace(hc: HashConses, e: ExprId,
                index: Int, subs: ExprId): ExprId = {
      val (n, t) = hc(e)
      NodeSubs.replace(n, index, subs)
      { n => hc.add(n, t) }
      { case (e, i, s) => replace(hc, e, i, s) }
      { case (e, s, c) => shifted(hc, e, s, c) }
    }

    def replace(hc: HashConses, e: ExprId,
                index: Int, subs: NatId): ExprId = {
      val (n, t) = hc(e)
      val n2 = NodeSubs.replace(hc, n, index, subs)
      { case (e, i, s) => replace(hc, e, i, s) }
      val t2 = NodeSubs.Type.replace(hc, t, index, subs)
      hc.add(n2, t2)
    }
  }

  object Nat {
    type Shift = rise.eqsat.Nat.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(hc: HashConses,
                id: NatId, shift: Shift, cutoff: Shift): NatId = {
      hc.add(hc(id) match {
        case NatVar(index) =>
          val delta = if (index >= cutoff) shift else 0
          NatVar(index + delta)
        case NatCst(value) =>
          NatCst(value)
        case NatPosInf => NatPosInf
        case NatNegInf => NatNegInf
        case NatAdd(a, b) =>
          NatAdd(shifted(hc, a, shift, cutoff), shifted(hc, b, shift, cutoff))
        case NatMul(a, b) =>
          NatMul(shifted(hc, a, shift, cutoff), shifted(hc, b, shift, cutoff))
        case NatPow(b, e) =>
          NatPow(shifted(hc, b, shift, cutoff), shifted(hc, e, shift, cutoff))
        case NatMod(a, b) =>
          NatMod(shifted(hc, a, shift, cutoff), shifted(hc, b, shift, cutoff))
        case NatIntDiv(a, b) =>
          NatIntDiv(shifted(hc, a, shift, cutoff), shifted(hc, b, shift, cutoff))
      })
    }

    def replace(hc: HashConses, id: NatId,
                index: Int, subs: NatId): NatId = {
      hc(id) match {
        case NatVar(i) if index == i => subs
        case nv: NatVar => hc.add(nv)
        case other => hc.add(other.map(n => replace(hc, n, index, subs)))
      }
    }
  }

  object DataType {
    type Shift = rise.eqsat.Type.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(hc: HashConses, id: DataTypeId,
                shift: Shift, cutoff: Shift): DataTypeId = {
      hc.add(hc(id) match {
        case DataTypeVar(index) =>
          val delta = if (index >= cutoff._2) shift._2 else 0
          DataTypeVar(index + delta)
        case ScalarType(s) => ScalarType(s)
        case NatType => NatType
        case VectorType(size, elemType) =>
          VectorType(Nat.shifted(hc, size, shift._1, cutoff._1),
            shifted(hc, elemType, shift, cutoff))
        case IndexType(size) =>
          IndexType(Nat.shifted(hc, size, shift._1, cutoff._1))
        case PairType(dt1, dt2) =>
          PairType(shifted(hc, dt1, shift, cutoff),
            shifted(hc, dt2, shift, cutoff))
        case ArrayType(size, elemType) =>
          ArrayType(Nat.shifted(hc, size, shift._1, cutoff._1),
            shifted(hc, elemType, shift, cutoff))
      })
    }

    def replace(hc: HashConses, id: DataTypeId,
                index: Int, subs: NatId): DataTypeId =
      hc.add(hc(id).map(
        n => Nat.replace(hc, n, index, subs),
        dt => replace(hc, dt, index, subs)
      ))

    def replaceDataType(index: Int, subs: DataType): DataType = {
      ???
    }
  }

  object Type {
    type Shift = rise.eqsat.Type.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(hc: HashConses, id: TypeId,
                shift: Shift, cutoff: Shift): TypeId = {
      id match {
        case dt: DataTypeId =>
          DataType.shifted(hc, dt, shift, cutoff)
        case _: NotDataTypeId =>
          hc(id) match {
            case FunType(inT, outT) => hc.add(
              FunType(shifted(hc, inT, shift, cutoff), shifted(hc, outT, shift, cutoff)))
            case NatFunType(t) => hc.add(
              NatFunType(shifted(hc, t, shift, cutoff.copy(_1 = cutoff._1 + 1))))
            case DataFunType(t) => hc.add(
              DataFunType(shifted(hc, t, shift, cutoff.copy(_2 = cutoff._2 + 1))))
            case _: DataTypeNode[NatId, DataTypeId] =>
              throw new Exception("this should not happen")
          }
      }
    }

    def replace(hc: HashConses, id: TypeId,
                index: Int, subs: NatId): TypeId =
      hc.add(hc(id) match {
        case NatFunType(t) =>
          // TODO: could shift lazily
          val t2 = replace(hc, t, index + 1, Nat.shifted(hc, subs, 1, 0))
          NatFunType(t2)
        case other => other.map(
          replace(hc, _, index, subs),
          Nat.replace(hc, _, index, subs),
          DataType.replace(hc, _, index, subs))
      })

    def replace(hc: HashConses, id: TypeId,
                index: Int, subs: DataTypeId): TypeId = {
      ???
    }

    // substitutes %n0 for arg in this
    def withNatArgument(hc: HashConses,
                        body: TypeId, arg: NatId): TypeId = {
      val argS = Nat.shifted(hc, arg, 1, 0)
      val bodyR = replace(hc, body, 0, argS)
      shifted(hc, bodyR, (-1, 0), (0, 0))
    }

    // substitutes %dt0 for arg in this
    def withDataArgument(hc: HashConses,
                         body: TypeId, arg: DataTypeId): TypeId = {
      val argS = DataType.shifted(hc, arg, (0, 1), (0, 0))
      val bodyR = replace(hc, body, 0, argS)
      shifted(hc, bodyR, (0, -1), (0, 0))
    }
  }
}
