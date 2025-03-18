package rise.eqsat
import rise.eqsat.NatLiteral

object NodeSubs {
  /** Shifts De-Bruijn indices up or down if they are >= cutoff
    *
    * @todo some traversals could be avoided for 0-shifts? */
  def shifted[E]
  (egraph: EGraph, n: Node[E, NatId, DataTypeId, Address],
   shift: Expr.Shift, cutoff: Expr.Shift)
  (shiftedE: (E, Expr.Shift, Expr.Shift) => E): Node[E, NatId, DataTypeId, Address] =
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
          Nat.shifted(egraph, x, shift._2, cutoff._2))
      case DataLambda(e) =>
        DataLambda(shiftedE(e, shift, cutoff.copy(_3 = cutoff._3 + 1)))
      case DataApp(f, x) =>
        DataApp(shiftedE(f, shift, cutoff),
          DataType.shifted(egraph, x, (shift._2, shift._3), (cutoff._2, cutoff._3)))
      case AddrLambda(e) =>
        AddrLambda(shiftedE(e, shift, cutoff.copy(_4 = cutoff._4 + 1)))
      case AddrApp(f, x) =>
        AddrApp(shiftedE(f, shift, cutoff),
          Address.shifted(x, shift._4, cutoff._4))
      case NatLiteral(n) => NatLiteral(Nat.shifted(egraph, n, shift._2, cutoff._2))
      case IndexLiteral(i, n) => IndexLiteral(
        Nat.shifted(egraph, i, shift._2, cutoff._2),
        Nat.shifted(egraph, n, shift._2, cutoff._2))
      case Literal(_) | Primitive(_) => n

      case Composition(f, g) =>
        Composition(shiftedE(f, shift, cutoff), shiftedE(g, shift, cutoff))
    }

  def replace[E](n: Node[E, NatId, DataTypeId, Address], index: Int, subs: E)
                (makeE: Node[E, NatId, DataTypeId, Address] => E)
                (replaceE: (E, Int, E) => E)
                (shiftedE: (E, Expr.Shift, Expr.Shift) => E): E =
    n match {
      case Var(idx) if idx == index => subs
      case Var(_) | Literal(_) | NatLiteral(_) | IndexLiteral(_, _) | Primitive(_) => makeE(n)
      case Lambda(e) =>
        // TODO: could shift lazily
        val e2 = replaceE(e, index + 1, shiftedE(subs, (1, 0, 0, 0), (0, 0, 0, 0)))
        makeE(Lambda(e2))
      case App(f, e) =>
        val f2 = replaceE(f, index, subs)
        val e2 = replaceE(e, index, subs)
        makeE(App(f2, e2))
      case NatLambda(e) =>
        // TODO: could shift lazily
        val subs2 = shiftedE(subs, (0, 1, 0, 0), (0, 0, 0, 0))
        makeE(NatLambda(replaceE(e, index, subs2)))
      case NatApp(f, x) =>
        makeE(NatApp(replaceE(f, index, subs), x))
      case DataLambda(e) =>
        // TODO: could shift lazily
        val subs2 = shiftedE(subs, (0, 0, 1, 0), (0, 0, 0, 0))
        makeE(DataLambda(replaceE(e, index, subs2)))
      case DataApp(f, x) =>
        makeE(DataApp(replaceE(f, index, subs), x))
      case AddrLambda(e) =>
        // TODO: could shift lazily
        val subs2 = shiftedE(subs, (0, 0, 0, 1), (0, 0, 0, 0))
        makeE(AddrLambda(replaceE(e, index, subs2)))
      case AddrApp(f, x) =>
        makeE(AddrApp(replaceE(f, index, subs), x))

      case Composition(f, g) =>
        makeE(Composition(replaceE(f, index, subs), replaceE(g, index, subs)))
    }

  def replace[E, ED, ND, TD](egraph: EGraph, n: Node[E, NatId, DataTypeId, Address],
                             index: Int, subs: NatId)
                            (replaceE: (E, Int, NatId) => E): Node[E, NatId, DataTypeId, Address] =
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
        val e2 = replaceE(e, index + 1, Nat.shifted(egraph, subs, 1, 0))
        NatLambda(e2)
      case NatApp(f, x) =>
        NatApp(replaceE(f, index, subs), Nat.replace(egraph, x, index, subs))
      case DataLambda(e) =>
        DataLambda(replaceE(e, index, subs))
      case DataApp(f, x) =>
        DataApp(replaceE(f, index, subs), DataType.replace(egraph, x, index, subs))
      case AddrLambda(e) =>
        AddrLambda(replaceE(e, index, subs))
      case AddrApp(f, x) =>
        AddrApp(replaceE(f, index, subs), x)
      case NatLiteral(n) =>
        NatLiteral(Nat.replace(egraph, n, index, subs))
      case IndexLiteral(i, n) =>
        IndexLiteral(Nat.replace(egraph, i, index, subs), Nat.replace(egraph, n, index, subs))

      case Composition(f, g) =>
        Composition(replaceE(f, index, subs), replaceE(g, index, subs))
    }

  object Nat {
    type Shift = rise.eqsat.Nat.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(egraph: EGraph,
                id: NatId, shift: Shift, cutoff: Shift): NatId = {
      egraph.add(egraph(id) match {
        case NatVar(index) =>
          val delta = if (index >= cutoff) shift else 0
          NatVar(index + delta)
        case NatCst(value) =>
          NatCst(value)
        case NatPosInf => NatPosInf
        case NatNegInf => NatNegInf
        case NatAdd(a, b) =>
          NatAdd(shifted(egraph, a, shift, cutoff), shifted(egraph, b, shift, cutoff))
        case NatMul(a, b) =>
          NatMul(shifted(egraph, a, shift, cutoff), shifted(egraph, b, shift, cutoff))
        case NatPow(b, e) =>
          NatPow(shifted(egraph, b, shift, cutoff), shifted(egraph, e, shift, cutoff))
        case NatMod(a, b) =>
          NatMod(shifted(egraph, a, shift, cutoff), shifted(egraph, b, shift, cutoff))
        case NatIntDiv(a, b) =>
          NatIntDiv(shifted(egraph, a, shift, cutoff), shifted(egraph, b, shift, cutoff))
      })
    }

    def replace(egraph: EGraph, id: NatId,
                index: Int, subs: NatId): NatId = {
      egraph(id) match {
        case NatVar(i) if index == i => subs
        case nv: NatVar => egraph.add(nv)
        case other => egraph.add(other.map(n => replace(egraph, n, index, subs)))
      }
    }

    def replace(egraph: EGraph, id: NatId,
                index: Int, subs: DataTypeId): NatId = id // nats cannot contain datatypes
  }

  object DataType {
    type Shift = rise.eqsat.Type.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(egraph: EGraph, id: DataTypeId,
                shift: Shift, cutoff: Shift): DataTypeId = {
      egraph.add(egraph(id) match {
        case DataTypeVar(index) =>
          val delta = if (index >= cutoff._2) shift._2 else 0
          DataTypeVar(index + delta)
        case ScalarType(s) => ScalarType(s)
        case NatType => NatType
        case VectorType(size, elemType) =>
          VectorType(Nat.shifted(egraph, size, shift._1, cutoff._1),
            shifted(egraph, elemType, shift, cutoff))
        case IndexType(size) =>
          IndexType(Nat.shifted(egraph, size, shift._1, cutoff._1))
        case PairType(dt1, dt2) =>
          PairType(shifted(egraph, dt1, shift, cutoff),
            shifted(egraph, dt2, shift, cutoff))
        case ArrayType(size, elemType) =>
          ArrayType(Nat.shifted(egraph, size, shift._1, cutoff._1),
            shifted(egraph, elemType, shift, cutoff))
      })
    }

    def replace(egraph: EGraph, id: DataTypeId,
                index: Int, subs: NatId): DataTypeId =
      egraph.add(egraph(id).map(
        n => Nat.replace(egraph, n, index, subs),
        dt => replace(egraph, dt, index, subs)
      ))

    def replace(egraph: EGraph, id: DataTypeId,
                index: Int, subs: DataTypeId): DataTypeId =
      egraph(id) match {
        case DataTypeVar(i) if i == index => subs
        case dtv: DataTypeVar => egraph.add(dtv)
        case other => egraph.add(other.map(
          Nat.replace(egraph, _, index, subs),
          replace(egraph, _, index, subs)
        ))
      }

    def replaceDataType(index: Int, subs: DataType): DataType = {
      ???
    }
  }

  object Type {
    type Shift = rise.eqsat.Type.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(egraph: EGraph, id: TypeId,
                shift: Shift, cutoff: Shift): TypeId = {
      id match {
        case dt: DataTypeId =>
          DataType.shifted(egraph, dt, shift, cutoff)
        case _: NotDataTypeId =>
          egraph(id) match {
            case FunType(inT, outT) => egraph.add(
              FunType(shifted(egraph, inT, shift, cutoff), shifted(egraph, outT, shift, cutoff)))
            case NatFunType(t) => egraph.add(
              NatFunType(shifted(egraph, t, shift, cutoff.copy(_1 = cutoff._1 + 1))))
            case DataFunType(t) => egraph.add(
              DataFunType(shifted(egraph, t, shift, cutoff.copy(_2 = cutoff._2 + 1))))
            case AddrFunType(t) => egraph.add(
              // note: addresses don't appear in types
              AddrFunType(shifted(egraph, t, shift, cutoff)))
            case _: DataTypeNode[NatId, DataTypeId] =>
              throw new Exception("this should not happen")
          }
      }
    }

    def replace(egraph: EGraph, id: TypeId,
                index: Int, subs: NatId): TypeId =
      egraph.add(egraph(id) match {
        case NatFunType(t) =>
          // TODO: could shift lazily
          val t2 = replace(egraph, t, index + 1, Nat.shifted(egraph, subs, 1, 0))
          NatFunType(t2)
        case other => other.map(
          replace(egraph, _, index, subs),
          Nat.replace(egraph, _, index, subs),
          DataType.replace(egraph, _, index, subs))
      })

    def replace(egraph: EGraph, id: TypeId,
                index: Int, subs: DataTypeId): TypeId =
      id match {
        case dt: DataTypeId => DataType.replace(egraph, dt, index, subs)
        case _: NotDataTypeId => egraph.add(egraph(id) match {
          case DataFunType(t) =>
            val t2 = replace(egraph, t, index + 1, DataType.shifted(egraph, subs, (0, 1), (0, 0)))
            DataFunType(t2)
          case _: DataTypeNode[NatId, DataTypeId] =>
            throw new Exception("this should not happen")
          case other => other.map(
            replace(egraph, _, index, subs),
            Nat.replace(egraph, _, index, subs),
            DataType.replace(egraph, _, index, subs))
        })
      }

    // substitutes %n0 for arg in this
    def withNatArgument(egraph: EGraph,
                        body: TypeId, arg: NatId): TypeId = {
      val argS = Nat.shifted(egraph, arg, 1, 0)
      val bodyR = replace(egraph, body, 0, argS)
      shifted(egraph, bodyR, (-1, 0), (0, 0))
    }

    // substitutes %dt0 for arg in this
    def withDataArgument(egraph: EGraph,
                         body: TypeId, arg: DataTypeId): TypeId = {
      val argS = DataType.shifted(egraph, arg, (0, 1), (0, 0))
      val bodyR = replace(egraph, body, 0, argS)
      shifted(egraph, bodyR, (0, -1), (0, 0))
    }
  }

  object Address {
    type Shift = rise.eqsat.Address.Shift

    /** Shifts DeBruijn indices up or down if they are >= cutoff */
    def shifted(a: Address, shift: Shift, cutoff: Shift): Address = {
      a match {
        case AddressVar(index) =>
          val delta = if (index >= cutoff) shift else 0
          AddressVar(index + delta)
        case Global | Local | Private | Constant => a
      }
    }

    def replace(a: Address, index: Int, subs: Address): Address = {
      a match {
        case AddressVar(i) if i == index => subs
        case AddressVar(_) => a
        case Global | Local | Private | Constant => a
      }
    }
  }
}
