package rise.eqsat

/* 
 This package contains features to translate Rise expressions and rewrites
 to an egg-compatible language living in Rust.

 see https://github.com/Bastacyclop/reggvolution

 */
object Reggvolution {
  def sym(s: String): String = s
    // s"""sym("$s")"""

  // cascade of apps bearing no types, used to encode many language constructs
  // as simple symbol applications
  def noTyApp(f: String, args: Iterable[String]): String = {
    args.foldLeft(f) { case (acc, arg) =>
      s"(app $acc $arg)"
      // s"App([$acc, $arg])"
    }
  }

  type Shift = rise.eqsat.Expr.Shift
  
  def reggvolve(expr: Expr): String =
    // NOTE: could define reggvolution for generic nodes, but this is simpler 
    reggvolve(Pattern.fromExpr(expr))

  // same as NamedRewrite.init, but flattens DeBruijn indices from different kinds.
  // TODO: could factorize even more
  def reggvolveNamedRewrite(
    name: String,
    rule: (NamedRewriteDSL.Pattern, NamedRewriteDSL.Pattern),
    parameters: Seq[NamedRewrite.Parameter] = Seq(),
  ): String = {
    import rise.core.DSL.infer
    import arithexpr.{arithmetic => ae}
    import rise.eqsat.NamedRewrite._
    import rise.core.types.DataKind.IDWrapper
    import rise.{core => rc}
    import rise.core.{types => rct}
    import rise.core.types.{DataType => rcdt}

    val (typedLhs, freeV, freeT, typedRhs) = typeRule(rule, parameters)

    type FlatShift = Int
    val patVars: PatternVarMap[FlatShift, PatternVar] = HashMap()
    val natPatVars: PatternVarMap[FlatShift, NatPatternVar] = HashMap()
    val dataTypePatVars: PatternVarMap[FlatShift, DataTypePatternVar] = HashMap()
    val typePatVars: PatternVarMap[FlatShift, TypePatternVar] = HashMap()
    val addrPatVars: PatternVarMap[FlatShift, AddressPatternVar] = HashMap()

    // nats which we need to pivot to avoid matching over certain nat constructs
    val natsToPivot = Vec[(rct.Nat, rct.NatIdentifier, FlatShift, NatPatternVar)]()

    val boundVarToShift = HashMap[String, FlatShift]()

    def shiftOfBound(bound: Expr.Bound): FlatShift =
      bound.expr.size + bound.nat.size + bound.data.size + bound.addr.size + bound.n2n.size

    val lhsPat = makePat(typedLhs, Expr.Bound.empty, isRhs = false,
      freeV, freeT,
      shiftOfBound, shiftOfBound, shiftOfBound, shiftOfBound,
      patVars, natPatVars, dataTypePatVars, typePatVars, addrPatVars,
      natsToPivot, boundVarToShift)
    val rhsPat = makePat(typedRhs, Expr.Bound.empty, isRhs = true,
      freeV, freeT,
      shiftOfBound, shiftOfBound, shiftOfBound, shiftOfBound,
      patVars, natPatVars, dataTypePatVars, typePatVars, addrPatVars,
      natsToPivot, boundVarToShift)

    def patMkShift(s1: FlatShift, pv1: Any)
                  (s2: FlatShift, pv2: Any)
                  (applier: String): String = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      s"""shifted("${pv1}", "${pv2}", ${shift}, ${cutoff}, ${applier})"""
    }

    def patMkShiftCheck(s1: FlatShift, pv1: Any)
                       (s2: FlatShift, pv2: Any)
                       (applier: String): String = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      s"""shifted_check("${pv1}", "${pv2}", ${shift}, ${cutoff}, ${applier})"""
    }

    def mkComputeNatCheck(pv: NatPatternVar, valuePat: NatPattern, applier: String): String = {
      val vp = reggvolve(valuePat)
      s"""compute_nat_check("${pv}", "${vp}", ${applier})"""
    }

    def mkComputeNat(pv: NatPatternVar, valuePat: NatPattern, applier: String): String = {
      val vp = reggvolve(valuePat)
      s"""compute_nat("${pv}", "${vp}", ${applier})"""
    }

    val searcher: String = s""""${reggvolve(lhsPat)}""""
    val param = parameters.foldRight((a: String) => a) { case (c, acc) =>
      c match {
        case NotFreeIn(notFree, in) =>
          val nfShift = boundVarToShift.getOrElse(notFree, 0)
          // all left-hand-side uses of `in` may contain `notFree`
          assert(patVars(in).forall {
            case (shift, (_, status)) =>
              shift >= nfShift || status != Known
          })
          // pick one of these uses
          val (iS, iPV) = patVars(in).collectFirst {
            case (s, (pv, Known)) => (s, pv)
          }.get
          val nfIndex = iS - nfShift // >= 0 because iS >= nfShift
          (a: String) => s"""not_free_in("${iPV}", ${nfIndex}, ${a})"""
            // NotFreeInApplier(iPV, nfIndex, acc(a))
        case VectorizeScalarFun(f, n, fV) =>
          val (nPV, nST) = natPatVars(n)(0)
          assert(nST == Known)
          val (fPV, fST) = patVars(f)(0)
          assert(fST == Known)
          val fVPV = makePatVar(fV, 0, patVars, PatternVar, Known)
          (a: String) => s"""vectorize_scalar_fun("${fPV}", "${nPV}", "${fVPV}", ${a})"""
            // VectorizeScalarFunExtractApplier(fPV, nPV, fVPV, acc(a))
      }
    }
    val rhsPatApplier = s"""pat("${reggvolve(rhsPat)}")"""
    val shiftPV = shiftAppliers(patVars, patMkShift, patMkShiftCheck)
    val shiftNPV = shiftAppliers(natPatVars, patMkShift, patMkShiftCheck)
    val shiftDTPV = shiftAppliers(dataTypePatVars, patMkShift, patMkShiftCheck)
    val shiftTPV = shiftAppliers(typePatVars, patMkShift, patMkShiftCheck)
    val shiftAPV = shiftAppliers(addrPatVars, patMkShift, patMkShiftCheck)
    val pivotNPV = pivotNats(natsToPivot.toSeq, natPatVars, patMkShift, patMkShiftCheck, mkComputeNatCheck, mkComputeNat)
    val applier = param(shiftPV(shiftNPV(shiftDTPV(shiftTPV(shiftAPV(pivotNPV(rhsPatApplier)))))))

    def allIsShiftCoherent[S, V](pvm: PatternVarMap[S, V]): Boolean =
      pvm.forall { case (_, shiftMap) =>
        shiftMap.forall { case (_, (_, status)) => status == ShiftCoherent }}
    assert(allIsShiftCoherent(patVars))
    assert(allIsShiftCoherent(natPatVars))
    assert(allIsShiftCoherent(dataTypePatVars))
    assert(allIsShiftCoherent(typePatVars))
    assert(allIsShiftCoherent(addrPatVars))

    s"""rewrite!("${name}"; ${searcher} => { ${applier}) }"""
  }

  // DEPRECATED:
  // def reggvolve(searcher: Searcher): String =
  // def reggvolve(applier: Applier): String =

  def reggvolve(pat: Pattern): String =
    reggvolve(pat, (0, 0, 0, 0, 0))

  def reggvolve(pat: NatPattern): String =
    reggvolve(pat, (0, 0, 0, 0, 0))

  def reggvolve(pat: Pattern, s: Shift): String = {
    val e = pat.p match {
      case PatternVar(index) => s"?e${index}"
      case PatternNode(node) => node match {
        case Var(index) => s"%${index + s._1}"
          // s"Var(${index + s._1})"
        case App(f, e) => s"(app ${reggvolve(f, s)} ${reggvolve(e, s)})"
          // s"App([${reggvolve(f, s)}, ${reggvolve(e, s)}])"
        case NatApp(f, x) => s"(app ${reggvolve(f, s)} ${reggvolve(x, s)})"
        case DataApp(f, x) => s"(app ${reggvolve(f, s)} ${reggvolve(x, s)})"
        case AddrApp(f, x) => s"(app ${reggvolve(f, s)} ${reggvolve(x, s)})"
        case AppNatToNat(f, x) => s"(app ${reggvolve(f, s)} ${reggvolve(x, s)})"
        case Lambda(e) =>
          val s2 = (s._1, s._2 + 1, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case NatLambda(e) =>
          val s2 = (s._1 + 1, s._2, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case DataLambda(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case AddrLambda(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case LambdaNatToNat(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4 + 1, s._5)
          s"(lam ${reggvolve(e, s)})"
        case Literal(d) =>
          import rise.core.semantics._

          d match {
            case BoolData(true) => sym("true")
            case BoolData(false) => sym("false")
            case IntData(i) => i.toString() // s"Integer($i)"
            case FloatData(f) => f.toString() // s"Float($f)"
            case DoubleData(d) => d.toString() // s"Double($d)"
            case _=> throw new Exception(s"not supporting literal $d yet")
          }
        case NatLiteral(n) => reggvolve(n, s)
        case IndexLiteral(i, n) => noTyApp(sym("idxL"), List(i, n).map(reggvolve(_, s)))
        case Primitive(p) => sym(p.name)
        case Composition(f, g) => ???
      }
    }
    val t = reggvolve(pat.t, s)
    // s"TypeOf([$e, $t])"
    s"(typeOf $e $t)"
  }

  def reggvolve(ty: TypePattern, s: Shift): String = {
    ty match {
      case TypePatternVar(index) => s"?t${index}"
      case DataTypePatternVar(index) => s"?dt${index}"
      case TypePatternAny => "?"
      case DataTypePatternAny => "?"
      case TypePatternNode(n) => n match {
        case dt: DataTypeNode[_, _] =>
          reggvolve(rise.eqsat.DataTypePatternNode(dt), s)
        case FunType(a, b) => noTyApp(sym("fun"), List(a, b).map(reggvolve(_, s)))
        // TODO: do we need to remember the arg kind as a type ?
        case NatFunType(t) =>
          val s2 = (s._1 + 1, s._2, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case DataFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case AddrFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case NatToNatFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4 + 1, s._5)
          s"(lam ${reggvolve(t, s2)})"
      }
      // FIXME: this construct is redundant ???
      case dtn: DataTypePatternNode => reggvolve(dtn, s)
    }
  }
  
  def reggvolve(n: NatPattern, s: Shift): String = {
    n match {
      case NatPatternVar(index) => s"?n${index}"
      case NatPatternAny => "?"
      case NatPatternNode(n) => n match {
        case NatVar(index) => s"%${index + s._2}"
        case NatCst(value) => value.toString()
        case NatNegInf => ???
        case NatPosInf => ???
        case NatAdd(a, b) => noTyApp(sym("add"), List(a, b).map(reggvolve(_, s)))
        case NatMul(a, b) => noTyApp(sym("mul"), List(a, b).map(reggvolve(_, s)))
        case NatPow(a, b) => noTyApp(sym("pow"), List(a, b).map(reggvolve(_, s)))
        case NatMod(a, b) => noTyApp(sym("mod"), List(a, b).map(reggvolve(_, s)))
        case NatIntDiv(a, b) => noTyApp(sym("floorDiv"), List(a, b).map(reggvolve(_, s)))
        case NatToNatApp(f, n) => ???
      }
    }
  }

  def reggvolve(dty: DataTypePatternNode, s: Shift): String = {
    dty.n match {
      case DataTypeVar(index) => s"%${index + s._3}"
      case ScalarType(s) => sym(s.toString())
      case NatType => sym("natT")
      case IndexType(n) => noTyApp(sym("idxT"), List(reggvolve(n, s)))
      case PairType(dt1, dt2) => noTyApp(sym("pairT"), List(dt1, dt2).map(reggvolve(_, s)))
      case ArrayType(n, et) => noTyApp(sym("arrT"), List(reggvolve(n, s), reggvolve(et, s)))
      case VectorType(n, et) => noTyApp(sym("vecT"), List(reggvolve(n, s), reggvolve(et, s)))
    }
  }

  def reggvolve(a: AddressPattern, s: Shift): String = {
    a match {
      case AddressPatternVar(index) => s"?a${index}"
      case AddressPatternAny => "?"
      case AddressPatternNode(n) => n match {
        case AddressVar(index) => s"%${index + s._4}"
        case Global => sym("global")
        case Local => sym("local")
        case Private => sym("private")
        case Constant => sym("constant")
      }
    }
  }

  def reggvolve(n: NatToNatNode[NatPattern], s: Shift): String = {
    ???
  }

}
