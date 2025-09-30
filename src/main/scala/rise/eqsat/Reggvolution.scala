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

    // nats which we need to pivot to avoid matching over certain nat constructs
    val natsToPivot = Vec[(rct.Nat, rct.NatIdentifier, FlatShift, NatPatternVar)]()

    val boundVarToShift = HashMap[String, FlatShift]()

    def makeExprPatVar(
      name: String,
      shift: FlatShift,
      status: PatVarStatus
    ): PatternVar =
      makePatVar(name, shift, patVars, PatternVar, status)

    def makeOtherPatVar[V](
      name: String,
      shift: FlatShift,
      constructor: Int => V,
      status: PatVarStatus,
    ): V =
      makeExprPatVar(name, shift, status) match {
        case PatternVar(index) => constructor(index)
      }

    def shiftOfBound(bound: Expr.Bound): FlatShift =
      // (bound.expr.size, bound.nat.size, bound.data.size, bound.addr.size, bound.n2n.size)
      bound.expr.size + bound.nat.size + bound.data.size + bound.addr.size + bound.n2n.size

    def makePat(expr: rc.Expr,
                bound: Expr.Bound,
                isRhs: Boolean,
                matchType: Boolean = true): Pattern =
      Pattern(expr match {
        case i: rc.Identifier if freeV.contains(i.name) =>
          makeExprPatVar(i.name, shiftOfBound(bound), if (isRhs) { Unknown } else { Known })
        case i: rc.Identifier => PatternNode(Var(bound.indexOf(i)))

        // note: we do not match for the type of lambda bodies, as we can always infer it:
        //       lam(x : xt, e : et) : xt -> et
        case rc.Lambda(x, e) =>
          // right now we assume that all bound variables are uniquely named
          if (!isRhs) {
            assert(!boundVarToShift.contains(x.name))
            boundVarToShift += x.name -> shiftOfBound(bound)
          }
          PatternNode(Lambda(makePat(e, bound + x, isRhs, matchType = false)))
        case rc.DepLambda(rct.NatKind, x: rct.NatIdentifier, e) =>
          PatternNode(NatLambda(makePat(e, bound + x, isRhs, matchType = false)))
        case rc.DepLambda(rct.DataKind, x: rcdt.DataTypeIdentifier, e) =>
          PatternNode(DataLambda(makePat(e, bound + x, isRhs, matchType = false)))
        case rc.DepLambda(rct.AddressSpaceKind, x: rct.AddressSpaceIdentifier, e) =>
          PatternNode(AddrLambda(makePat(e, bound + x, isRhs, matchType = false)))
        case rc.DepLambda(_, _, _) => ???

        case rc.App(rc.App(NamedRewriteDSL.Composition(_), f), g) =>
          PatternNode(Composition(
            makePat(f, bound, isRhs, matchType = true),
            makePat(g, bound, isRhs, matchType = false)))

        // note: we do not match for the type of applied functions, as we can always infer it:
        //       app(f : et -> at, e : et) : at
        case rc.App(f, e) =>
          PatternNode(App(makePat(f, bound, isRhs, matchType = false), makePat(e, bound, isRhs)))
        case rc.DepApp(rct.NatKind, f, x: rct.Nat) =>
          PatternNode(NatApp(
            makePat(f, bound, isRhs, matchType = false), makeNPat(x, bound, isRhs)))
        case rc.DepApp(rct.DataKind, f, x: rct.DataType) =>
          PatternNode(DataApp(
            makePat(f, bound, isRhs, matchType = false), makeDTPat(x, bound, isRhs)))
        case rc.DepApp(rct.AddressSpaceKind, f, x: rct.AddressSpace) =>
          PatternNode(AddrApp(
            makePat(f, bound, isRhs, matchType = false), makeAPat(x, bound, isRhs)))
        case rc.DepApp(_, _, _) => ???

        case rc.Literal(rc.semantics.NatData(n)) =>
          PatternNode(NatLiteral(makeNPat(n, bound, isRhs)))
        case rc.Literal(rc.semantics.IndexData(i, n)) =>
          PatternNode(IndexLiteral(makeNPat(i, bound, isRhs), makeNPat(n, bound, isRhs)))
        case rc.Literal(d) => PatternNode(Literal(d))
        // note: we set the primitive type to a place holder here,
        // because we do not want type information at the node level
        case p: rc.Primitive => PatternNode(Primitive(p.setType(rct.TypePlaceholder)))

        case _ => ???
      }, if (!isRhs && !matchType) TypePatternAny else makeTPat(expr.t, bound, isRhs))

    def makeNPat(n: rct.Nat, bound: Expr.Bound, isRhs: Boolean): NatPattern =
      n match {
        case i: rct.NatIdentifier if freeT(rct.NatKind.IDWrapper(i)) =>
          makeOtherPatVar(i.name, shiftOfBound(bound),
            NatPatternVar, if (isRhs) { Unknown } else { Known })
        case i: rct.NatIdentifier =>
          NatPatternNode(NatVar(bound.indexOf(i)))
        case ae.Cst(c) =>
          NatPatternNode(NatCst(c))
        case ae.Sum(Nil) => NatPatternNode(NatCst(0))
        case ae.Sum(t +: ts) if isRhs => ts.foldRight(makeNPat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatAdd(makeNPat(t, bound, isRhs), acc))
        }
        case ae.Prod(Nil) => NatPatternNode(NatCst(1))
        case ae.Prod(t +: ts) if isRhs => ts.foldRight(makeNPat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatMul(makeNPat(t, bound, isRhs), acc))
        }
        case ae.Pow(b, e) if isRhs =>
          NatPatternNode(NatPow(makeNPat(b, bound, isRhs), makeNPat(e, bound, isRhs)))
        // do not match over these nat constructs on the left-hand side,
        // as structural matching would not be sufficient,
        // try to pivot the equality around a fresh pattern variable instead
        case ae.Sum(_) | ae.Prod(_) | ae.Pow(_, _) if !isRhs =>
          val nv = rct.NatIdentifier(s"_nv${natsToPivot.size}")
          val shift = shiftOfBound(bound)
          val pv = makeOtherPatVar(nv.name, shift, NatPatternVar, Known)
          natsToPivot.addOne((n, nv, shift, pv))
          pv
        case _ =>
          throw new Exception(s"did not expect $n")
      }

    def makeDTPat(dt: rct.DataType, bound: Expr.Bound, isRhs: Boolean): DataTypePattern =
      dt match {
        case i: rcdt.DataTypeIdentifier if freeT(IDWrapper(i)) =>
          makeOtherPatVar(i.name, shiftOfBound(bound),
            DataTypePatternVar, if (isRhs) { Unknown } else { Known })
        case i: rcdt.DataTypeIdentifier =>
          DataTypePatternNode(DataTypeVar(bound.indexOf(i)))
        case s: rcdt.ScalarType =>
          DataTypePatternNode(ScalarType(s))
        case rcdt.NatType =>
          DataTypePatternNode(NatType)
        case rcdt.VectorType(s, et) =>
          DataTypePatternNode(VectorType(makeNPat(s, bound, isRhs), makeDTPat(et, bound, isRhs)))
        case rcdt.IndexType(s) =>
          DataTypePatternNode(IndexType(makeNPat(s, bound, isRhs)))
        case rcdt.PairType(dt1, dt2) =>
          DataTypePatternNode(PairType(makeDTPat(dt1, bound, isRhs), makeDTPat(dt2, bound, isRhs)))
        case rcdt.ArrayType(s, et) =>
          DataTypePatternNode(ArrayType(makeNPat(s, bound, isRhs), makeDTPat(et, bound, isRhs)))
        case _: rcdt.DepArrayType | _: rcdt.DepPairType[_, _] |
             _: rcdt.NatToDataApply | _: rcdt.FragmentType | rcdt.ManagedBufferType(_) | rcdt.OpaqueType(_) =>
          throw new Exception(s"did not expect $dt")
      }

    def makeTPat(t: rct.ExprType, bound: Expr.Bound, isRhs: Boolean): TypePattern =
      t match {
        case dt: rct.DataType => makeDTPat(dt, bound, isRhs)
        case rct.FunType(a, b) =>
          TypePatternNode(FunType(makeTPat(a, bound, isRhs), makeTPat(b, bound, isRhs)))
        case rct.DepFunType(rct.NatKind, x: rct.NatIdentifier, t) =>
          TypePatternNode(NatFunType(makeTPat(t, bound + x, isRhs)))
        case rct.DepFunType(rct.DataKind, x: rcdt.DataTypeIdentifier, t) =>
          TypePatternNode(DataFunType(makeTPat(t, bound + x, isRhs)))
        case rct.DepFunType(rct.AddressSpaceKind, x: rct.AddressSpaceIdentifier, t) =>
          TypePatternNode(AddrFunType(makeTPat(t, bound + x, isRhs)))
        case rct.DepFunType(_, _, _) => ???
        case i: rct.TypeIdentifier =>
          assert(freeT(rct.TypeKind.IDWrapper(i)))
          makeOtherPatVar(i.name, shiftOfBound(bound),
            TypePatternVar, if (isRhs) { Unknown } else { Known })
        case rct.TypePlaceholder =>
          throw new Exception(s"did not expect $t, something was not infered")
      }

    def makeAPat(a: rct.AddressSpace, bound: Expr.Bound, isRhs: Boolean): AddressPattern =
      a match {
        case i: rct.AddressSpaceIdentifier if freeT(rct.AddressSpaceKind.IDWrapper(i)) =>
          makeOtherPatVar(i.name, shiftOfBound(bound),
            AddressPatternVar, if (isRhs) { Unknown } else { Known })
        case i: rct.AddressSpaceIdentifier =>
          AddressPatternNode(AddressVar(bound.indexOf(i)))
        case rct.AddressSpace.Global => AddressPatternNode(Global)
        case rct.AddressSpace.Local => AddressPatternNode(Local)
        case rct.AddressSpace.Private => AddressPatternNode(Private)
        case rct.AddressSpace.Constant => AddressPatternNode(Constant)
      }

    val lhsPat = makePat(typedLhs, Expr.Bound.empty, isRhs = false)
    val rhsPat = makePat(typedRhs, Expr.Bound.empty, isRhs = true)

    def shiftAppliers[S, V](pvm: PatternVarMap[S, V],
                            mkShift: (S, V) => (S, V) => String => String,
                            mkShiftCheck: (S, V) => (S, V) => String => String,
                           ): String => String = {
      pvm.foldRight { a: String => a } { case ((name, shiftMap), acc) =>
        shiftMap.collectFirst { case (s, (v, ShiftCoherent)) => (s, v) }
          // if nothing is shift coherent yet, pick any known shift as our reference
          .orElse(shiftMap.collectFirst { case (s, (v, Known)) => (s, v) }) match {
            case Some(base) =>
              shiftMap(base._1) = (base._2, ShiftCoherent)

              shiftMap.foldRight(acc) { case ((shift, (pv, status)), acc) =>
                status match {
                  // nothing to do
                  case ShiftCoherent => acc
                  // check a shifted variable
                  case Known =>
                    shiftMap(shift) = (pv, ShiftCoherent)
                    a: String => acc(mkShiftCheck.tupled(base)(shift, pv)(a))
                  // construct a shifted variable
                  case Unknown =>
                    shiftMap(shift) = (pv, ShiftCoherent)
                    a: String => acc(mkShift.tupled(base)(shift, pv)(a))
                }
              }
            // nothing is known, but it may become known later (e.g. after nat pivoting)
            case None => acc
          }
        }
      }

    def patMkShift(s1: FlatShift, pv1: PatternVar)
                  (s2: FlatShift, pv2: PatternVar)
                  (applier: String): String = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      s"""{ shifted("${pv1}", "${pv2}", ${shift}, ${cutoff}, ${applier}) }"""
    }

    def patMkShiftCheck(s1: FlatShift, pv1: PatternVar)
                       (s2: FlatShift, pv2: PatternVar)
                       (applier: String): String = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      s"""{ shifted_check("${pv1}", "${pv2}", ${shift}, ${cutoff}, ${applier}) }"""
    }

    // FIXME: duplicated from type inference's 'pivotSolution'
    @scala.annotation.tailrec
    def tryPivot(pivot: rct.NatIdentifier, n: rct.Nat, value: rct.Nat): Option[rct.Nat] = {
      import arithexpr.arithmetic._

      n match {
        case i: rct.NatIdentifier if i == pivot => Some(value)
        case Prod(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            tryPivot(pivot, p.head, rest.foldLeft(value)({
              case (v, r) => v /^ r
            }))
          }
        case Sum(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            tryPivot(pivot, p.head, rest.foldLeft(value)({
              case (v, r) => v - r
            }))
          }
        case Pow(b, Cst(-1)) => tryPivot(pivot, b, Cst(1) /^ value)
        case Mod(p, m) if p == pivot =>
          val k = rct.NatIdentifier(s"_k_${p}_${m}", RangeAdd(0, PosInf, 1))
          Some(k*m + value)
        case _ => None
      }
    }

    def pivotNatsRec(natsToPivot: Seq[(rct.Nat, rct.NatIdentifier, FlatShift, NatPatternVar)],
                     couldNotPivot: Seq[(rct.Nat, rct.NatIdentifier, FlatShift, NatPatternVar)])
                    (applier: String): String = {
      import arithexpr.arithmetic._

      def pivotSuccess = pivotNatsRec(natsToPivot.tail ++ couldNotPivot, Seq())(applier)
      def pivotFailure = pivotNatsRec(natsToPivot.tail, couldNotPivot :+ natsToPivot.head)(applier)

      natsToPivot.headOption match {
        case Some((n, nv, shift, pv)) =>
          def fromNamed(n: rct.Nat): NatPattern = {
            n match {
              case i: rct.NatIdentifier =>
                makeOtherPatVar(i.name, shift, NatPatternVar, Unknown)
              case PosInf => NatPatternNode(NatPosInf)
              case NegInf => NatPatternNode(NatNegInf)
              case Cst(c) => NatPatternNode(NatCst(c))
              case Sum(Nil) => NatPatternNode(NatCst(0))
              case Sum(t +: ts) => ts.foldRight(fromNamed(t)) { case (t, acc) =>
                NatPatternNode(NatAdd(fromNamed(t), acc))
              }
              case Prod(Nil) => NatPatternNode(NatCst(1))
              case Prod(t +: ts) => ts.foldRight(fromNamed(t)) { case (t, acc) =>
                NatPatternNode(NatMul(fromNamed(t), acc))
              }
              case Pow(b, e) =>
                NatPatternNode(NatPow(fromNamed(b), fromNamed(e)))
              case Mod(a, b) =>
                NatPatternNode(NatMod(fromNamed(a), fromNamed(b)))
              case IntDiv(a, b) =>
                NatPatternNode(NatIntDiv(fromNamed(a), fromNamed(b)))
              case _ => throw new Exception(s"no support for $n")
            }
          }

          val natsToFindOut = HashMap[rct.NatIdentifier, Integer]().withDefault(_ => 0)
          ArithExpr.visit(n, {
            case ni: rct.NatIdentifier =>
              val isKnown = patVars.get(ni.name)
                .exists(shiftMap => shiftMap.exists { case (s, (pv, status)) => status != Unknown })
              if (!isKnown) {
                natsToFindOut(ni) += 1
              }
            case _ =>
          })
          natsToFindOut.size match {
            case 0 => // check nv = n
              val valuePat = fromNamed(n)
              val updateShifts = shiftAppliers(patVars, patMkShift, patMkShiftCheck)
              val vp = reggvolve(valuePat)
              updateShifts(s"""{ compute_nat_check("${pv}", "${vp}", ${pivotSuccess}) }""")
              // ComputeNatCheckApplier(pv, valuePat, pivotSuccess))
            case 1 =>
              val (potentialPivot, uses) = natsToFindOut.head
              if (uses == 1) {
                tryPivot(potentialPivot, n, nv) match {
                  case Some(value) =>
                    val valuePat = fromNamed(value)
                    val updateShifts = shiftAppliers(patVars, patMkShift, patMkShiftCheck)
                    val pivotPat = makeOtherPatVar(potentialPivot.name, shift,
                      NatPatternVar, Known)
                    val vp = reggvolve(valuePat)
                    val a = shiftAppliers(patVars, patMkShift, patMkShiftCheck)(pivotSuccess)
                    updateShifts(s"""{ compute_nat("${pivotPat}", "${vp}", ${a}) }""")
                      // ComputeNatApplier(pivotPat, valuePat, a)
                  case None => pivotFailure
                }
              } else {
                pivotFailure
              }
            case _ => pivotFailure
          }
        case None =>
          if (couldNotPivot.nonEmpty) {
            throw new Exception(s"could not pivot nats: $couldNotPivot")
          } else {
            applier
          }
      }
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
          (a: String) => s"""{ not_free_in("${iPV}", ${nfIndex}, ${a}) }"""
            // NotFreeInApplier(iPV, nfIndex, acc(a))
        case VectorizeScalarFun(f, n, fV) =>
          val (enPV, nST) = patVars(n)(0)
          val nPV = enPV match {
            case PatternVar(index) => NatPatternVar(index)
          }
          assert(nST == Known)
          val (fPV, fST) = patVars(f)(0)
          assert(fST == Known)
          val fVPV = makePatVar(fV, 0, patVars, PatternVar, Known)
          (a: String) => s"""{ vectorize_scalar_fun("${fPV}", ${nPV}, "${fVPV}", ${a}) }"""
            // VectorizeScalarFunExtractApplier(fPV, nPV, fVPV, acc(a))
      }
    }
    val shiftPV = shiftAppliers(patVars, patMkShift, patMkShiftCheck)
    val pivotNats = pivotNatsRec(natsToPivot.toSeq, Seq()) _
    val rhsPatApplier = s""""${reggvolve(rhsPat)}""""
    val applier = param(shiftPV(pivotNats(rhsPatApplier)))

    def allIsShiftCoherent[S, V](pvm: PatternVarMap[S, V]): Boolean =
      pvm.forall { case (_, shiftMap) =>
        shiftMap.forall { case (_, (_, status)) => status == ShiftCoherent }}
    assert(allIsShiftCoherent(patVars))

    s"""rewrite!("${name}"; ${searcher} => ${applier})"""
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
