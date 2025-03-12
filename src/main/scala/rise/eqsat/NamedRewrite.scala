package rise.eqsat

import rise.core.types.DataKind.IDWrapper
import rise.{core => rc}
import rise.core.{types => rct}
import rise.core.types.{DataType => rcdt}
import rise.core.{primitives => rcp}

object NamedRewrite {
  sealed trait Parameter
  // TODO: could try to either:
  //  (1) infer these constraints from the rule
  //  (2) check that such a constraint is not missing
  //      for the rule to be well-formed
  case class NotFreeIn(notFree: String, // bound variable in named pattern
                       in: String // free variable in named pattern
                      ) extends Parameter
  case class VectorizeScalarFun(f: String, // free variable in lhs
                                n: String, // free nat variable in lhs
                                fV: String, // free variable in rhs
                               ) extends Parameter

  private def vectorizeScalarFunType(n: rct.Nat, t: rct.ExprType): rct.ExprType = {
    t match {
      case rct.TypePlaceholder => ???
      case rct.TypeIdentifier(_) => ???
      case rct.FunType(inT, outT) => rct.FunType(vectorizeScalarFunType(n, inT), vectorizeScalarFunType(n, outT))
      case rct.DepFunType(_, _, _) => ???
      case dt: rct.DataType => vectorizeScalarFunDataType(n, dt)
    }
  }
  private def vectorizeScalarFunDataType(n: rct.Nat, t: rct.DataType): rct.DataType = {
    t match {
      case _: rcdt.ScalarType | _: rcdt.DataTypeIdentifier => rcdt.VectorType(n, t)
      case rcdt.PairType(a, b) => rcdt.PairType(vectorizeScalarFunDataType(n, a), vectorizeScalarFunDataType(n, b))
      case _ => throw new Exception(s"did not expect $t")
    }
  }

  def init(name: String,
           rule: (NamedRewriteDSL.Pattern, NamedRewriteDSL.Pattern),
           parameters: Seq[NamedRewrite.Parameter] = Seq(),
          ): Rewrite = {
    import rise.core.DSL.infer
    import arithexpr.{arithmetic => ae}

    val (lhs, rhs) = rule
    val untypedFreeV = infer.collectFreeEnv(lhs).map { case (name, t) =>
      assert(t == rct.TypePlaceholder)
      name -> rct.TypeIdentifier("t" + name)
    }
    val typedLhs = infer(lhs, untypedFreeV, Set())
    val freeV1 = infer.collectFreeEnv(typedLhs)
    val freeT = rise.core.IsClosedForm.freeVars(typedLhs)._2.set
    val freeV2 = parameters.flatMap {
      case NotFreeIn(_, _) => None
      case VectorizeScalarFun(f, n, fV) =>
        assert(!freeV1.contains(fV))
        val np = NamedRewriteDSL.stringAsNatPattern(n)
        Some(fV -> vectorizeScalarFunType(np, freeV1(f)))
    }
    val freeV = freeV1 ++ freeV2
    val typedRhs = infer(rc.TypeAnnotation(rhs, typedLhs.t), freeV, freeT)

    trait PatVarStatus
    case object Unknown extends PatVarStatus
    case object Known extends PatVarStatus
    // both known and coherent with other shifts
    case object ShiftCoherent extends PatVarStatus

    // from var name to var index and a status depending on local index shift
    type PatternVarMap[S, V] = HashMap[String, HashMap[S, (V, PatVarStatus)]]
    val patVars: PatternVarMap[Expr.Shift, PatternVar] = HashMap()
    val natPatVars: PatternVarMap[Nat.Shift, NatPatternVar] = HashMap()
    val dataTypePatVars: PatternVarMap[Type.Shift, DataTypePatternVar] = HashMap()
    val typePatVars: PatternVarMap[Type.Shift, TypePatternVar] = HashMap()
    val addrPatVars: PatternVarMap[Address.Shift, AddressPatternVar] = HashMap()

    // nats which we need to pivot to avoid matching over certain nat constructs
    val natsToPivot = Vec[(rct.Nat, rct.NatIdentifier, Nat.Shift, NatPatternVar)]()

    val boundVarToShift = HashMap[String, Expr.Shift]()

    def makePatVar[S, V](name: String,
                         shift: S,
                         pvm: PatternVarMap[S, V],
                         constructor: Int => V,
                         status: PatVarStatus): V = {
      val shiftMap = pvm.getOrElseUpdate(name, HashMap())
      val (pv, previousStatus) = shiftMap.getOrElseUpdate(shift, {
        val pvCount = pvm.values.map(m => m.size).sum
        (constructor(pvCount), Unknown)
      })
      val updatedStatus = (previousStatus, status) match {
        case (Unknown, s) => s
        case (s, Unknown) => s
        case (Known, Known) => Known
        case t => throw new Exception(s"did not expect $t")
      }
      shiftMap(shift) = (pv, updatedStatus)
      pv
    }

    def makePat(expr: rc.Expr,
                bound: Expr.Bound,
                isRhs: Boolean,
                matchType: Boolean = true): Pattern =
      Pattern(expr match {
        case i: rc.Identifier if freeV.contains(i.name) =>
          makePatVar(i.name,
            (bound.expr.size, bound.nat.size, bound.data.size, bound.addr.size),
            patVars, PatternVar, if (isRhs) { Unknown } else { Known })
        case i: rc.Identifier => PatternNode(Var(bound.indexOf(i)))

        // note: we do not match for the type of lambda bodies, as we can always infer it:
        //       lam(x : xt, e : et) : xt -> et
        case rc.Lambda(x, e) =>
          // right now we assume that all bound variables are uniquely named
          if (!isRhs) {
            assert(!boundVarToShift.contains(x.name))
            boundVarToShift += x.name ->
              (bound.expr.size + 1, bound.nat.size, bound.data.size, bound.addr.size)
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

        case rc.Literal(d) => PatternNode(Literal(d))
        // note: we set the primitive type to a place holder here,
        // because we do not want type information at the node level
        case p: rc.Primitive => PatternNode(Primitive(p.setType(rct.TypePlaceholder)))

        case _ => ???
      }, if (!isRhs && !matchType) TypePatternAny else makeTPat(expr.t, bound, isRhs))

    def makeNPat(n: rct.Nat, bound: Expr.Bound, isRhs: Boolean): NatPattern =
      n match {
        case i: rct.NatIdentifier if freeT(rct.NatKind.IDWrapper(i)) =>
          makePatVar(i.name, bound.nat.size, natPatVars,
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
          val pv = makePatVar(nv.name, bound.nat.size, natPatVars, NatPatternVar, Known)
          natsToPivot.addOne((n, nv, bound.nat.size, pv))
          pv
        case _ =>
          throw new Exception(s"did not expect $n")
      }

    def makeDTPat(dt: rct.DataType, bound: Expr.Bound, isRhs: Boolean): DataTypePattern =
      dt match {
        case i: rcdt.DataTypeIdentifier if freeT(IDWrapper(i)) =>
          makePatVar(i.name, (bound.nat.size, bound.data.size),
            dataTypePatVars, DataTypePatternVar, if (isRhs) { Unknown } else { Known })
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
          makePatVar(i.name, (bound.nat.size, bound.data.size),
            typePatVars, TypePatternVar, if (isRhs) { Unknown } else { Known })
        case rct.TypePlaceholder =>
          throw new Exception(s"did not expect $t, something was not infered")
      }

    def makeAPat(a: rct.AddressSpace, bound: Expr.Bound, isRhs: Boolean): AddressPattern =
      a match {
        case i: rct.AddressSpaceIdentifier if freeT(rct.AddressSpaceKind.IDWrapper(i)) =>
          makePatVar(i.name, bound.addr.size, addrPatVars,
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
                            mkShift: (S, V) => (S, V) => Applier => Applier,
                            mkShiftCheck: (S, V) => (S, V) => Applier => Applier,
                           ): Applier => Applier = {
      pvm.foldRight { a: Applier => a } { case ((name, shiftMap), acc) =>
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
                    a: Applier => acc(mkShiftCheck.tupled(base)(shift, pv)(a))
                  // construct a shifted variable
                  case Unknown =>
                    shiftMap(shift) = (pv, ShiftCoherent)
                    a: Applier => acc(mkShift.tupled(base)(shift, pv)(a))
                }
              }
            // nothing is known, but it may become known later (e.g. after nat pivoting)
            case None => acc
          }
        }
      }

    def patMkShift(s1: Expr.Shift, pv1: PatternVar)
                  (s2: Expr.Shift, pv2: PatternVar)
                  (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2, s2._3 - s1._3, s2._4 - s1._4)
      // TODO: or ShiftedApplier?
      ShiftedExtractApplier(pv1, pv2, shift, cutoff, applier)
    }

    def patMkShiftCheck(s1: Expr.Shift, pv1: PatternVar)
                       (s2: Expr.Shift, pv2: PatternVar)
                       (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2, s2._3 - s1._3, s2._4 - s1._4)
      ShiftedCheckApplier(pv1, pv2, shift, cutoff, applier)
    }

    def natPatMkShift(s1: Nat.Shift, pv1: NatPatternVar)
                     (s2: Nat.Shift, pv2: NatPatternVar)
                     (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      ShiftedNatApplier(pv1, pv2, shift, cutoff, applier)
    }

    def natPatMkShiftCheck(s1: Nat.Shift, pv1: NatPatternVar)
                          (s2: Nat.Shift, pv2: NatPatternVar)
                          (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = s2 - s1
      ShiftedNatCheckApplier(pv1, pv2, shift, cutoff, applier)
    }

    def dataTypePatMkShift(s1: Type.Shift, pv1: DataTypePatternVar)
                          (s2: Type.Shift, pv2: DataTypePatternVar)
                          (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2)
      ShiftedDataTypeApplier(pv1, pv2, shift, cutoff, applier)
    }

    def dataTypePatMkShiftCheck(s1: Type.Shift, pv1: DataTypePatternVar)
                               (s2: Type.Shift, pv2: DataTypePatternVar)
                               (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2)
      ShiftedDataTypeCheckApplier(pv1, pv2, shift, cutoff, applier)
    }

    def typePatMkShift(s1: Type.Shift, pv1: TypePatternVar)
                      (s2: Type.Shift, pv2: TypePatternVar)
                      (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2)
      ShiftedTypeApplier(pv1, pv2, shift, cutoff, applier)
    }

    def typePatMkShiftCheck(s1: Type.Shift, pv1: TypePatternVar)
                           (s2: Type.Shift, pv2: TypePatternVar)
                           (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2._1 - s1._1, s2._2 - s1._2)
      ShiftedTypeCheckApplier(pv1, pv2, shift, cutoff, applier)
    }

    def addrPatMkShift(s1: Address.Shift, pv1: AddressPatternVar)
                      (s2: Address.Shift, pv2: AddressPatternVar)
                      (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2 - s1)
      // ShiftedAddressApplier(pv1, pv2, shift, cutoff, applier)
      ???
    }

    def addrPatMkShiftCheck(s1: Address.Shift, pv1: AddressPatternVar)
                           (s2: Address.Shift, pv2: AddressPatternVar)
                           (applier: Applier): Applier = {
      assert(s1 != s2)
      val cutoff = s1
      val shift = (s2 - s1)
      // ShiftedAddressCheckApplier(pv1, pv2, shift, cutoff, applier)
      ???
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

    def pivotNatsRec(natsToPivot: Seq[(rct.Nat, rct.NatIdentifier, Nat.Shift, NatPatternVar)],
                     couldNotPivot: Seq[(rct.Nat, rct.NatIdentifier, Nat.Shift, NatPatternVar)])
                    (applier: Applier): Applier = {
      import arithexpr.arithmetic._

      def pivotSuccess = pivotNatsRec(natsToPivot.tail ++ couldNotPivot, Seq())(applier)
      def pivotFailure = pivotNatsRec(natsToPivot.tail, couldNotPivot :+ natsToPivot.head)(applier)

      natsToPivot.headOption match {
        case Some((n, nv, shift, pv)) =>
          def fromNamed(n: rct.Nat): NatPattern = {
            n match {
              case i: rct.NatIdentifier =>
                makePatVar(i.name, shift, natPatVars, NatPatternVar, Unknown)
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
              val isKnown = natPatVars.get(ni.name)
                .exists(shiftMap => shiftMap.exists { case (s, (pv, status)) => status != Unknown })
              if (!isKnown) {
                natsToFindOut(ni) += 1
              }
            case _ =>
          })
          natsToFindOut.size match {
            case 0 => // check nv = n
              val valuePat = fromNamed(n)
              val updateShifts = shiftAppliers(natPatVars, natPatMkShift, natPatMkShiftCheck)
              updateShifts(ComputeNatCheckApplier(pv, valuePat, pivotSuccess))
            case 1 =>
              val (potentialPivot, uses) = natsToFindOut.head
              if (uses == 1) {
                tryPivot(potentialPivot, n, nv) match {
                  case Some(value) =>
                    val valuePat = fromNamed(value)
                    val updateShifts = shiftAppliers(natPatVars, natPatMkShift, natPatMkShiftCheck)
                    val pivotPat = makePatVar(potentialPivot.name, shift, natPatVars,
                      NatPatternVar, Known)
                    updateShifts(ComputeNatApplier(pivotPat, valuePat,
                      shiftAppliers(natPatVars, natPatMkShift, natPatMkShiftCheck)(pivotSuccess)))
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

    val searcher: Searcher = lhsPat.compile()
    val param = parameters.foldRight((a: Applier) => a) { case (c, acc) =>
      c match {
        case NotFreeIn(notFree, in) =>
          val nfShift = boundVarToShift.getOrElse(notFree, (0, 0, 0, 0))._1
          // all left-hand-side uses of `in` may contain `notFree`
          assert(patVars(in).forall {
            case ((shift, _, _, _), (_, status)) =>
              shift >= nfShift || status != Known
          })
          // pick one of these uses
          val (iS, iPV) = patVars(in).collectFirst {
            case ((s, _, _, _), (pv, Known)) => (s, pv)
          }.get
          val nfIndex = iS - nfShift // >= 0 because iS >= nfShift
          (a: Applier) => (new ConditionalApplier(Set(iPV), (Set(FreeAnalysis), Set()), acc(a)) {
            def cond(egraph: EGraph, eclass: EClassId, shc: Substs)(subst: shc.Subst): Boolean = {
              val freeOf = egraph.getAnalysis(FreeAnalysis)
              !freeOf(shc.get(iPV, subst)).free.contains(nfIndex)
            }
          })
        case VectorizeScalarFun(f, n, fV) =>
          val (nPV, nST) = natPatVars(n)(0)
          assert(nST == Known)
          val (fPV, fST) = patVars(f)((0, 0, 0, 0))
          assert(fST == Known)
          val fVPV = makePatVar(fV, (0, 0, 0, 0), patVars, PatternVar, Known)
          (a: Applier) => VectorizeScalarFunExtractApplier(fPV, nPV, fVPV, acc(a))
      }
    }
    val shiftPV = shiftAppliers(patVars, patMkShift, patMkShiftCheck)
    val shiftNPV = shiftAppliers(natPatVars, natPatMkShift, natPatMkShiftCheck)
    val shiftDTPV = shiftAppliers(dataTypePatVars, dataTypePatMkShift, dataTypePatMkShiftCheck)
    val shiftTPV = shiftAppliers(typePatVars, typePatMkShift, typePatMkShiftCheck)
    val shiftAPV = shiftAppliers(addrPatVars, addrPatMkShift, addrPatMkShiftCheck)
    val pivotNats = pivotNatsRec(natsToPivot.toSeq, Seq()) _
    val applier = param(shiftPV(shiftNPV(shiftDTPV(shiftTPV(shiftAPV(pivotNats(rhsPat)))))))

    def allIsShiftCoherent[S, V](pvm: PatternVarMap[S, V]): Boolean =
      pvm.forall { case (_, shiftMap) =>
        shiftMap.forall { case (_, (_, status)) => status == ShiftCoherent }}
    assert(allIsShiftCoherent(patVars))
    assert(allIsShiftCoherent(natPatVars))
    assert(allIsShiftCoherent(dataTypePatVars))
    assert(allIsShiftCoherent(typePatVars))
    assert(allIsShiftCoherent(addrPatVars))

    Rewrite.init(name, searcher -> applier)
  }
}

object NamedRewriteDSL {
  type Pattern = rc.Expr
  type NatPattern = rct.Nat
  type DataTypePattern = rct.DataType
  type TypePattern = rct.ExprType
  type AddressSpacePattern = rct.AddressSpace

  import scala.language.implicitConversions

  implicit final class RewriteArrow(private val lhs: Pattern) extends AnyVal {
    @inline def -->(rhs: Pattern): (Pattern, Pattern) = lhs -> rhs
  }

  implicit def stringAsIdentifier(name: String): Pattern =
    rc.Identifier(name)(rct.TypePlaceholder)
  def app(a: Pattern, b: Pattern): Pattern =
    rc.App(a, b)(rct.TypePlaceholder)
  def lam(name: String, e: Pattern): Pattern =
    rc.Lambda(rc.Identifier(name)(rct.TypePlaceholder), e)(rct.TypePlaceholder)
  def nApp(f: Pattern, x: NatPattern): Pattern =
    rc.DepApp(rct.NatKind, f, x)(rct.TypePlaceholder)
  def nLam(name: String, e: Pattern): Pattern = {
    val n = rct.NatIdentifier(name)
    rc.DepLambda(rct.NatKind, n, e)(rct.TypePlaceholder)
  }
  def aApp(f: Pattern, x: AddressSpacePattern): Pattern =
    rc.DepApp(rct.AddressSpaceKind, f, x)(rct.TypePlaceholder)
  def aLam(name: String, e: Pattern): Pattern = {
    val n = rct.AddressSpaceIdentifier(name)
    rc.DepLambda(rct.AddressSpaceKind, n, e)(rct.TypePlaceholder)
  }
  def l(d: rc.semantics.Data): Pattern = rc.Literal(d)
  def lf32(f: Float): Pattern = l(rise.core.semantics.FloatData(f))
  def lidx(i: Int, n: Int) = l(rise.core.semantics.IndexData(i, n))

  def slide: Pattern = rcp.slide.primitive
  def map: Pattern = rcp.map.primitive
  def reduce: Pattern = rcp.reduce.primitive
  def transpose: Pattern = rcp.transpose.primitive
  def zip: Pattern = rcp.zip.primitive
  def unzip: Pattern = rcp.unzip.primitive
  def split: Pattern = rcp.split.primitive
  def join: Pattern = rcp.join.primitive
  def fst: Pattern = rcp.fst.primitive
  def snd: Pattern = rcp.snd.primitive
  def mapFst: Pattern = rcp.mapFst.primitive
  def mapSnd: Pattern = rcp.mapSnd.primitive
  def makePair: Pattern = rcp.makePair.primitive
  def add: Pattern = rcp.add.primitive
  def mul: Pattern = rcp.mul.primitive
  def div: Pattern = rcp.div.primitive
  def drop: Pattern = rcp.drop.primitive
  def take: Pattern = rcp.take.primitive
  def asScalar: Pattern = rcp.asScalar.primitive
  def asVector: Pattern = rcp.asVector.primitive
  def asVectorAligned: Pattern = rcp.asVectorAligned.primitive
  def vectorFromScalar: Pattern = rcp.vectorFromScalar.primitive

  def `?n`: NatPattern =
    rct.NatIdentifier(rc.freshName("n"))
  implicit def stringAsNatPattern(name: String): NatPattern =
    rct.NatIdentifier(name)

  def `?dt`: DataTypePattern =
    rcdt.DataTypeIdentifier(rc.freshName("dt"))
  implicit def stringAsDataTypePattern(name: String): DataTypePattern =
    rcdt.DataTypeIdentifier(name)

  def `?a`: AddressSpacePattern =
    rct.AddressSpaceIdentifier(rc.freshName("a"))
  implicit def stringAsAddressSpacePattern(name: String): AddressSpacePattern =
    rct.AddressSpaceIdentifier(name)

  def `?t`: TypePattern = rct.TypePlaceholder
  def t(name: String): TypePattern =
    rct.TypeIdentifier(name)

  val int: DataTypePattern = rcdt.int
  val f32: DataTypePattern = rcdt.f32

  implicit final class TypeAnnotation(private val t: TypePattern) extends AnyVal {
    @inline def ::(p: Pattern): Pattern = rc.TypeAnnotation(p, t)
  }
  implicit final class FunConstructorT(private val r: TypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern =
      rct.FunType[rct.ExprType, rct.ExprType](t, r)
  }
  implicit final class FunConstructorDT(private val r: DataTypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern =
      rct.FunType[rct.ExprType, rct.ExprType](t, r)
  }
  implicit final class ArrayConstructor(private val s: NatPattern) extends AnyVal {
    @inline def `.`(et: DataTypePattern): DataTypePattern =
      rcdt.ArrayType(s, et)
  }
  implicit final class PairConstructor(private val a: DataTypePattern) extends AnyVal {
    @inline def x(b: DataTypePattern): DataTypePattern =
      rcdt.PairType(a, b)
  }

  implicit final class NotFreeIn(private val in: String) extends AnyVal {
    @inline def notFree(notFree: String): NamedRewrite.Parameter =
      NamedRewrite.NotFreeIn(notFree, in)
  }
  def vectorizeScalarFun(f: String, n: String, fV: String): NamedRewrite.Parameter =
    NamedRewrite.VectorizeScalarFun(f, n, fV)

  // combinatory

  implicit final class MakeComposition(private val f: Pattern) extends AnyVal {
    @inline def >>(g: Pattern): Pattern = app(app(Composition(), f), g)
  }
  implicit final class MakeCompositionString(private val f: String) extends AnyVal {
    @inline def >>(g: Pattern): Pattern = app(app(Composition(), f), g)
  }
  case class Composition(override val t: rct.ExprType = rct.TypePlaceholder) extends rc.Primitive {
    import rise.core.DSL.Type.{impl, ->:}

    override def name: String = ">>"

    override def typeScheme: rct.ExprType =
      impl { a: rct.TypeIdentifier =>
      impl { b: rct.TypeIdentifier =>
      impl { c: rct.TypeIdentifier =>
        (a ->: b) ->: (b ->: c) ->: (a ->: c)
      }}}
    override def primEq(obj: rc.Primitive): Boolean =
      obj match {
        case _: Composition => true
        case _ => false
      }

    override def setType(t: rct.ExprType): rc.Primitive =
      Composition(t)
  }
}
