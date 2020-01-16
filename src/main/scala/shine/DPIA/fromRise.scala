package shine.DPIA

import scala.language.existentials

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.{OperationalSemantics => OpSem}
import shine.DPIA.Types._
import rise.core.{semantics => rs, types => rt}
import rise.{core => r}

import scala.annotation.tailrec

object fromRise {
  def apply(expr: r.Expr): Phrase[_ <: PhraseType] = {
    if (!r.IsClosedForm(expr)) {
      throw new Exception(s"expression is not in closed form: $expr")
    }
    expression(expr).left.get
  }

  private def arity(ft: rt.Type): Int = {
    @tailrec
    def tailrecArity(ft: rt.Type, accum: Int): Int = {
      ft match {
        case rt.DepFunType(x, t) => tailrecArity(t, accum + 1)
        case rt.FunType(_, outT) => tailrecArity(outT, accum + 1)
        case _ => accum
      }
    }

    tailrecArity(ft, accum = 0)
  }

  private final case class AppliedPrimitiveArgs(p: r.Primitive,
                                                args: List[Phrase[_ <: PhraseType]],
                                                nonPhraseArgs: List[Kind#T])

  private def expression(expr: r.Expr): Either[Phrase[_ <: PhraseType], AppliedPrimitiveArgs] = expr match {
    case r.Identifier(name) =>
      Left(Identifier(name, `type`(expr.t)))

    case r.Lambda(x, e) => expr.t match {
      case rt.FunType(i, _) =>
        Left(Lambda(Identifier(x.name, `type`(i)), expression(e).left.get))
      case _ => error(expr.t.toString, "a function type")
    }

    case r.App(f, e) => {
      val ee = expression(e) match {
        case Left(le) => le
        case Right(appargs) =>
            primitive(appargs.p, appargs.args.reverse, appargs.nonPhraseArgs.reverse)
      }

      f match {
        case fp: r.Primitive =>
          if (arity(fp.t) == 1)
            Left(primitive(fp, ee :: Nil, Nil))
          else
            Right(AppliedPrimitiveArgs(fp, ee :: Nil, Nil))
        case _ => {
          expression(f) match {
            case Right(AppliedPrimitiveArgs(p, args, npArgs)) =>
              if (arity(p.t) == args.length + npArgs.length + 1)
                Left(primitive(p, (ee :: args).reverse, npArgs.reverse))
              else
                Right(AppliedPrimitiveArgs(p, ee :: args, npArgs))
            case Left(ff) =>
              val liftedF = ff match {
                //We allow DepLambda[AccessKind] even for f as part of App, in order to infer AccessType
                case dl: DepLambda[_, t2] =>
                  //TODO check if it is possible that ee is not of ExpType in this case
                  val access = ee.asInstanceOf[Phrase[ExpType]].t.accessType
                  Lifting.liftDependentFunction(dl.asInstanceOf[DepLambda[AccessKind, t2]])(access)
                case l: Lambda[_, _] => l
                case _ => throw new Exception("This should never happen.")
              }

              val reducedF = liftedF match {
                case ndl: Lambda[_, t2] =>
                  ndl.param.`type` match {
                    case _: ExpType =>
                      Lifting.liftFunction(ndl.asInstanceOf[Phrase[FunType[ExpType, t2]]])
                        .reducing(ee.asInstanceOf[Phrase[ExpType]])
                    case _: FunType[_, _] =>
                      Lifting.liftFunction(ndl.asInstanceOf[Phrase[FunType[FunType[_, _], t2]]])
                        .reducing(ee.asInstanceOf[Phrase[FunType[_,_]]])
                    case _ => throw new Exception("This should never happen.")
                  }
                case _ => throw new Exception("This should never happen.")
              }
              Left(reducedF)
          }
        }
      }
    }

    case r.DepLambda(x, e) =>
      val appE = expression(e) match {
        case Left(ee) => ee
        case Right(appargs) => primitive(appargs.p, appargs.args, appargs.nonPhraseArgs)
      }

      x match {
        case n: rt.NatIdentifier =>
          Left(DepLambda[NatKind](natIdentifier(n))(appE))
        case dt: rt.DataTypeIdentifier =>
          Left(DepLambda[DataKind](dataTypeIdentifier(dt))(appE))
        case a: rt.AddressSpaceIdentifier =>
          Left(DepLambda[AddressSpaceKind](addressSpaceIdentifier(a))(appE))
      }

    case r.DepApp(f, x) =>
      f match {
        case fp: r.Primitive =>
          if (arity(fp.t) == 1)
            Left(primitive(fp, Nil, kind(x) :: Nil))
          else
            Right(AppliedPrimitiveArgs(fp, Nil, kind(x) :: Nil))
        case _ =>
          expression(f) match {
            case Right(AppliedPrimitiveArgs(p, args, npArgs)) =>
              if(arity(p.t) == args.length + npArgs.length + 1)
                Left(primitive(p, args.reverse, (kind(x) :: npArgs).reverse))
              else
                Right(AppliedPrimitiveArgs(p, args, kind(x) :: npArgs))
            case Left(df) =>
              x match {
                case n: Nat =>
                  Left(Lifting.liftDependentFunction(
                    df.asInstanceOf[Phrase[DepFunType[NatKind, PhraseType]]])(n))
                case dt: rt.DataType =>
                  Left(Lifting.liftDependentFunction(
                    df.asInstanceOf[Phrase[DepFunType[DataKind, PhraseType]]])(dataType(dt)))
                case a: rt.AddressSpace =>
                  Left(Lifting.liftDependentFunction(
                    df.asInstanceOf[Phrase[DepFunType[AddressSpaceKind, PhraseType]]])(addressSpace(a)))
              }
          }

      }

    case r.Literal(d) => d match {
      case rs.NatData(n) => Left(Natural(n))
      case rs.IndexData(i, n) => Left(FunctionalPrimitives.AsIndex(n, Natural(i)))
      case _ => Left(Literal(data(d)))
    }

    case p: r.Primitive => Left(primitive(p, Nil, Nil))
  }

  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
    case rt.AddressSpace.Global => AddressSpace.Global
    case rt.AddressSpace.Local => AddressSpace.Local
    case rt.AddressSpace.Private => AddressSpace.Private
    case rt.AddressSpace.Constant => AddressSpace.Constant
    case rt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }

  def scalarType(t: rt.ScalarType): ScalarType = t match {
    case rt.bool => bool
    case rt.int => int
    case rt.float => float
    case rt.double => double
    case rt.NatType => NatType
  }

  def basicType(t: rt.BasicType): BasicType = t match {
    case st: rt.ScalarType => scalarType(st)
    case rt.IndexType(sz) => IndexType(sz)
    case rt.VectorType(sz, et) => et match {
      case e : rt.ScalarType => VectorType(sz, scalarType(e))
      case _ => ???
    }
  }

  def dataType(t: rt.DataType): DataType = t match {
    case bt: rt.BasicType => basicType(bt)
    case i: rt.DataTypeIdentifier => dataTypeIdentifier(i)
    case rt.ArrayType(sz, et) => ArrayType(sz, dataType(et))
    case rt.DepArrayType(sz, f) => DepArrayType(sz, ntd(f))
    case rt.PairType(a, b) => PairType(dataType(a), dataType(b))
    case rt.NatToDataApply(f, n) => NatToDataApply(ntd(f), n)
  }

  def ntd(ntd: rt.NatToData): NatToData= ntd match {
    case rt.NatToDataLambda(n, body) => NatToDataLambda(natIdentifier(n), dataType(body))
    case rt.NatToDataIdentifier(x, _) => NatToDataIdentifier(x)
  }

  def ntn(ntn: rt.NatToNat): NatToNat= ntn match {
    case rt.NatToNatLambda(n, body) => NatToNatLambda(natIdentifier(n), body)
    case rt.NatToNatIdentifier(x, _) => NatToNatIdentifier(x)
  }

  def dataTypeIdentifier(dt: rt.DataTypeIdentifier): DataTypeIdentifier = DataTypeIdentifier(dt.name)
  def natIdentifier(n: rt.NatIdentifier): NatIdentifier = NatIdentifier(n.name, n.range)
  def addressSpaceIdentifier(a: rt.AddressSpaceIdentifier): AddressSpaceIdentifier = AddressSpaceIdentifier(a.name)
  def natToNatIdentifier(n: rt.NatToNatIdentifier): NatToNatIdentifier = NatToNatIdentifier(n.name)
  def natToDataIdentifier(n: rt.NatToDataIdentifier): NatToDataIdentifier = NatToDataIdentifier(n.name)
  def accessTypeIdentifier(): AccessTypeIdentifier = AccessTypeIdentifier(freshName("access"))

  def kind(k: rt.Kind#T): Kind#T = k match {
    case addr: rt.AddressSpaceKind#T => addressSpace(addr).asInstanceOf[Kind#T]
    case dt: rt.DataKind#T => dataType(dt).asInstanceOf[Kind#T]
    case nat: rt.NatKind#T => nat.asInstanceOf[Kind#T]
    //TODO what about NatToData etc.?
    case _ => throw new Exception("This should never happen")
  }

  def `type`(ty: rt.Type): PhraseType = ty match {
    case dt: rt.DataType => ExpType(dataType(dt), read)
    case rt.FunType(i, o)     => `type`(i) ->: `type`(o)
    case rt.DepFunType(i, t)  => i match {
        case dt: rt.DataTypeIdentifier    => dataTypeIdentifier(dt)   `()->:` `type`(t)
        case n: rt.NatIdentifier           => natIdentifier(n)         `()->:` `type`(t)
        case n2n: rt.NatToNatIdentifier   => natToNatIdentifier(n2n)  `()->:` `type`(t)
        case n2d: rt.NatToDataIdentifier  => natToDataIdentifier(n2d) `()->:` `type`(t)
      }
    case rt.TypeIdentifier(_) | rt.TypePlaceholder => throw new Exception("This should not happen")
  }

  def data(d: rs.Data): OpSem.Data = d match {
    case rs.ArrayData(a) => OpSem.ArrayData(a.map(data).toVector)
    case rs.PairData(a, b) => OpSem.PairData(data(a), data(b))
    case rs.BoolData(b) => OpSem.BoolData(b)
    case rs.IntData(i) => OpSem.IntData(i)
    case rs.FloatData(f) => OpSem.FloatData(f)
    case rs.DoubleData(d) => OpSem.DoubleData(d)
    case rs.VectorData(v) => OpSem.VectorData(v.map(data(_)).toVector)
    case rs.IndexData(i, n) => OpSem.IndexData(i, n)
    case rs.NatData(n) => OpSem.NatData(n)
  }

  import rise.core.{primitives => core}
  import shine.DPIA.FunctionalPrimitives._

  def fun[T <: PhraseType](t: T,
                           f: Phrase[T] => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  @tailrec
  private def applyAllExistingArgs(f: Phrase[_ <: PhraseType],
                           args: List[Phrase[_ <: PhraseType]],
                           nonPhraseArgs: List[Kind#T]): Phrase[_ <: PhraseType] = f match {
      case depLambda: DepLambda[_, t2] =>
        if (nonPhraseArgs.isEmpty) return f

        depLambda.x match {
          case _: AddressSpace =>
            applyAllExistingArgs (Lifting.liftDependentFunction(depLambda.asInstanceOf[DepLambda[AddressSpaceKind, t2]])
                                                       (nonPhraseArgs.head.asInstanceOf[AddressSpace]),
              args, nonPhraseArgs.tail)
          case _: DataType =>
            applyAllExistingArgs (Lifting.liftDependentFunction(depLambda.asInstanceOf[DepLambda[DataKind, t2]])
                                                       (nonPhraseArgs.head.asInstanceOf[DataType]),
              args, nonPhraseArgs.tail)
          case _: AccessType =>
            applyAllExistingArgs (Lifting.liftDependentFunction(depLambda.asInstanceOf[DepLambda[AccessKind, t2]])
                                                       (nonPhraseArgs.head.asInstanceOf[AccessType]),
              args, nonPhraseArgs.tail)
          case _: Nat =>
            applyAllExistingArgs (Lifting.liftDependentFunction(depLambda.asInstanceOf[DepLambda[NatKind, t2]])
                                                       (nonPhraseArgs.head.asInstanceOf[Nat]),
              args, nonPhraseArgs.tail)
          case _ => throw new Exception("This should never happen.")
        }
      case lambda: Lambda[_, t2] =>
        if (args.isEmpty) return f

        lambda.param.`type` match {
          case _: ExpType =>
            applyAllExistingArgs(Lifting.liftFunction(lambda.asInstanceOf[Lambda[ExpType, t2]])
              .reducing(args.head.asInstanceOf[Phrase[ExpType]]), args.tail, nonPhraseArgs)
          case _: FunType[_, _] =>
            applyAllExistingArgs(Lifting.liftFunction(lambda.asInstanceOf[Lambda[FunType[_, _], t2]])
              .reducing(args.head.asInstanceOf[Phrase[FunType[_, _]]]), args.tail, nonPhraseArgs)
          case _: DepFunType[_, _] =>
            applyAllExistingArgs(Lifting.liftFunction(lambda.asInstanceOf[Lambda[DepFunType[_, _], t2]])
              .reducing(args.head.asInstanceOf[Phrase[DepFunType[_, _]]]), args.tail, nonPhraseArgs)
          case _ => throw new Exception("This should never happen.")
        }
      case _ => f
    }

  def primitive(p: r.Primitive,
                args: List[Phrase[_ <: PhraseType]],
                nonPhraseArgs: List[Kind#T]): Phrase[_ <: PhraseType] = {
    import rise.OpenCL.{primitives => ocl}
    import rise.OpenMP.{primitives => omp}
    import shine.OpenCL.FunctionalPrimitives._
    import shine.OpenMP.FunctionalPrimitives._

    (p, p.t) match {
      case (core.PrintType(msg),
            rt.FunType(rt: rt.DataType, _))
      =>
        val t = dataType(rt)
        val unappPrim = fun[ExpType](exp"[$t, $read]", e => PrintType(msg, t, e))
        applyAllExistingArgs(unappPrim, args, Nil)

      case (core.NatAsIndex(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.NatType, rt.IndexType(_))))
      =>
        val t = dataType(rt.NatType)
        val unappPrim = DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](exp"[$t, $read]", arg => AsIndex(n, arg)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Map(),
      rt.FunType(rt.FunType(rin: rt.DataType, rout: rt.DataType),
      rt.FunType(rt.ArrayType(n, _), _)))
      =>
        val in = dataType(rin)
        val out = dataType(rout)

        val ai = accessTypeIdentifier()
        val unappPrim = DepLambda[AccessKind](ai)(
          fun[FunType[ExpType, ExpType]](exp"[$in, $ai]" ->: exp"[$out, $ai]",
            f => fun[ExpType](exp"[$n.$in, $ai]", arr => Map(n, in, out, ai, f, arr))))

        if (args.length >= 2) {
          val access = args(1).asInstanceOf[Phrase[ExpType]].t.accessType
          val fwAcc =
            //If the function is parametric over the access type
            if (args.head.t.isInstanceOf[DepFunType[_, _]])
              Lifting.liftDependentFunction(
                args.head.asInstanceOf[Phrase[DepFunType[AccessKind, FunType[_, _]]]])(access)
            //If the access types for f are already determined
            else args.head.asInstanceOf[Phrase[FunType[_, _]]]

          val withAccess = Lifting.liftDependentFunction(unappPrim)(access)
          applyAllExistingArgs(withAccess, fwAcc :: args.tail, nonPhraseArgs)
        } else {
          val fwAcc = Lifting.liftDependentFunction(
            args.head.asInstanceOf[Phrase[DepFunType[AccessKind, FunType[_, _]]]])(ai)

          applyAllExistingArgs(unappPrim, fwAcc :: args.tail, nonPhraseArgs)
        }

      case (core.MapSeq(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapSeq, n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.MapSeqUnroll(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapSeqUnroll, n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (omp.MapPar(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapPar, n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.MapGlobal(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapGlobal(dim), n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.MapLocal(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapLocal(dim), n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.MapWorkGroup(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        val unappPrim = makeLoopMap(MapWorkGroup(dim), n, la, lb, args)
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.DepMapSeq(),
      rt.FunType(
      rt.DepFunType(lk: rt.NatIdentifier, rt.FunType(_, _)),
      rt.FunType(rt.DepArrayType(n, la), rt.DepArrayType(_, lb))))
      =>
        val a = ntd(la)
        val b = ntd(lb)
        val k = natIdentifier(lk)
        val unappPrim = fun[`(nat)->:`[ExpType ->: ExpType]](
          k `()->:` (ExpType(a(k), read) ->: ExpType(b(k), read))
          , f =>
          fun[ExpType](ExpType(DepArrayType(n, a), read), e =>
            DepMapSeq(n, a, b, f, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.ReduceSeq(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType ->: ExpType ->: ExpType](exp"[$b, $read]" ->: exp"[$a, $read]" ->: exp"[$b, $write]", f =>
            fun[ExpType](exp"[$b, $write]", i =>
              fun[ExpType](exp"[$n.$a, $read]", e =>
                ReduceSeq(n, a, b, f, i, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.ReduceSeqUnroll(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
         fun[ExpType ->: ExpType ->: ExpType](exp"[$b, $read]" ->: exp"[$a, $read]" ->: exp"[$b, $write]", f =>
            fun[ExpType](exp"[$b, $write]", i =>
              fun[ExpType](exp"[$n.$a, $read]", e =>
                ReduceSeqUnroll(n, a, b, f, i, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.OclReduceSeq(),
      rt.DepFunType(i: rt.AddressSpaceIdentifier,
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        val unappPrim =
          DepLambda[AddressSpaceKind](i_space)(
            fun[ExpType ->: ExpType ->: ExpType](exp"[$b, $read]" ->: exp"[$a, $read]" ->: exp"[$b, $write]", f =>
              fun[ExpType](exp"[$b, $write]", i =>
                fun[ExpType](exp"[$n.$a, $read]", e =>
                  OpenCLReduceSeq(n, i_space, a, b, f, i, e, unroll = false)))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.OclReduceSeqUnroll(),
      rt.DepFunType(i: rt.AddressSpaceIdentifier,
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        val unappPrim =
          DepLambda[AddressSpaceKind](i_space)(
            fun[ExpType ->: ExpType ->: ExpType](exp"[$b, $read]" ->: exp"[$a, $read]" ->: exp"[$b, $write]", f =>
              fun[ExpType](exp"[$b, $write]", i =>
                fun[ExpType](exp"[$n.$a, $read]", e =>
                  OpenCLReduceSeq(n, i_space, a, b, f, i, e, unroll = true)))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.ScanSeq(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType ->: ExpType ->: ExpType](exp"[$a, $read]" ->: exp"[$b, $read]" ->: exp"[$b, $write]", f =>
            fun[ExpType](exp"[$b, $write]", i =>
              fun[ExpType](exp"[$n.$a, $read]", e =>
                ScanSeq(n, a, b, f, i, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.DepJoin(),
        rt.FunType(rt.DepArrayType(n, llenF), rt.ArrayType(_, la)))
        =>
        val a = dataType(la)
        val lenF: NatToNatLambda = ??? // fromLift(llenF)
        val unappPrim =
          fun[ExpType](exp"[$n.${NatToDataLambda(n, (i:NatIdentifier) => ArrayType(lenF(i), a))}, $read]", e =>
            DepJoin(n, lenF, a, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Join(),
      rt.FunType(rt.ArrayType(n, rt.ArrayType(m, la)), _))
      =>
        val a = dataType(la)
        val unappPrim =
          if (args.nonEmpty) {
            val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
            fun[ExpType](exp"[$n.$m.$a, $access]", e =>
              Join(n, m, access, a, e))
          } else {
            val ai = accessTypeIdentifier()
            DepLambda[AccessKind](ai)(
              fun[ExpType](exp"[$n.$m.$a, $ai]", e =>
                Join(n, m, ai, a, e)))
          }
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Split(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, ra), rt.ArrayType(m, _))))
      =>
        val a = dataType(ra)
        val unappPrim =
          if (args.nonEmpty) {
            val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
            DepLambda[NatKind](natIdentifier(n))(
                fun[ExpType](exp"[$mn.$a, $access]", e =>
                  Split(n, m, access, a, e)))
          } else {
            val ai = accessTypeIdentifier()
            DepLambda[NatKind](natIdentifier(n))(
              DepLambda[AccessKind](ai)(
                fun[ExpType](exp"[$mn.$a, $ai]", e =>
                  Split(n, m, ai, a, e))))
          }
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Slide(),
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(insz, la), rt.ArrayType(n, _)))))
      =>
        val a = dataType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(sz))(
            DepLambda[NatKind](natIdentifier(sp))(
              //TODO
              fun[ExpType](exp"[$insz.$a, $read]", e =>
                Slide(n, sz, sp, a, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.SlideSeq(rot),
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(insz, ls), rt.ArrayType(n, lt)))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(sz))(
            DepLambda[NatKind](natIdentifier(sp))(
              fun[ExpType ->: ExpType](ExpType(s, read) ->: ExpType(s, write), write_dt1 =>
                fun[ExpType ->: ExpType](exp"[$sz.$s, $read]" ->: ExpType(t, write), f =>
                  fun[ExpType](exp"[$insz.$s, $read]", e =>
                    SlideSeq(rot, n, sz, sp, s, t, write_dt1, f, e))))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.OclSlideSeq(rot),
      rt.DepFunType(la: rt.AddressSpaceIdentifier,
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(insz, ls), rt.ArrayType(n, lt))))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        val a = addressSpaceIdentifier(la)
        val unappPrim =
          DepLambda[AddressSpaceKind](a)(
            DepLambda[NatKind](natIdentifier(sz))(
              DepLambda[NatKind](natIdentifier(sp))(
                fun[ExpType ->: ExpType](ExpType(s, read) ->: ExpType(s, write), write_dt1 =>
                  fun[ExpType ->: ExpType](exp"[$sz.$s, $read]" ->: ExpType(t, write), f =>
                    fun[ExpType](exp"[$insz.$s, $read]", e =>
                      OpenCLSlideSeq(rot, a, n, sz, sp, s, t, write_dt1, f, e)))))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Reorder(),
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)

        val unappPrim =
          if (args.length == 3) {
            val access = args(2).asInstanceOf[Phrase[ExpType]].t.accessType
            fun[ExpType ->: ExpType](exp"[idx($n), $read]" ->: exp"[idx($n), $read]", idxF =>
              fun[ExpType ->: ExpType](exp"[idx($n), $read]" ->: exp"[idx($n), $read]", idxFinv =>
                fun[ExpType](exp"[$n.$a, $access]", e =>
                  Reorder(n, a, access, idxF, idxFinv, e))))
          } else if (args.length < 3) {
            val ai = accessTypeIdentifier()
            fun[ExpType ->: ExpType](exp"[idx($n), $read]" ->: exp"[idx($n), $read]", idxF =>
              fun[ExpType ->: ExpType](exp"[idx($n), $read]" ->: exp"[idx($n), $read]", idxFinv =>
                DepLambda[AccessKind](ai)(
                  fun[ExpType](exp"[$n.$a, $ai]", e =>
                    Reorder(n, a, ai, idxF, idxFinv, e)))))
          } else {
            throw new Exception("This should never happen.")
          }
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Gather(),
      rt.FunType(rt.ArrayType(m, _),
      rt.FunType(rt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$m.idx($n), $read]", y =>
            fun[ExpType](exp"[$n.$a, $read]", x =>
              Gather(n, m, a, y, x)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Transpose(),
      rt.FunType(rt.ArrayType(n, rt.ArrayType(m, la)), _))
      =>
        val a = dataType(la)
/* FIXME?

        val transposeFunction =
          λ(ExpType(IndexType(n * m), read))(i => {
            AsIndex(n * m, mapTransientNat(i, j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            }))
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m), read))(i => {
            AsIndex(n * m, mapTransientNat(i, j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            }))
          })

        fun[ExpType](exp"[$n.$m.$a, $read]", e =>
          Split(n, m, read, a,
            Reorder(n * m, a, transposeFunction, transposeInverseFunction,
              Join(n, m, read, a, e))))

 */
        //FIXME fails if Transpose appears unapplied as a function argument, same goes for other primitives as well
        val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
        val unappPrim =
          fun[ExpType](exp"[$n.$m.$a, $access]", e =>
            Transpose(n, m, a, access, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Take(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(nm, la), lw)))
      =>
        val m = nm - n
        val a = dataType(la)
        //FIXME fails if Take appears unapplied as a function argument, same goes for other primitives as well
        val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
        val unappPrim =
          DepLambda[NatKind](natIdentifier(n))(
            fun[ExpType](exp"[$nm.$a, $access]", e =>
              Take(n, m, access, a, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Drop(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = dataType(la)
        val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
        val unappPrim =
          DepLambda[NatKind](natIdentifier(n))(
            fun[ExpType](exp"[$nm.$a, $access]", e =>
              Drop(n, m, access, a, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.PadCst(),
      rt.DepFunType(l: rt.NatIdentifier,
      rt.DepFunType(r: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(l))(
            DepLambda[NatKind](natIdentifier(r))(
              fun[ExpType](exp"[$a, $read]", cst =>
                  fun[ExpType](exp"[$n.$a, $read]", e =>
                    Pad(n, l, r, a, cst, e)))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.PadClamp(),
      rt.DepFunType(l: rt.NatIdentifier,
      rt.DepFunType(r: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(l))(
            DepLambda[NatKind](natIdentifier(r))(
                fun[ExpType](exp"[$n.$a, $read]", e =>
                  PadClamp(n, l, r, a, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Unzip(),
      rt.FunType(
      rt.ArrayType(n, rt.PairType(la, lb)),
      rt.PairType(rt.ArrayType(_, _), rt.ArrayType(_, _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
        fun[ExpType](exp"[$n.($a x $b), $read]", e =>
            Unzip(n, a, b, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Zip(),
      rt.FunType(rt.ArrayType(n, la),
      rt.FunType(rt.ArrayType(_, lb), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType](exp"[$n.$a, $read]", x =>
            fun[ExpType](exp"[$n.$b, $read]", y =>
              Zip(n, a, b, x, y)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Fst(),
      rt.FunType(rt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType](exp"[($a x $b), $read]", e => Fst(a, b, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.MapFst(),
      rt.FunType(rt.FunType(la: rt.DataType, la2: rt.DataType),
      rt.FunType(rt.PairType(_, lb), _)))
      =>
        val a = dataType(la)
        val a2 = dataType(la2)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType ->: ExpType](exp"[$a, $read]" ->: exp"[$a2, $read]", f =>
            fun[ExpType](exp"[($a x $b), $read]", e => MapFst(a, b, a2, f, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Snd(),
      rt.FunType(rt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType](exp"[($a x $b), $read]", e => Snd(a, b, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.MapSnd(),
      rt.FunType(rt.FunType(lb: rt.DataType, lb2: rt.DataType),
      rt.FunType(rt.PairType(la, _), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val b2 = dataType(lb2)
        val unappPrim =
          fun[ExpType ->: ExpType](exp"[$b, $read]" ->: exp"[$b2, $read]", f =>
            fun[ExpType](exp"[($a x $b), $read]", e => MapSnd(a, b, b2, f, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Pair(),
      rt.FunType(la: rt.DataType,
      rt.FunType(lb: rt.DataType, _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", x =>
            fun[ExpType](exp"[$b, $read]", y =>
              Pair(a, b, x, y)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Idx(),
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[idx($n), $read]", i =>
            fun[ExpType](exp"[$n.$a, $read]", e =>
              FunctionalPrimitives.Idx(n, a, i, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Select(),
      rt.FunType(_,
      rt.FunType(la: rt.DataType, _)))
      =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](ExpType(bool, read), c =>
            fun[ExpType](ExpType(a, read), tExpr =>
              fun[ExpType](ExpType(a, read), fExpr =>
                IfThenElse(c, tExpr, fExpr))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Neg(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e => UnaryOp(Operators.Unary.NEG, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Add(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.ADD, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Sub(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.SUB, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Mul(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.MUL, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Div(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.DIV, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Mod(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.MOD, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Gt(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.GT, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Lt(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.LT, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Equal(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType](exp"[$a, $read]", e1 =>
            fun[ExpType](exp"[$a, $read]", e2 => BinOp(Operators.Binary.EQ, e1, e2)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Cast(), rt.FunType(la: rt.BasicType, lb: rt.BasicType))
      =>
        val a = basicType(la)
        val b = basicType(lb)
        val unappPrim =
          fun[ExpType](ExpType(a, read), x =>
            Cast(a, b, x))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Let(), rt.FunType(rt.FunType(la: rt.DataType, lb: rt.DataType), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val unappPrim =
          fun[ExpType ->: ExpType](exp"[$a, $read]" ->: exp"[$b, $read]", f =>
            fun[ExpType](ExpType(a, read), x =>
              Let(a, b, x, f)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (f @ r.ForeignFunction(decl), _)
      =>
        val (inTs, outT) = foreignFunIO(f.t)
        val unappPrim = wrapForeignFun(decl, inTs, outT, Vector())
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Generate(), rt.FunType(_, rt.ArrayType(n, la)))
      =>
        val a = dataType(la)
        val unappPrim =
          fun[ExpType ->: ExpType](exp"[idx($n), $read]" ->: exp"[$a, $read]", f =>
            Generate(n, a, f))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.MakeArray(_), lt) =>
        //TODO applying args?!
        wrapArray(lt, Vector())

      case (core.Iterate(),
      rt.DepFunType(k: rt.NatIdentifier,
      rt.FunType(rt.DepFunType(ll: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(ln, _), _)),
      rt.FunType(rt.ArrayType(insz, _), rt.ArrayType(m, la)))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val a = dataType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(k))(
            fun[`(nat)->:`[ExpType ->: ExpType]](l `()->:` (exp"[$ln.$a, $read]" ->: exp"[$l.$a, $write]"), f =>
                fun[ExpType](exp"[$insz.$a, $read]", e =>
                  Iterate(n, m, k, a, f, e))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.OclIterate(),
      rt.DepFunType(la: rt.AddressSpaceIdentifier,
      rt.DepFunType(k: rt.NatIdentifier,
      rt.FunType(rt.DepFunType(ll: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(ln, _), _)),
      rt.FunType(rt.ArrayType(insz, _), rt.ArrayType(m, ldt))))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val dt = dataType(ldt)
        val a = addressSpaceIdentifier(la)
        val unappPrim =
          DepLambda[AddressSpaceKind](a)(
            DepLambda[NatKind](natIdentifier(k))(
              fun[`(nat)->:`[ExpType ->: ExpType]](l `()->:` (exp"[$ln.$dt, $read]" ->: exp"[$l.$dt, $read]"), f =>
                fun[ExpType](exp"[$insz.$dt, $read]", e =>
                  OpenCLIterate(a, n, m, k, dt, f, e)))))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.AsVector(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, la: rt.ScalarType), rt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(n))(
            fun[ExpType](exp"[$mn.$a, $read]", e =>
              AsVector(n, m, a, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.AsVectorAligned(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, la: rt.ScalarType), rt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        val unappPrim =
          DepLambda[NatKind](natIdentifier(n))(
            fun[ExpType](exp"[$mn.$a, $read]", e =>
              AsVectorAligned(n, m, a, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.AsScalar(), rt.FunType(rt.ArrayType(m, rt.VectorType(n, la: rt.ScalarType)), _))
      =>
        val a = scalarType(la)
        val access = args.head.asInstanceOf[Phrase[ExpType]].t.accessType
        val unappPrim =
          fun[ExpType](ExpType(ArrayType(m, VectorType(n, a)), access), e =>
            AsScalar(m, n, a, access, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.VectorFromScalar(), rt.FunType(_, rt.VectorType(n, la: rt.ScalarType)))
      =>
        val a = scalarType(la)
        val unappPrim =
          fun[ExpType](ExpType(a, read), e =>
            VectorFromScalar(n, a, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.IndexAsNat(), rt.FunType(rt.IndexType(n), rt.NatType))
      =>
        val unappPrim =
          fun[ExpType](exp"[idx($n), $read]", e =>
            IndexAsNat(n, e))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (ocl.ToMem(),
      rt.DepFunType(las: rt.AddressSpaceIdentifier,
      rt.FunType(la: rt.DataType, _)))
      =>
        val a = dataType(la)
        val as = addressSpaceIdentifier(las)
        val unappPrim =
          DepLambda[AddressSpaceKind](as)(
            fun[ExpType](exp"[$a, $write]", e =>
              To(as, a, e)))
        applyAllExistingArgs(unappPrim, args, nonPhraseArgs)

      case (core.Reduce(), _) =>
        throw new Exception(s"$p has no implementation")

      case (p, _) =>
        throw new Exception(s"Missing rule for $p")
    }
  }

  private def makeLoopMap(map: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => Phrase[_ <: PhraseType],
                      n: Nat,
                      ra: rt.DataType,
                      rb: rt.DataType,
                      args: List[Phrase[_ <: PhraseType]]): Phrase[_ <: PhraseType] = {
    val a = dataType(ra)
    val b = dataType(rb)
    fun[FunType[ExpType, ExpType]](exp"[$a, $read]" ->: exp"[$b, $write]",
      f => fun[ExpType](exp"[$n.$a, $read]", arr => map(n, a, b, f, arr)))
  }

  def foreignFunIO(t: rt.Type): (Vector[DataType], DataType) = {
    t match {
      case lo: rt.DataType =>
        (Vector(), dataType(lo))
      case rt.FunType(laa, lb) => laa match {
        case la: rt.DataType =>
          val a = dataType(la)
          val (i, o) = foreignFunIO(lb)
          (a +: i, o)
        case _ => ???
      }
      case rt.DepFunType(_, _) => throw new Exception("This should not be possible")
      case rt.TypeIdentifier(_) | rt.TypePlaceholder => throw new Exception("This should not happen")
    }
  }

  def wrapForeignFun(decl: r.ForeignFunction.Decl,
                     intTs: Vector[DataType],
                     outT: DataType,
                     args: Vector[Phrase[ExpType]]): Phrase[_ <: PhraseType] = {
    val i = args.length
    if (i < intTs.length) {
      fun[ExpType](ExpType(intTs(i), read), a =>
        wrapForeignFun(decl, intTs, outT, args :+ a))
    } else {
      ForeignFunction(decl, intTs, outT, args)
    }
  }

  def wrapArray(t: rt.Type, elements: Vector[Phrase[ExpType]]): Phrase[_ <: PhraseType] = {
    t match {
      case rt.ArrayType(_, et) => Array(dataType(et), elements)
      case rt.FunType(in: rt.DataType, t2) =>
        fun[ExpType](ExpType(dataType(in), read), e =>
          wrapArray(t2, elements :+ e))
      case _ => error(s"did not expect $t")
    }
  }
}
