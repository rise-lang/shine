package shine.DPIA

import elevate.core.strategies.Traversable
import elevate.core.strategies.basic.normalize
import elevate.rise.Rise
import elevate.rise.rules._
import rise.core.{semantics => rs, types => rt}
import rise.{core => r}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.{OperationalSemantics => OpSem}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._

object fromRise {
  def apply(expr: r.Expr)(implicit ev: Traversable[Rise]): Phrase[_ <: PhraseType] = {
    if (!r.IsClosedForm(expr)) {
      throw new Exception(s"expression is not in closed form: $expr")
    }
    val bnfExpr = normalize(ev).apply(betaReduction)(expr).get
    val rwMap = inferAccess(bnfExpr)
    expression(bnfExpr, rwMap)
  }

  def expression(
    expr: r.Expr,
    ptMap: MutableIdentityHashMap[r.Expr, PhraseType]): Phrase[_ <: PhraseType] = expr match {

    case r.Identifier(name) =>
      Identifier(name, ptMap(expr))

    case r.Lambda(x, e) =>
      Lambda(Identifier(x.name, ptMap(x)), expression(e, ptMap))

    case r.App(f, e) =>
      val ef = expression(f, ptMap)
        .asInstanceOf[Phrase[FunType[PhraseType, PhraseType]]]
      val ee = expression(e, ptMap).asInstanceOf[Phrase[PhraseType]]
      Apply(ef, ee)

    case r.DepLambda(x, e) => x match {
      case ni: rt.NatIdentifier =>
        DepLambda[NatKind](natIdentifier(ni))(expression(e, ptMap))
      case dti: rt.DataTypeIdentifier =>
        DepLambda[DataKind](dataTypeIdentifier(dti))(expression(e, ptMap))
      case addri: rt.AddressSpaceIdentifier =>
        DepLambda[AddressSpaceKind](
          addressSpaceIdentifier(addri))(expression(e, ptMap))
    }

    case r.DepApp(f, x) =>
      def depApp[K <: Kind](f: r.Expr, arg: K#T): DepApply[K, PhraseType] =
        DepApply[K, PhraseType](
          expression(f, ptMap).asInstanceOf[Phrase[DepFunType[K, PhraseType]]],
          arg)

      x match {
        case n: Nat => depApp[NatKind](f, n)
        case dt: rt.DataType =>
          depApp[DataKind](f, dataType(dt))
        case a: rt.AddressSpace => depApp[AddressSpaceKind](f, addressSpace(a))
      }

    case r.Literal(d) => d match {
      case rs.NatData(n) => Natural(n)
      case rs.IndexData(i, n) => FunctionalPrimitives.NatAsIndex(n, Natural(i))
      case _ => Literal(data(d))
    }

    case p: r.Primitive => primitive(p, ptMap(p))
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

  def fun[T <: PhraseType](
    t: T,
    f: Phrase[T] => Phrase[_ <: PhraseType]
  ): Lambda[T, _ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  object depFun {
    def apply[K <: Kind](x: K#I): Object {
      def apply[T <: PhraseType](body: Phrase[T])
                                (implicit kn: KindName[K]): DepLambda[K, T]
    } = new {
      def apply[T <: PhraseType](body: Phrase[T])
                                (implicit kn: KindName[K]): DepLambda[K, T] = DepLambda(x, body)
    }
  }

  private def primitive(p: r.Primitive,
                        t: PhraseType): Phrase[_ <: PhraseType] = {
    import rise.openCL.{primitives => ocl}
    import rise.openMP.{primitives => omp}
    import shine.OpenCL.FunctionalPrimitives._
    import shine.OpenMP.FunctionalPrimitives._
    import shine.DPIA.Types.MatchingDSL._

    (p, t) match {
      case (core.PrintType(msg), expT(dt: DataType, w) ->: _) =>
        fun[ExpType](expT(dt, w), e => PrintType(msg, dt, w, e))

      case (core.NatAsIndex(),
        nFunT(n, expT(`NatType`, `read`) ->:
          expT(IndexType(_), `read`))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT(NatType, read), e =>
            NatAsIndex(n, e)))

      case (core.Map(),
        ( expT(s, ai) ->: expT(t, _) ) ->:
          expT(ArrayType(n, _), _) ->:
          expT(ArrayType(_, _), _) ) =>
        fun[ExpType ->: ExpType](expT(s, ai) ->: expT(t, ai), f =>
          fun[ExpType](expT(ArrayType(n, s), ai), e =>
            Map(n, s, t, ai, f, e)))

      case (core.MapSeq(),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapSeq(n, s, t, f, e)))

      case (core.MapStream(),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapStream(n, s, t, f, e)))

      case (core.MapSeqUnroll(),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapSeqUnroll(n, s, t, f, e)))

      case (omp.MapPar(),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapPar(n, s, t, f, e)))

      case (ocl.MapGlobal(dim),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapGlobal(dim)(n, s, t, f, e)))

      case (ocl.MapLocal(dim),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapLocal(dim)(n, s, t, f, e)))

      case (ocl.MapWorkGroup(dim),
        ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`) ) =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapWorkGroup(dim)(n, s, t, f, e)))

      case (core.DepMapSeq(),
        nFunT(k, expT(_, `read`) ->: expT(_, `write`)) ->:
          expT(DepArrayType(n, ft1), `read`) ->:
          expT(DepArrayType(_, ft2), `write`)) =>
        fun[`(nat)->:`[ExpType ->: ExpType]](
          k ->: (ExpType(ft1(k), read) ->: ExpType(ft2(k), read)), f =>
            fun[ExpType](ExpType(DepArrayType(n, ft1), read), e =>
              DepMapSeq(n, ft1, ft2, f, e)))

      case (core.ReduceSeq(),
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`) ) =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ReduceSeq(n, s, t, f, i, e))))

      case (core.ReduceSeqUnroll(),
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`) ) =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ReduceSeqUnroll(n, t, s, f, i, e))))

      case (ocl.OclReduceSeq(),
        aFunT(a,
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`))) =>
        depFun[AddressSpaceKind](a)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(t, write), i =>
                fun[ExpType](expT(n`.`s, read), e =>
                  OpenCLReduceSeq(n, a, s, t, f, i, e, unroll = false)))))

      case (ocl.OclReduceSeqUnroll(),
        aFunT(a,
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`))) =>
        depFun[AddressSpaceKind](a)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(t, write), i =>
                fun[ExpType](expT(n`.`s, read), e =>
                  OpenCLReduceSeq(n, a, s, t, f, i, e, unroll = true)))))

      case (core.ScanSeq(),
        (expT(s, `read`) ->: expT(t, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)) =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(s, read) ->: expT(t, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ScanSeq(n, s, t, f, i, e))))

      case (core.DepJoin(),
        expT(DepArrayType(n, n2d), `read`) ->:
          expT(ArrayType(_, dt), `read`)) =>
        val lenF: NatToNatLambda = ??? // fromLift(n2d)
        fun[ExpType](expT(n`.d`{ i => lenF(i)`.`dt }, read), e =>
          DepJoin(n, lenF, dt, e))

      case (core.Join(),
        expT(ArrayType(n, ArrayType(m, t)), a) ->:
          expT(ArrayType(_, _), _)) =>
        fun[ExpType](expT(n`.`(m`.`t), a), e =>
          Join(n, m, a, t, e))

      case (core.Split(),
        nFunT(n, expT(ArrayType(_, t), a) ->:
          expT(ArrayType(m, ArrayType(_, _)), _))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT({m*n}`.`t, a), e =>
            Split(n, m, a, t, e)))

      case (core.Slide(),
        nFunT(sz, nFunT(sp,
          expT(ArrayType(insz, t), `read`) ->:
          expT(ArrayType(n, ArrayType(_, _)), `read`)))) =>
        depFun[NatKind](sz)(
          depFun[NatKind](sp)(
            fun[ExpType](expT(insz`.`t, read), e =>
              Slide(n, sz, sp, t, e))))

      case (core.CircularBuffer(),
        nFunT(alloc, nFunT(sz,
          (expT(s, `read`) ->: expT(t, `write`)) ->:
            expT(ArrayType(insz, _), `read`) ->:
            expT(ArrayType(n, _), `read`)))) =>
        depFun[NatKind](alloc)(
          depFun[NatKind](sz)(
            fun[ExpType ->: ExpType](
              expT(s, read) ->: expT(t, write), load =>
                fun[ExpType](expT(insz`.`s, read), e =>
                  // TODO: alloc
                  SlideSeq(SlideSeq.Indices, n, sz, 1, s, load, e)))))

      case (core.RotateValues(),
        nFunT(sz,
          (expT(s, `read`) ->: expT(_, `write`)) ->:
            expT(ArrayType(insz, _), `read`) ->:
            expT(ArrayType(n, _), `read`))) =>
        depFun[NatKind](sz)(
          fun[ExpType ->: ExpType](
            expT(s, read) ->: expT(s, write), wr =>
              fun[ExpType](expT(insz`.`s, read), e =>
                SlideSeq(SlideSeq.Values, n, sz, 1, s, wr, e))))

      case (ocl.OclCircularBuffer(),
        aFunT(a, nFunT(alloc, nFunT(sz,
        (expT(s, `read`) ->: expT(t, `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(n, _), `read`))))) =>
        depFun[AddressSpaceKind](a)(
          depFun[NatKind](alloc)(
            depFun[NatKind](sz)(
              fun[ExpType ->: ExpType](
                expT(s, read) ->: expT(t, write), load =>
                  fun[ExpType](expT(insz`.`s, read), e =>
                    OpenCLSlideSeq(OpenCLSlideSeq.Indices,
                      a, n, alloc, sz, s, load, e))))))

      case (ocl.OclRotateValues(),
        aFunT(a, nFunT(sz,
        (expT(s, `read`) ->: expT(t, `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(n, _), `read`)))) =>
        depFun[AddressSpaceKind](a)(
          depFun[NatKind](sz)(
              fun[ExpType ->: ExpType](
                expT(t, read) ->: expT(t, write), write_t =>
                  fun[ExpType](expT(insz`.`t, read), e =>
                    OpenCLSlideSeq(OpenCLSlideSeq.Values,
                      a, n, sz, 1, t, write_t, e)))))

      case (core.Reorder(),
        (expT(IndexType(n), `read`) ->: expT(IndexType(_), `read`)) ->:
          (expT(IndexType(_), `read`) ->: expT(IndexType(_), `read`)) ->:
          expT(ArrayType(_, t), a) ->: expT(ArrayType(_, _), _)) =>
        fun[ExpType ->: ExpType](
          expT(idx(n), read) ->: expT(idx(n), read), idxF =>
            fun[ExpType ->: ExpType](
              expT(idx(n), read) ->: expT(idx(n), read), idxFinv =>
                fun[ExpType](expT(n`.`t, a), e =>
                  Reorder(n, t, a, idxF, idxFinv, e))))

      case (core.Gather(),
        expT(ArrayType(m, IndexType(n)), `read`) ->:
          expT(ArrayType(_, t), `read`) ->:
          expT(ArrayType(_, _), `read`)) =>
        fun[ExpType](expT(m`.`idx(n), read), y =>
          fun[ExpType](expT(n`.`t, read), x =>
            Gather(n, m, t, y, x)))

      case (core.Transpose(),
        expT(ArrayType(n, ArrayType(m, t)), a) ->:
          expT(ArrayType(_, ArrayType(_, _)), _)) =>
        fun[ExpType](expT(n`.`(m`.`t), a), e =>
          Transpose(n, m, t, a, e))

      case (core.Take(),
        nFunT(n, expT(ArrayType(nm, t), `read`) ->:
          expT(ArrayType(_, _), `read`))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT(nm`.`t, read), e => Take(n, nm-n, t, e)))

      case (core.Drop(),
        nFunT(n, expT(ArrayType(nm, t), `read`) ->:
          expT(ArrayType(_, _), `read`))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT(nm`.`t, read), e =>
            Drop(n, nm-n, t, e)))

      case (core.PadCst(),
        nFunT(l, nFunT(q,
          expT(t, `read`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `read`)))) =>
        depFun[NatKind](l)(
          depFun[NatKind](q)(
            fun[ExpType](expT(t, read), cst =>
              fun[ExpType](expT(n`.`t, read), e =>
                Pad(n, l, q, t, cst, e)))))

      case (core.PadClamp(),
        nFunT(l, nFunT(q,
          expT(ArrayType(n, t), `read`) ->:
          expT(ArrayType(_, _), `read`)))) =>
        depFun[NatKind](l)(
          depFun[NatKind](q)(
            fun[ExpType](expT(n`.`t, read), e =>
              PadClamp(n, l, q, t, e))))

      case (core.Unzip(),
        expT(ArrayType(n, PairType(s, t)), a) ->:
          expT(PairType(ArrayType(_, _), ArrayType(_, _)), _)) =>
        fun[ExpType](expT(n`.`(s x t), a), e =>
          Unzip(n, s, t, a, e))

      case (core.Zip(),
        expT(ArrayType(n, s), a) ->:
          expT(ArrayType(_, t), _) ->:
          expT(ArrayType(_, PairType(_, _)), _)) =>
        fun[ExpType](expT(n`.`s, a), x =>
          fun[ExpType](expT(n`.`t, a), y =>
            Zip(n, s, t, a, x, y)))

      case (core.Fst(), expT(PairType(s, t), `read`) ->: expT(_, read)) =>
        fun[ExpType](expT(s x t, read), e => Fst(s, t, e))

      case (core.Snd(), expT(PairType(s, t), `read`) ->: expT(_, read)) =>
        fun[ExpType](expT(s x t, read), e => Snd(s, t, e))

      case (core.MapFst(),
        (expT(s, a) ->: expT(s2, _)) ->:
          expT(PairType(_, t), _) ->:
          expT(PairType(_, _), _)) =>
        fun[ExpType ->: ExpType](expT(s, a) ->: expT(s2, a), f =>
          fun[ExpType](expT(s x t, a), e => MapFst(a, s, t, s2, f, e)))

      case (core.MapSnd(),
        (expT(t, a) ->: expT(t2, _)) ->:
          expT(PairType(s, _), _) ->:
          expT(PairType(_, _), _)) =>
        fun[ExpType ->: ExpType](expT(t, a) ->: expT(t2, a), f =>
          fun[ExpType](expT(s x t, a), e => MapSnd(a, s, t, t2, f, e)))

      case (core.Pair(),
        expT(s, a) ->:
          expT(t, _) ->:
          expT(PairType(_, _), _)) =>
        fun[ExpType](expT(s, a), x =>
          fun[ExpType](expT(t, a), y =>
            Pair(s, t, a, x, y)))

      case (core.Idx(),
        expT(IndexType(n), `read`) ->:
          expT(ArrayType(_, t), `read`) ->:
          expT(_, `read`)) =>
        fun[ExpType](expT(idx(n), read), i =>
          fun[ExpType](expT(n`.`t, read), e =>
            FunctionalPrimitives.Idx(n, t, i, e)))

      case (core.Select(),
        expT(`bool`, `read`) ->:
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](ExpType(bool, read), cond =>
          fun[ExpType](ExpType(t, read), tExpr =>
            fun[ExpType](ExpType(t, read), fExpr =>
              IfThenElse(cond, tExpr, fExpr))))

      case (core.Neg(), expT(t, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e => UnaryOp(Operators.Unary.NEG, e))

      case (core.Not(), expT(`bool`, `read`) ->: expT(`bool`, `read`)) =>
        fun[ExpType](expT(bool, read), e => UnaryOp(Operators.Unary.NOT, e))

      case (core.Add(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.ADD, e1, e2)))

      case (core.Sub(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.SUB, e1, e2)))

      case (core.Mul(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.MUL, e1, e2)))

      case (core.Div(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.DIV, e1, e2)))

      case (core.Mod(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.MOD, e1, e2)))

      case (core.Gt(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`) ) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.GT, e1, e2)))

      case (core.Lt(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`) ) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.LT, e1, e2)))

      case (core.Equal(),
          expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`) ) =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.EQ, e1, e2)))

      case (core.Cast(),
          expT(s: BasicType, `read`) ->: expT(t: BasicType, `read`)) =>
        fun[ExpType](ExpType(s, read), x => Cast(s, t, x))

      case (core.Let(), expT(s, `read`) ->:
          (expT(_, `read`) ->: expT(t, a)) ->: expT(_, _)) =>
        fun[ExpType](ExpType(s, read), x =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, a), f =>
            Let(s, t, a, x, f)))

      case (r.ForeignFunction(decl), t) =>
        def collectTypes(t: PhraseType): (Vector[DataType], DataType) = {
          t match {
            case ExpType(dt: DataType, `read`) =>
              (Vector(), dt)
            case FunType(ExpType(dt: DataType, `read`), out) =>
              val (i, o) = collectTypes(out)
              (dt +: i, o)
            case _ =>
              throw new Exception("This should not be possible")
          }
        }
        val (inTs, outT) = collectTypes(t)

        def buildFFPrimitive(args: Vector[Phrase[ExpType]]
                            ): Phrase[_ <: PhraseType] = {
          val i = args.length
          if (i < inTs.length) {
            fun[ExpType](ExpType(inTs(i), read), a =>
              buildFFPrimitive(args :+ a))
          } else {
            ForeignFunction(decl, inTs, outT, args)
          }
        }
        buildFFPrimitive(Vector())

      case (core.Generate(),
        (expT(IndexType(n), `read`) ->: expT(t, `read`)) ->:
          expT(ArrayType(n_, _), `read`)) =>
        fun[ExpType ->: ExpType](
          expT(idx(n), read) ->: expT(t, read), f =>
            Generate(n, t, f))

      case (core.MakeArray(_), t) =>
        def buildArrayPrimitive(t: PhraseType, elements: Vector[Phrase[ExpType]]
                     ): Phrase[_ <: PhraseType] = t match {
          case FunType(in: ExpType, out) =>
            fun[ExpType](in, e => buildArrayPrimitive(out, elements :+ e))
          case ExpType(ArrayType(_, et), _) => MakeArray(et, elements)
          case _ => error(s"did not expect $t")
        }
        buildArrayPrimitive(t, Vector())

      case (core.Iterate(),
        nFunT(k,
          nFunT(l, expT(ArrayType(ln, t), `read`) ->:
            expT(ArrayType(_, _), `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(m, _), `write`) )) =>
        depFun[NatKind](k)(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`t, read) ->: expT(l`.`t, write)), f =>
              fun[ExpType](expT(insz`.`t, read), e =>
                Iterate(ln /^ l, m, k, t, f, e))))

      case (ocl.OclIterate(),
        aFunT(a, nFunT(k,
          nFunT(l, expT(ArrayType(ln, t), `read`) ->:
            expT(ArrayType(_, _), `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(m, _), `write`) ))) =>
        depFun[AddressSpaceKind](a)(depFun[NatKind](k)(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`t, read) ->: expT(l`.`t, write)), f =>
              fun[ExpType](expT(insz`.`t, read), e =>
                OpenCLIterate(a, ln /^ l, m, k, t, f, e)))))

      case (core.AsVector(),
        nFunT(n,
          expT(ArrayType(mn, _), a) ->:
          expT(ArrayType(m, VectorType(_, t)), _))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT(mn`.`t, a), e =>
            AsVector(n, m, t, a, e)))

      case (core.AsVectorAligned(),
        nFunT(n,
          expT(ArrayType(mn, _), a) ->:
          expT(ArrayType(m, VectorType(_, t)), _))) =>
        depFun[NatKind](n)(
          fun[ExpType](expT(mn`.`t, read), e =>
            AsVectorAligned(n, m, a, t, e)))

      case (core.AsScalar(),
        expT(ArrayType(m, VectorType(n, t)), a) ->:
          expT(ArrayType(_, _), _)) =>
        fun[ExpType](expT(m`.`vec(n, t), a), e =>
          AsScalar(m, n, t, a, e))

      case (core.VectorFromScalar(),
        expT(_, `read`) ->:
          expT(VectorType(n, t), `read`)) =>
        fun[ExpType](expT(t, read), e =>
          VectorFromScalar(n, t, e))

      case (core.IndexAsNat(),
        expT(IndexType(n), `read`) ->:
          expT(`NatType`, `read`)) =>
        fun[ExpType](expT(idx(n), read), e =>
          IndexAsNat(n, e))

      case (core.ToMem(), expT(t, `write`) ->: expT(_, `read`)) =>
        fun[ExpType](expT(t, write), e => ToMem(t, e))

      case (ocl.OclToMem(), aFunT(a, expT(t, `write`) ->: expT(_, `read`))) =>
        depFun[AddressSpaceKind](a)(
          fun[ExpType](expT(t, write), e =>
            OclToMem(a, t, e)))

      case (ocl.SetVal(),
        expT(ArrayType(n, t), `read`) ->:
          expT(IndexType(_), `read`) ->:
            expT(_, `read`) ->:
              expT(ArrayType(_, _), `read`)) =>
        fun[ExpType](expT(n`.`t, read), e =>
          fun[ExpType](expT(idx(n), read), i =>
            fun[ExpType](expT(t, read), x =>
              SetVal(n, t, e, i, x))))

      case (ocl.ReduceByIndexSeq(),
        (expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `write`)) ->:
          expT(ArrayType(k, _), `read`) ->:
            expT(ArrayType(n, `NatType`), `read`) ->:
              expT(ArrayType(_, _), `read`) ->:
                expT(ArrayType(_, _), `read`)) =>
          fun[ExpType ->: ExpType ->: ExpType](
            expT(t, read) ->: expT(t, read) ->: expT(t, write), f =>
              fun[ExpType](expT(k`.`t, read), h =>
                fun[ExpType](expT(n`.`NatType, read), i =>
                  fun[ExpType](expT(n`.`t, read), x =>
                    ReduceByIndexSeq(n, k, t, f, h, i, x)))))

      case (core.Reduce(), _) =>
        throw new Exception(s"$p has no implementation")

      case _ => throw new Exception(s"Missing rule for $p : ${p.t}")
    }
  }

  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
    case rt.AddressSpace.Global => AddressSpace.Global
    case rt.AddressSpace.Local => AddressSpace.Local
    case rt.AddressSpace.Private => AddressSpace.Private
    case rt.AddressSpace.Constant => AddressSpace.Constant
    case rt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }

  def dataType(t: rt.DataType): DataType = t match {
    case bt: rt.BasicType => basicType(bt)
    case i: rt.DataTypeIdentifier => dataTypeIdentifier(i)
    case rt.ArrayType(sz, et) => ArrayType(sz, dataType(et))
    case rt.DepArrayType(sz, f) => DepArrayType(sz, ntd(f))
    case rt.PairType(a, b) => PairType(dataType(a), dataType(b))
    case rt.NatToDataApply(f, n) => NatToDataApply(ntd(f), n)
  }

  def basicType(t: rt.BasicType): BasicType = t match {
    case st: rt.ScalarType => scalarType(st)
    case rt.IndexType(sz) => IndexType(sz)
    case rt.VectorType(sz, et) => et match {
      case e : rt.ScalarType => VectorType(sz, scalarType(e))
      case _ => ???
    }
  }

  def scalarType(t: rt.ScalarType): ScalarType = t match {
    case rt.bool => bool
    case rt.int => int
    case rt.i8 => i8
    case rt.i16 => i16
    case rt.i32 => i32
    case rt.i64 => i64
    case rt.u8 => u8
    case rt.u16 => u16
    case rt.u32 => u32
    case rt.u64 => u64
    case rt.f16 => f16
    case rt.f32 => f32
    case rt.f64 => f64
    case rt.NatType => NatType
  }

  def ntd(ntd: rt.NatToData): NatToData= ntd match {
    case rt.NatToDataLambda(n, body) =>
      NatToDataLambda(natIdentifier(n), dataType(body))
    case rt.NatToDataIdentifier(x, _) => NatToDataIdentifier(x)
  }

  def ntn(ntn: rt.NatToNat): NatToNat= ntn match {
    case rt.NatToNatLambda(n, body) => NatToNatLambda(natIdentifier(n), body)
    case rt.NatToNatIdentifier(x, _) => NatToNatIdentifier(x)
  }

  def dataTypeIdentifier(dt: rt.DataTypeIdentifier): DataTypeIdentifier =
    DataTypeIdentifier(dt.name)
  def natIdentifier(n: rt.NatIdentifier): NatIdentifier =
    NatIdentifier(n.name, n.range)
  def addressSpaceIdentifier(
    a: rt.AddressSpaceIdentifier
  ): AddressSpaceIdentifier = AddressSpaceIdentifier(a.name)
  def natToNatIdentifier(n: rt.NatToNatIdentifier): NatToNatIdentifier =
    NatToNatIdentifier(n.name)
  def natToDataIdentifier(n: rt.NatToDataIdentifier): NatToDataIdentifier =
    NatToDataIdentifier(n.name)
  def accessTypeIdentifier(): AccessTypeIdentifier =
    AccessTypeIdentifier(freshName("access"))
}
