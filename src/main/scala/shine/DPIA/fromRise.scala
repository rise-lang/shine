package shine.DPIA

import rise.core.{semantics => ls, types => lt}
import rise.{core => l}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.{OperationalSemantics => OpSem}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._

object fromRise {
  def apply(expr: l.Expr): Phrase[_ <: PhraseType] = {
    if (!l.IsClosedForm(expr)) {
      throw new Exception(s"expression is not in closed form: $expr")
    }
    expression(expr)
  }

  def expression(expr: l.Expr): Phrase[_ <: PhraseType] = expr match {
    case l.Identifier(name) =>
      Identifier(name, `type`(expr.t))

    case l.Lambda(x, e) => expr.t match {
      case lt.FunType(i, _) =>
        Lambda(Identifier(x.name, `type`(i)), expression(e))
      case _ => error(expr.t.toString, "a function type")
    }

    case l.App(f, e) => {
      val ef = expression(f)
        .asInstanceOf[Phrase[FunType[PhraseType, PhraseType]]]
      val ee = expression(e).asInstanceOf[Phrase[PhraseType]]
      Apply(ef, ee)
    }

    case l.DepLambda(x, e) => x match {
      case n: lt.NatIdentifier =>
        DepLambda[NatKind](natIdentifier(n))(expression(e))
      case dt: lt.DataTypeIdentifier =>
        DepLambda[DataKind](dataTypeIdentifier(dt))(expression(e))
      case a: lt.AddressSpaceIdentifier =>
        DepLambda[AddressSpaceKind](addressSpaceIdentifier(a))(expression(e))
    }

    case l.DepApp(f, x) =>
      def depApp[K <: Kind](f: l.Expr, arg: K#T): DepApply[K, PhraseType] =
        DepApply[K, PhraseType](
          expression(f).asInstanceOf[Phrase[DepFunType[K, PhraseType]]],
          arg)
      x match {
        case n: Nat => depApp[NatKind](f, n)
        case dt: lt.DataType =>
          l.lifting.liftDepFunExpr[rise.core.types.DataKind](f) match {
            case l.lifting.Reducing(r) => expression(r(dt))
            case _ => depApp[DataKind](f, dataType(dt))
          }
        case a: lt.AddressSpace => depApp[AddressSpaceKind](f, addressSpace(a))
      }

    case l.Literal(d) => d match {
      case ls.NatData(n) => Natural(n)
      case ls.IndexData(i, n) => FunctionalPrimitives.AsIndex(n, Natural(i))
      case _ => Literal(data(d))
    }

    case p: l.Primitive => primitive(p, expr.t)
  }

  def addressSpace(a: lt.AddressSpace): AddressSpace = a match {
    case lt.AddressSpace.Global => AddressSpace.Global
    case lt.AddressSpace.Local => AddressSpace.Local
    case lt.AddressSpace.Private => AddressSpace.Private
    case lt.AddressSpace.Constant => AddressSpace.Constant
    case lt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }

  def scalarType(t: lt.ScalarType): ScalarType = t match {
    case lt.bool => bool
    case lt.int => int
    case lt.i8 => i8
    case lt.i16 => i16
    case lt.i32 => i32
    case lt.i64 => i64
    case lt.u8 => u8
    case lt.u16 => u16
    case lt.u32 => u32
    case lt.u64 => u64
    case lt.f16 => f16
    case lt.f32 => f32
    case lt.f64 => f64
    case lt.NatType => NatType
  }

  def basicType(t: lt.BasicType): BasicType = t match {
    case st: lt.ScalarType => scalarType(st)
    case lt.IndexType(sz) => IndexType(sz)
    case lt.VectorType(sz, et) => et match {
      case e : lt.ScalarType => VectorType(sz, scalarType(e))
      case _ => ???
    }
  }

  def dataType(t: lt.DataType): DataType = t match {
    case bt: lt.BasicType => basicType(bt)
    case i: lt.DataTypeIdentifier => dataTypeIdentifier(i)
    case lt.ArrayType(sz, et) => ArrayType(sz, dataType(et))
    case lt.DepArrayType(sz, f) => DepArrayType(sz, ntd(f))
    case lt.PairType(a, b) => PairType(dataType(a), dataType(b))
    case lt.NatToDataApply(f, n) => NatToDataApply(ntd(f), n)
  }

  def ntd(ntd: lt.NatToData): NatToData= ntd match {
    case lt.NatToDataLambda(n, body) =>
      NatToDataLambda(natIdentifier(n), dataType(body))
    case lt.NatToDataIdentifier(x, _) => NatToDataIdentifier(x)
  }

  def ntn(ntn: lt.NatToNat): NatToNat= ntn match {
    case lt.NatToNatLambda(n, body) => NatToNatLambda(natIdentifier(n), body)
    case lt.NatToNatIdentifier(x, _) => NatToNatIdentifier(x)
  }

  def dataTypeIdentifier(dt: lt.DataTypeIdentifier): DataTypeIdentifier =
    DataTypeIdentifier(dt.name)
  def natIdentifier(n: lt.NatIdentifier): NatIdentifier =
    NatIdentifier(n.name, n.range)
  def addressSpaceIdentifier(a: lt.AddressSpaceIdentifier): AddressSpaceIdentifier =
    AddressSpaceIdentifier(a.name)
  def natToNatIdentifier(n: lt.NatToNatIdentifier): NatToNatIdentifier =
    NatToNatIdentifier(n.name)
  def natToDataIdentifier(n: lt.NatToDataIdentifier): NatToDataIdentifier =
    NatToDataIdentifier(n.name)

  def `type`(ty: lt.Type): PhraseType = ty match {
    case dt: lt.DataType => ExpType(dataType(dt), read)
    case lt.FunType(i, o) => `type`(i) ->: `type`(o)
    case lt.DepFunType(i, t) => i match {
      case dt: lt.DataTypeIdentifier =>
        dataTypeIdentifier(dt) ->: `type`(t)
      case n: lt.NatIdentifier =>
        natIdentifier(n) ->: `type`(t)
      case n2n: lt.NatToNatIdentifier =>
        natToNatIdentifier(n2n) ->: `type`(t)
      case n2d: lt.NatToDataIdentifier =>
        natToDataIdentifier(n2d) ->: `type`(t)
    }
    case lt.TypeIdentifier(_) | lt.TypePlaceholder =>
      throw new Exception("This should not happen")
  }

  def data(d: ls.Data): OpSem.Data = d match {
    case ls.ArrayData(a) => OpSem.ArrayData(a.map(data).toVector)
    case ls.PairData(a, b) => OpSem.PairData(data(a), data(b))
    case ls.BoolData(b) => OpSem.BoolData(b)
    case ls.IntData(i) => OpSem.IntData(i)
    case ls.FloatData(f) => OpSem.FloatData(f)
    case ls.DoubleData(d) => OpSem.DoubleData(d)
    case ls.VectorData(v) => OpSem.VectorData(v.map(data(_)).toVector)
    case ls.IndexData(i, n) => OpSem.IndexData(i, n)
    case ls.NatData(n) => OpSem.NatData(n)
  }

  import rise.core.{primitives => core}
  import shine.DPIA.FunctionalPrimitives._

  def fun[T <: PhraseType](
    t: T,
    f: Phrase[T] => Phrase[_ <: PhraseType]
  ): Phrase[_ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  def primitive(p: l.Primitive, t: lt.Type): Phrase[_ <: PhraseType] = {
    import rise.OpenCL.{primitives => ocl}
    import rise.OpenMP.{primitives => omp}
    import shine.OpenCL.FunctionalPrimitives._
    import shine.OpenMP.FunctionalPrimitives._

    (p, t) match {
      case (core.PrintType(msg),
        lt.FunType(lt: lt.DataType, _))
      =>
        val t = dataType(lt)
        fun[ExpType](expT(t, read), e => PrintType(msg, t, e))

      case (core.NatAsIndex(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.NatType, lt.IndexType(_))))
      =>
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](ExpType(NatType, read), e =>
            AsIndex(n, e)))

      case (core.Map(),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(Map, n, la, lb)

      case (core.MapSeq(),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapSeq, n, la, lb)

      case (core.MapSeqUnroll(),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapSeqUnroll, n, la, lb)

      case (omp.MapPar(),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapPar, n, la, lb)

      case (ocl.MapGlobal(dim),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapGlobal(dim), n, la, lb)

      case (ocl.MapLocal(dim),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapLocal(dim), n, la, lb)

      case (ocl.MapWorkGroup(dim),
      lt.FunType(lt.FunType(_, lb: lt.DataType),
      lt.FunType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapWorkGroup(dim), n, la, lb)

      case (core.DepMapSeq(),
      lt.FunType(
      lt.DepFunType(lk: lt.NatIdentifier, lt.FunType(_, _)),
      lt.FunType(lt.DepArrayType(n, la), lt.DepArrayType(_, lb))))
      =>
        val a = ntd(la)
        val b = ntd(lb)
        val k = natIdentifier(lk)
        fun[`(nat)->:`[ExpType ->: ExpType]](
          k ->: (ExpType(a(k), read) ->: ExpType(b(k), read))
          , f =>
          fun[ExpType](ExpType(DepArrayType(n, a), read), e =>
            DepMapSeq(n, a, b, f, e)))

      case (core.ReduceSeq(),
      lt.FunType(_,
      lt.FunType(lb: lt.DataType,
      lt.FunType(lt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, read), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ReduceSeq(n, a, b, f, i, e))))

      case (core.ReduceSeqUnroll(),
      lt.FunType(_,
      lt.FunType(lb: lt.DataType,
      lt.FunType(lt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, read), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ReduceSeqUnroll(n, a, b, f, i, e))))

      case (ocl.OclReduceSeq(),
      lt.DepFunType(i: lt.AddressSpaceIdentifier,
      lt.FunType(_,
      lt.FunType(lb: lt.DataType,
      lt.FunType(lt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        DepLambda[AddressSpaceKind](i_space)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
            fun[ExpType](expT(b, read), i =>
              fun[ExpType](expT(n`.`a, read), e =>
                OpenCLReduceSeq(n, i_space, a, b, f, i, e, unroll = false)))))

      case (ocl.OclReduceSeqUnroll(),
      lt.DepFunType(i: lt.AddressSpaceIdentifier,
      lt.FunType(_,
      lt.FunType(lb: lt.DataType,
      lt.FunType(lt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        DepLambda[AddressSpaceKind](i_space)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
            fun[ExpType](expT(b, read), i =>
              fun[ExpType](expT(n`.`a, read), e =>
                OpenCLReduceSeq(n, i_space, a, b, f, i, e, unroll = true)))))

      case (core.ScanSeq(),
      lt.FunType(_,
      lt.FunType(lb: lt.DataType,
      lt.FunType(lt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(a, read) ->: expT(b, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, write), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ScanSeq(n, a, b, f, i, e))))

      case (core.DepJoin(),
      lt.FunType(lt.DepArrayType(n, llenF), lt.ArrayType(_, la)))
      =>
        val a = dataType(la)
        val lenF: NatToNatLambda = ??? // fromLift(llenF)
        fun[ExpType](expT(n`.d`{ i => lenF(i)`.`a }, read), e =>
          DepJoin(n, lenF, a, e))

      case (core.Join(),
      lt.FunType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = dataType(la)
        val w = read // TODO
        fun[ExpType](expT(n`.`(m`.`a), w), e =>
          Join(n, m, w, a, e))

      case (core.Split(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(mn, la), lt.ArrayType(m, _))))
      =>
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, w), e =>
            Split(n, m, w, a, e)))

      case (core.Slide(),
      lt.DepFunType(sz: lt.NatIdentifier,
      lt.DepFunType(sp: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(insz, la), lt.ArrayType(n, _)))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(sz))(
          DepLambda[NatKind](natIdentifier(sp))(
            fun[ExpType](expT(insz`.`a, read), e =>
              Slide(n, sz, sp, a, e))))

      case (core.SlideSeq(rot),
      lt.DepFunType(sz: lt.NatIdentifier,
      lt.DepFunType(sp: lt.NatIdentifier,
      lt.FunType(_,
      lt.FunType(_,
      lt.FunType(lt.ArrayType(insz, ls), lt.ArrayType(n, lt)))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        DepLambda[NatKind](natIdentifier(sz))(
          DepLambda[NatKind](natIdentifier(sp))(
            fun[ExpType ->: ExpType](
              expT(s, read) ->: expT(s, write), write_dt1 =>
              fun[ExpType ->: ExpType](
                expT(sz`.`s, read) ->: expT(t, write), f =>
                fun[ExpType](expT(insz`.`s, read), e =>
                  SlideSeq(rot, n, sz, sp, s, t, write_dt1, f, e))))))

      case (ocl.OclSlideSeq(rot),
      lt.DepFunType(la: lt.AddressSpaceIdentifier,
      lt.DepFunType(sz: lt.NatIdentifier,
      lt.DepFunType(sp: lt.NatIdentifier,
      lt.FunType(_,
      lt.FunType(_,
      lt.FunType(lt.ArrayType(insz, ls), lt.ArrayType(n, lt))))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        val a = addressSpaceIdentifier(la)
        DepLambda[AddressSpaceKind](a)(
        DepLambda[NatKind](natIdentifier(sz))(
        DepLambda[NatKind](natIdentifier(sp))(
          fun[ExpType ->: ExpType](
            expT(s, read) ->: expT(s, write), write_dt1 =>
            fun[ExpType ->: ExpType](
              expT(sz`.`s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(insz`.`s, read), e =>
                OpenCLSlideSeq(rot, a, n, sz, sp, s, t, write_dt1, f, e)))))))

      case (core.Reorder(),
      lt.FunType(_,
      lt.FunType(_,
      lt.FunType(lt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        fun[ExpType ->: ExpType](
          expT(idx(n), read) ->: expT(idx(n), read), idxF =>
          fun[ExpType ->: ExpType](
            expT(idx(n), read) ->: expT(idx(n), read), idxFinv =>
            fun[ExpType](expT(n`.`a, read), e =>
              Reorder(n, a, idxF, idxFinv, e))))

      case (core.Gather(),
      lt.FunType(lt.ArrayType(m, _),
      lt.FunType(lt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        fun[ExpType](expT(m`.`idx(n), read), y =>
          fun[ExpType](expT(n`.`a, read), x =>
            Gather(n, m, a, y, x)))

      case (core.Transpose(),
      lt.FunType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
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
        fun[ExpType](expT(n`.`(m`.`a), read), e =>
          Transpose(n, m, a, e))

      case (core.Take(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(nm, la), lw)))
      =>
        val m = nm - n
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(nm`.`a, w), e =>
            Take(n, m, w, a, e)))

      case (core.Drop(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(nm`.`a, w), e =>
            Drop(n, m, w, a, e)))

      case (core.PadCst(),
      lt.DepFunType(l: lt.NatIdentifier,
      lt.DepFunType(r: lt.NatIdentifier,
      lt.FunType(_,
      lt.FunType(lt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(l))(
          DepLambda[NatKind](natIdentifier(r))(
            fun[ExpType](expT(a, read), cst =>
                fun[ExpType](expT(n`.`a, read), e =>
                  Pad(n, l, r, a, cst, e)))))

      case (core.PadClamp(),
      lt.DepFunType(l: lt.NatIdentifier,
      lt.DepFunType(r: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(l))(
          DepLambda[NatKind](natIdentifier(r))(
              fun[ExpType](expT(n`.`a, read), e =>
                PadClamp(n, l, r, a, e))))

      case (core.Unzip(),
      lt.FunType(
      lt.ArrayType(n, lt.PairType(la, lb)),
      lt.PairType(lt.ArrayType(_, _), lt.ArrayType(_, _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(n`.`(a x b), read), e =>
            Unzip(n, a, b, e))

      case (core.Zip(),
      lt.FunType(lt.ArrayType(n, la),
      lt.FunType(lt.ArrayType(_, lb), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(n`.`a, read), x =>
          fun[ExpType](expT(n`.`b, read), y =>
            Zip(n, a, b, x, y)))

      case (core.Fst(),
      lt.FunType(lt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a x b, read), e => Fst(a, b, e))

      case (core.MapFst(),
      lt.FunType(lt.FunType(la: lt.DataType, la2: lt.DataType),
      lt.FunType(lt.PairType(_, lb), _)))
      =>
        val a = dataType(la)
        val a2 = dataType(la2)
        val b = dataType(lb)
        fun[ExpType ->: ExpType](expT(a, read) ->: expT(a2, read), f =>
          fun[ExpType](expT(a x b, read), e => MapFst(a, b, a2, f, e)))

      case (core.Snd(),
      lt.FunType(lt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a x b, read), e => Snd(a, b, e))

      case (core.MapSnd(),
      lt.FunType(lt.FunType(lb: lt.DataType, lb2: lt.DataType),
      lt.FunType(lt.PairType(la, _), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val b2 = dataType(lb2)
        fun[ExpType ->: ExpType](expT(b, read) ->: expT(b2, read), f =>
          fun[ExpType](expT(a x b, read), e => MapSnd(a, b, b2, f, e)))

      case (core.Pair(),
      lt.FunType(la: lt.DataType,
      lt.FunType(lb: lt.DataType, _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a, read), x =>
          fun[ExpType](expT(b, read), y =>
            Pair(a, b, x, y)))

      case (core.Idx(),
      lt.FunType(_,
      lt.FunType(lt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        fun[ExpType](expT(idx(n), read), i =>
          fun[ExpType](expT(n`.`a, read), e =>
            FunctionalPrimitives.Idx(n, a, i, e)))

      case (core.Select(),
      lt.FunType(_,
      lt.FunType(la: lt.DataType, _)))
      =>
        val a = dataType(la)
        fun[ExpType](ExpType(bool, read), c =>
          fun[ExpType](ExpType(a, read), tExpr =>
            fun[ExpType](ExpType(a, read), fExpr =>
              IfThenElse(c, tExpr, fExpr))))

      case (core.Neg(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e => UnaryOp(Operators.Unary.NEG, e))
      case (core.Not(), _) =>
        fun[ExpType](expT(bool, read), e => UnaryOp(Operators.Unary.NOT, e))

      case (core.Add(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.ADD, e1, e2)))
      case (core.Sub(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.SUB, e1, e2)))
      case (core.Mul(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.MUL, e1, e2)))
      case (core.Div(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.DIV, e1, e2)))
      case (core.Mod(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.MOD, e1, e2)))

      case (core.Gt(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.GT, e1, e2)))
      case (core.Lt(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.LT, e1, e2)))
      case (core.Equal(), lt.FunType(la: lt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.EQ, e1, e2)))

      case (core.Cast(), lt.FunType(la: lt.BasicType, lb: lt.BasicType))
      =>
        val a = basicType(la)
        val b = basicType(lb)
        fun[ExpType](ExpType(a, read), x =>
          Cast(a, b, x))

      case (core.Let(), lt.FunType(lt.FunType(la: lt.DataType, lb: lt.DataType), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType](expT(a, read) ->: expT(b, read), f =>
          fun[ExpType](ExpType(a, read), x =>
            Let(a, b, x, f)))

      case (f @ l.ForeignFunction(decl), _)
      =>
        val (inTs, outT) = foreignFunIO(f.t)
        wrapForeignFun(decl, inTs, outT, Vector())

      case (core.Generate(), lt.FunType(_, lt.ArrayType(n, la)))
      =>
        val a = dataType(la)
        fun[ExpType ->: ExpType](expT(idx(n), read) ->: expT(a, read), f =>
          Generate(n, a, f))

      case (core.MakeArray(_), lt) =>
        wrapArray(lt, Vector())

      case (core.Iterate(),
      lt.DepFunType(k: lt.NatIdentifier,
      lt.FunType(lt.DepFunType(ll: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(ln, _), _)),
      lt.FunType(lt.ArrayType(insz, _), lt.ArrayType(m, la)))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(k))(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`a, read) ->: expT(l`.`a, read)), f =>
              fun[ExpType](expT(insz`.`a, read), e =>
                Iterate(n, m, k, a, f, e))))

      case (ocl.OclIterate(),
      lt.DepFunType(la: lt.AddressSpaceIdentifier,
      lt.DepFunType(k: lt.NatIdentifier,
      lt.FunType(lt.DepFunType(ll: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(ln, _), _)),
      lt.FunType(lt.ArrayType(insz, _), lt.ArrayType(m, ldt))))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val dt = dataType(ldt)
        val a = addressSpaceIdentifier(la)
        DepLambda[AddressSpaceKind](a)(
          DepLambda[NatKind](natIdentifier(k))(
            fun[`(nat)->:`[ExpType ->: ExpType]](
              l ->: (expT(ln`.`dt, read) ->: expT(l`.`dt, read)), f =>
              fun[ExpType](expT(insz`.`dt, read), e =>
                OpenCLIterate(a, n, m, k, dt, f, e)))))

      case (core.AsVector(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(mn, la: lt.ScalarType), lt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, read), e =>
            AsVector(n, m, a, e)))

      case (core.AsVectorAligned(),
      lt.DepFunType(n: lt.NatIdentifier,
      lt.FunType(lt.ArrayType(mn, la: lt.ScalarType), lt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, read), e =>
            AsVectorAligned(n, m, a, e)))

      case (core.AsScalar(),
      lt.FunType(lt.ArrayType(m, lt.VectorType(n, la: lt.ScalarType)), _))
      =>
        val a = scalarType(la)
        fun[ExpType](ExpType(ArrayType(m, VectorType(n, a)), read), e =>
          AsScalar(m, n, a, e))

      case (core.VectorFromScalar(),
      lt.FunType(_, lt.VectorType(n, la: lt.ScalarType)))
      =>
        val a = scalarType(la)
        fun[ExpType](ExpType(a, read), e =>
          VectorFromScalar(n, a, e))

      case (core.IndexAsNat(), lt.FunType(lt.IndexType(n), lt.NatType))
      =>
        fun[ExpType](expT(idx(n), read), e =>
          IndexAsNat(n, e))

      case (ocl.OclToMem(),
      lt.DepFunType(las: lt.AddressSpaceIdentifier,
      lt.FunType(la: lt.DataType, _)))
      =>
        val a = dataType(la)
        val as = addressSpaceIdentifier(las)
        DepLambda[AddressSpaceKind](as)(
          fun[ExpType](expT(a, write), e =>
            To(as, a, e)))

      case (core.Reduce(), _) =>
        throw new Exception(s"$p has no implementation")

      case (p, _) =>
        throw new Exception(s"Missing rule for $p : ${p.t}")
    }
  }

  private def makeMap(map: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => Phrase[_ <: PhraseType],
                      n: Nat,
                      la: lt.DataType,
                      lb: lt.DataType): Phrase[_ <: PhraseType] = {
    val a = dataType(la)
    val b = dataType(lb)
    fun[ExpType ->: ExpType](ExpType(a, read) ->: ExpType(b, write), f =>
      fun[ExpType](expT(n`.`a, read), e =>
        map(n, a, b, f, e)))
  }

  def foreignFunIO(t: lt.Type): (Vector[DataType], DataType) = {
    t match {
      case lo: lt.DataType =>
        (Vector(), dataType(lo))
      case lt.FunType(laa, lb) => laa match {
        case la: lt.DataType =>
          val a = dataType(la)
          val (i, o) = foreignFunIO(lb)
          (a +: i, o)
        case _ => ???
      }
      case lt.DepFunType(_, _) =>
        throw new Exception("This should not be possible")
      case lt.TypeIdentifier(_) | lt.TypePlaceholder =>
        throw new Exception("This should not happen")
    }
  }

  def wrapForeignFun(decl: l.ForeignFunction.Decl,
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

  def wrapArray(
    t: lt.Type,
    elements: Vector[Phrase[ExpType]]
  ): Phrase[_ <: PhraseType] = {
    t match {
      case lt.ArrayType(_, et) => Array(dataType(et), elements)
      case lt.FunType(in: lt.DataType, t2) =>
        fun[ExpType](ExpType(dataType(in), read), e =>
          wrapArray(t2, elements :+ e))
      case _ => error(s"did not expect $t")
    }
  }
}