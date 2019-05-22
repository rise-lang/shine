package idealised.DPIA

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.Semantics.{OperationalSemantics => OpSem}
import lift.{core => l}
import lift.core.{types => lt}
import lift.core.{semantics => ls}

object fromLift {
  def apply(expr: l.Expr): Phrase[_ <: PhraseType] = {
    expr match {
      case l.TypedExpr(l.Identifier(name), t) =>
        Identifier(name, fromLift(t))
      case l.TypedExpr(l.Lambda(x, e), lt.FunctionType(i, _)) =>
        Lambda(Identifier(x.name, fromLift(i)), fromLift(e))
      case l.TypedExpr(l.Apply(f, e), _) =>
        Lifting.liftFunction( // TODO: should we try to reduce by lifting here?
          fromLift(f).asInstanceOf[Phrase[FunctionType[PhraseType, PhraseType]]])
          .value(fromLift(e).asInstanceOf[Phrase[PhraseType]])
      case l.TypedExpr(l.NatLambda(n, e), _) =>
        NatDependentLambda(n, fromLift(e))
      case l.TypedExpr(l.NatApply(f, n), _) =>
        NatDependentApply( // TODO: should we try to reduce by lifting here?
          fromLift(f).asInstanceOf[Phrase[NatDependentFunctionType[PhraseType]]],
          n)
      case l.TypedExpr(l.TypeLambda(dt, e), _) =>
        TypeDependentLambda(DataTypeIdentifier(dt.name), fromLift(e))
      case l.TypedExpr(l.TypeApply(f, dt), _) =>
        TypeDependentApply( // TODO: should we try to reduce by lifting here?
          fromLift(f).asInstanceOf[Phrase[TypeDependentFunctionType[PhraseType]]],
          fromLift(dt)
        )
      case l.TypedExpr(l.Literal(d), _) =>
        Literal(fromLift(d))
      case l.TypedExpr(l.Index(n, sz), _) =>
        FunctionalPrimitives.AsIndex(sz, Natural(n)) // TODO: check
      case l.TypedExpr(l.NatExpr(n), _) =>
        Natural(n)
      case l.TypedExpr(p: l.Primitive, t) =>
        fromLift(p, t)
    }
  }

  def apply(t: lt.ScalarType): ScalarType = {
    t match {
      case lt.bool => bool
      case lt.int => int
      case lt.float => float
      case lt.double => double
      case lt.NatType => NatType
    }
  }

  def apply(t: lt.BasicType): BasicType = {
    t match {
      case st: lt.ScalarType => fromLift(st)
      case lt.IndexType(sz) => IndexType(sz)
      case lt.VectorType(sz, e: lt.ScalarType) => VectorType(sz, fromLift(e))
    }
  }

  def apply(t: lt.DataType): DataType = {
    t match {
      case bt: lt.BasicType => fromLift(bt)
      case lt.DataTypeIdentifier(name) => DataTypeIdentifier(name)
      case lt.ArrayType(sz, et) => ArrayType(sz, fromLift(et))
      case lt.DepArrayType(sz, et) => ???
      case lt.TupleType(a, b) => RecordType(fromLift(a), fromLift(b))
    }
  }

  def apply(ty: lt.Type): PhraseType = {
    ty match {
      case dt: lt.DataType => ExpType(fromLift(dt))
      case lt.FunctionType(i, o) => FunctionType(fromLift(i), fromLift(o))
      case lt.TypeDependentFunctionType(dt, t) =>
        TypeDependentFunctionType(DataTypeIdentifier(dt.name), fromLift(t))
      case lt.NatDependentFunctionType(n, t) =>
        NatDependentFunctionType(n, fromLift(t))
    }
  }

  def apply(d: ls.Data): OpSem.Data = {
    d match {
      case ls.ArrayData(a) => OpSem.ArrayData(a.map(fromLift(_)).toVector)
      case ls.TupleData(a, b) => OpSem.RecordData(fromLift(a), fromLift(b))
      case ls.BoolData(b) => OpSem.BoolData(b)
      case ls.IntData(i) => OpSem.IntData(i)
      case ls.FloatData(f) => OpSem.FloatData(f)
      case ls.DoubleData(d) => OpSem.DoubleData(d)
      case ls.VectorData(v) => OpSem.VectorData(v.map(fromLift(_)).toVector)
    }
  }

  import lift.core.{primitives => lp}
  import idealised.DPIA.FunctionalPrimitives._

  def fun[T <: PhraseType](t: T,
                           f: Phrase[T] => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  def apply(p: l.Primitive, t: lt.Type): Phrase[_ <: PhraseType] = {
    import lift.OpenMP.{primitives => omp}
    import lift.OpenCL.{primitives => ocl}
    import idealised.OpenMP.FunctionalPrimitives._
    import idealised.OpenCL.FunctionalPrimitives._
    import idealised.OpenCL.{GlobalMemory, LocalMemory, PrivateMemory}

    // TODO: remove surface language
    import idealised.SurfaceLanguage.Operators.Unary
    import idealised.SurfaceLanguage.Operators.Binary

    (p, t) match {
      case (lp.map,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(Map, n, la, lb)

      case (lp.mapSeq,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapSeq, n, la, lb)

      case (omp.mapPar,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapPar, n, la, lb)

      case (ocl.mapGlobal(dim),
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapGlobal(dim), n, la, lb)

      case (ocl.mapLocal(dim),
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapLocal(dim), n, la, lb)

      case (ocl.mapWorkGroup(dim),
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapWorkGroup(dim), n, la, lb)

      case (lp.reduceSeq,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _ ))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ReduceSeq(n, a, b, f, i, e))))

      case (ocl.oclReduceSeq(i_space),
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _ ))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              OpenCLReduceSeq(n, a, b, f, i, i_space, e))))

      case (lp.reduceSeqUnroll,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _ ))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ReduceSeqUnroll(n, a, b, f, i, e))))

      case (lp.scanSeq,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _ ))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ScanSeq(n, a, b, f, i, e))))

      case (lp.join,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$n.$m.$a]", e =>
          Join(n, m, a, e))

      case (lp.split,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(m, _))))
      =>
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$insz.$a]", e =>
            Split(n, m, a, e)))

      case (lp.slide,
        lt.NatDependentFunctionType(sz,
        lt.NatDependentFunctionType(sp,
        lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        NatDependentLambda(sz,
          NatDependentLambda(sp,
            fun[ExpType](exp"[$insz.$a]", e =>
              Slide(n, sz, sp, a, e))))

      case (lp.slideSeq(rot),
      lt.NatDependentFunctionType(sz,
      lt.NatDependentFunctionType(sp,
      lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        NatDependentLambda(sz,
          NatDependentLambda(sp,
            fun[ExpType](exp"[$insz.$a]", e =>
              SlideSeq(rot, n, sz, sp, a, e))))

      case (lp.reorder,
      lt.FunctionType(_,
      lt.FunctionType(_,
      lt.FunctionType(lt.ArrayType(n, la), _))))
      =>
        val a = fromLift(la)
        fun[ExpType -> ExpType](exp"[idx($n)]" -> exp"[idx($n)]", idxF =>
          fun[ExpType -> ExpType](exp"[idx($n)]" -> exp"[idx($n)]", idxFinv =>
            fun[ExpType](exp"[$n.$a]", e =>
              Reorder(n, a, idxF, idxFinv, e))))

      case (lp.transpose,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            })
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            })
          })

        fun[ExpType](exp"[$n.$m.$a]", e =>
          Split(n, m, a,
            Reorder(n*m, a, transposeFunction, transposeInverseFunction,
              Join(n, m, a, e))))

      case (lp.take,
        lt.NatDependentFunctionType(n,
        lt.FunctionType(lt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$nm.$a]", e =>
            Take(n, m, a, e)))

      case (lp.drop,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$nm.$a]", e =>
            Drop(n, m, a, e)))

      case (lp.padCst,
      lt.NatDependentFunctionType(l,
      lt.NatDependentFunctionType(r,
      lt.FunctionType(_,
      lt.FunctionType(lt.ArrayType(n, la), _)))))
      =>
        val a = fromLift(la)
        NatDependentLambda(l,
          NatDependentLambda(r,
            fun[ExpType](exp"[$a]", cst =>
                fun[ExpType](exp"[$n.$a]", e =>
                  Pad(n, l, r, a, cst, e)))))

      case (lp.padClamp,
      lt.NatDependentFunctionType(l,
      lt.NatDependentFunctionType(r,
      lt.FunctionType(lt.ArrayType(n, la), _))))
      =>
        val a = fromLift(la)
        NatDependentLambda(l,
          NatDependentLambda(r,
              fun[ExpType](exp"[$n.$a]", e =>
                PadClamp(n, l, r, a, e))))

      case (lp.zip,
        lt.FunctionType(lt.ArrayType(n, la),
        lt.FunctionType(lt.ArrayType(_, lb), _)))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$n.$a]", x =>
          fun[ExpType](exp"[$n.$b]", y =>
            Zip(n, a, b, x, y)))

      case (lp.fst,
        lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Fst(a, b, e))

      case (lp.snd,
      lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Snd(a, b, e))

      case (lp.idx,
      lt.FunctionType(_,
      lt.FunctionType(lt.ArrayType(n, la), _)))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[idx($n)]", i =>
          fun[ExpType](exp"[$n.$a]", e =>
            ImperativePrimitives.Idx(n, a, i, e)))

      case (lp.select,
      lt.FunctionType(_,
      lt.FunctionType(la: lt.DataType, _)))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(bool), c =>
          fun[ExpType](ExpType(a), tExpr =>
            fun[ExpType](ExpType(a), fExpr =>
              IfThenElse(c, tExpr, fExpr))))

      case (lp.neg, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e => UnaryOp(Unary.NEG, e))

      case (lp.add, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.ADD, e1, e2)))
      case (lp.sub, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.SUB, e1, e2)))
      case (lp.mul, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.MUL, e1, e2)))
      case (lp.div, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.DIV, e1, e2)))
      case (lp.mod, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.MOD, e1, e2)))

      case (lp.gt, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.GT, e1, e2)))
      case (lp.lt, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.LT, e1, e2)))
      case (lp.equal, lt.FunctionType(la: lt.DataType, _)) =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e1 =>
          fun[ExpType](exp"[$a]", e2 => BinOp(Binary.EQ, e1, e2)))

      case (lp.cast, lt.FunctionType(la: lt.BasicType, lb: lt.BasicType))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](ExpType(a), x =>
          Cast(a, b, x))

      case (lp.ForeignFun(decl, la), _)
      =>
        val (inTs, outT) = foreignFunIO(la)
        wrapForeignFun(decl, inTs, outT, Vector())
      case (lp.generate, lt.FunctionType(_, lt.ArrayType(n, la)))
      =>
        val a = fromLift(la)
        fun[ExpType -> ExpType](exp"[idx($n)]" -> ExpType(a), f =>
          Generate(n, a, f))

      case (lp.iterate,
      lt.NatDependentFunctionType(k,
      lt.FunctionType(lt.NatDependentFunctionType(l,
        lt.FunctionType(lt.ArrayType(ln, _), _)),
      lt.FunctionType(lt.ArrayType(insz, _), lt.ArrayType(m, la)))))
      =>
        val n = ln /^ l
        val a = fromLift(la)
        NatDependentLambda(k,
          fun[`(nat)->`[ExpType -> ExpType]](
            NatDependentFunctionType(l, exp"[$ln.$a]" -> exp"[$l.$a]"), f =>
            fun[ExpType](exp"[$insz.$a]", e =>
              Iterate(n, m, k, a, f, e))))

      case (lp.asVector,
        lt.NatDependentFunctionType(n,
        lt.FunctionType(lt.ArrayType(mn, la: lt.ScalarType), lt.ArrayType(m, _))))
      =>
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$mn.$a]", e =>
            AsVector(n, m, a, e)))

      case (lp.asScalar, lt.FunctionType(lt.ArrayType(m, lt.VectorType(n, la: lt.ScalarType)), _))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(ArrayType(m, VectorType(n, a))), e =>
          AsScalar(m, n, a, e))

      case (lp.vectorFromScalar, lt.FunctionType(_, lt.VectorType(n, la: lt.ScalarType)))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(a), e =>
          VectorFromScalar(n, a, e))

      case (lp.indexAsNat, lt.FunctionType(lt.IndexType(n), lt.NatType))
      =>
        fun[ExpType](exp"[idx($n)]", e =>
          IndexAsNat(n, e))

      case (ocl.to(i_space),
      lt.FunctionType(lt.FunctionType(la: lt.DataType, lb: lt.DataType), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> ExpType](exp"[$a]" -> exp"[$b]", f =>
          fun[ExpType](exp"[$a]", e =>
            i_space match {
              case GlobalMemory => ToGlobal(a, b, f, e)
              case LocalMemory => ToLocal(a, b, f, e)
              case PrivateMemory => ToPrivate(a, b, f, e)
            }))

      case (lp.reduce, _) | (lp.scan, _) =>
        throw new Exception(s"$p has no implementation")
    }
  }

  def makeMap(map: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => Phrase[_ <: PhraseType],
              n: Nat,
              la: lt.DataType,
              lb: lt.DataType): Phrase[_ <: PhraseType] = {
    val a = fromLift(la)
    val b = fromLift(lb)
    fun[ExpType -> ExpType](ExpType(a) -> ExpType(b), f =>
      fun[ExpType](exp"[$n.$a]", e =>
        map(n, a, b, f, e)))
  }

  def foreignFunIO(t: lt.Type): (Vector[DataType], DataType) = {
    t match {
      case lt.FunctionType(la: lt.DataType, lb) =>
        val a = fromLift(la)
        val (i, o) = foreignFunIO(lb)
        (a +: i, o)
      case lo: lt.DataType =>
        (Vector(), fromLift(lo))
    }
  }

  def wrapForeignFun(decl: lp.ForeignFunDecl,
                     intTs: Vector[DataType],
                     outT: DataType,
                     args: Vector[Phrase[ExpType]]): Phrase[_ <: PhraseType] = {
    val i = args.length
    if (i < intTs.length) {
      fun[ExpType](ExpType(intTs(i)), a =>
        wrapForeignFun(decl, intTs, outT, args :+ a))
    } else {
      ForeignFunction(decl, intTs, outT, args)
    }
  }
}