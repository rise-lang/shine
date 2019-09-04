package idealised.DPIA

import idealised.{DPIA, SurfaceLanguage}
import idealised.DPIA.FunctionalPrimitives.TransposeDepArray
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{ExpType, NatToNatLambda, NatToDataLambda, PhraseType}
import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.OpenMP.SurfaceLanguage.Primitives.{DepMapPar, MapPar, ReducePar}
import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Types.{DataType, _}
import idealised.SurfaceLanguage._
import lift.arithmetic.{Cst, Pow, Prod}


object FromSurfaceLanguagePrimitives {

  def apply(expr: Expr): Option[Phrase[_ <: PhraseType]] = {
    expr match {
      case map: AbstractDepMap => ( (map.df.t, map.array.t) : @unchecked) match {
        case (
          Some(DependentFunctionType(k: SurfaceLanguage.NatIdentifier, FunctionType(df1_k: DataType, df2_k: DataType))),
          Some(DepArrayType(n, DependentFunctionType(_: SurfaceLanguage.NatIdentifier, _)))
          ) =>
          val ft1 = NatToDataLambda(n, (x: NatIdentifier) => Type.substitute[DataType](x, `for` = k, in = df1_k))
          val ft2 = NatToDataLambda(n, (x: NatIdentifier) => Type.substitute[DataType](x, `for` = k, in = df2_k))

          Some(makeDPIADepMap(map)(n, ft1, ft2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.DepFunType[DPIA.Types.NatKind, DPIA.Types.FunType[ExpType, ExpType]]](map.df),
            FromSurfaceLanguage.asPhrase[ExpType](map.array)))
      }

      case map: AbstractMap => (map.f.t, map.array.t) match {
        case (Some(FunctionType(dt1: DataType, dt2: DataType)), Some(ArrayType(n, dt1_))) if dt1 == dt1_ =>
          Some(makeDPIAMap(map)(n, dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[ExpType, ExpType]](map.f),
            FromSurfaceLanguage.asPhrase[ExpType](map.array)))
      }

      case red: AbstractReduce => (red.f.t, red.init.t, red.array.t) match {
        case (Some(FunctionType(t1, FunctionType(t2, t3))), Some(dt2: DataType), Some(ArrayType(n, dt1)))
          if dt1 == t1 && dt2 == t2 && dt2 == t3 =>
          Some(makeDPIAReduce(red)(n, dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[ExpType, DPIA.Types.FunType[ExpType, ExpType]]](red.f),
            FromSurfaceLanguage.asPhrase[ExpType](red.init),
            FromSurfaceLanguage.asPhrase[ExpType](red.array)))
      }

      case scan: AbstractScan => (scan.f.t, scan.array.t) match {
        case (Some(FunctionType(dt1: DataType, FunctionType(dt2: DataType, _))), Some(ArrayType(n, dt1_)))
          if dt1 == dt1_ =>
          Some(makeDPIAScan(scan)(n, dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[ExpType, DPIA.Types.FunType[ExpType, ExpType]]](scan.f),
            FromSurfaceLanguage.asPhrase[ExpType](scan.init),
            FromSurfaceLanguage.asPhrase[ExpType](scan.array)
          ))
      }

      case slide: AbstractSlide => (slide.input.t: @unchecked) match {
        case Some(ArrayType(m, dt)) =>
          val n = (m - slide.sz + slide.sp) /^ slide.sp
          Some(makeDPIASlide(slide)(n, slide.sz, slide.sp, dt, FromSurfaceLanguage.asPhrase[ExpType](slide.input)))
      }

      case AsScalar(array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, VectorType(m, dt))) =>
          Some(FunctionalPrimitives.AsScalar(n, m, dt,
            FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case AsVector(n, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(mn, st: ScalarType)) =>
          Some(FunctionalPrimitives.AsVector(n, mn /^ n, st,
            FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case Drop(n, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(m, dt)) =>
          Some(FunctionalPrimitives.Drop(n, m - n, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case ForeignFunction(funDecl, inTs, outT, args) =>
        Some(FunctionalPrimitives.ForeignFunction(
          FunctionalPrimitives.ForeignFunction.Declaration(funDecl.name,
            Some(FunctionalPrimitives.ForeignFunction.Definition(funDecl.argNames, funDecl.body))),
          inTs.map(Types.DataType(_)), Types.DataType(outT),
          args.map(FromSurfaceLanguage.asPhrase[ExpType])
        ))

      case Fst(tuple, _) => tuple.t match {
        case Some(TupleType(dt1, dt2)) =>
          Some(FunctionalPrimitives.Fst(dt1, dt2, FromSurfaceLanguage.asPhrase[ExpType](tuple)))
      }

      case Iterate(k, f, array, _) => (f.t, array.t) match {
        case (Some(DependentFunctionType(_: SurfaceLanguage.NatIdentifier,
        FunctionType(ArrayType(l, dt1), ArrayType(l_n, dt2)))), Some(ArrayType(m, dt)))
          if dt1 == dt && dt2 == dt =>
          val n = l_n match {
            case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
            case _ => throw new Exception("")
          }
          Some(FunctionalPrimitives.Iterate(n, m, k, dt,
            FromSurfaceLanguage.asPhrase[DPIA.Types.DepFunType[DPIA.Types.NatKind, DPIA.Types.FunType[ExpType, ExpType]]](f),
            FromSurfaceLanguage.asPhrase[ExpType](array)
          ))
      }

      case Join(array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, ArrayType(m, dt))) =>
          Some(FunctionalPrimitives.Join(n, m, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
        case Some(ArrayType(n, DepArrayType(m, DependentFunctionType(i: SurfaceLanguage.NatIdentifier, dt)))) =>
          ???
        case Some(DepArrayType(n, DependentFunctionType(d_i: SurfaceLanguage.NatIdentifier, ArrayType(d_n, dt)))) =>
          Some(FunctionalPrimitives.DepJoin(n, NatToNatLambda(n, NatIdentifier(d_i.name, d_i.range), d_n), dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
        case Some(DepArrayType(n, DependentFunctionType(i: SurfaceLanguage.NatIdentifier, DepArrayType(m, DependentFunctionType(j: SurfaceLanguage.NatIdentifier, dt))))) =>
          ???
      }

      case Pad(l, r, padExpr, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, dt)) =>
          Some(FunctionalPrimitives.Pad(n, l, r, dt,
            FromSurfaceLanguage.asPhrase[ExpType](padExpr), FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case Partition(m, lenF, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, dt)) =>
          Some(FunctionalPrimitives.Partition(n, m, lenF, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case PrintType(input, _, _) => Some(FromSurfaceLanguage.asPhrase[ExpType](input))

      case Reorder(idxF, idxFinv, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, dt)) =>
          Some(FunctionalPrimitives.Reorder(n, dt,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[ExpType, ExpType]](idxF),
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[ExpType, ExpType]](idxFinv),
            FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case Snd(tuple, _) => (tuple.t: @unchecked) match {
        case Some(TupleType(dt1, dt2)) =>
          Some(FunctionalPrimitives.Snd(dt1, dt2, FromSurfaceLanguage.asPhrase[ExpType](tuple)))
      }

      case Split(n, array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(mn, dt)) =>
          Some(FunctionalPrimitives.Split(n, mn /^ n, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
        case x => None
      }

      case Take(n, array, _) =>
        (array.t: @unchecked) match {
        case Some(ArrayType(m, dt)) =>
          Some(FunctionalPrimitives.Take(n, m - n, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case Transpose(array, _) => (array.t: @unchecked) match {
        case Some(ArrayType(n, ArrayType(m, dt))) =>
          import idealised.DPIA.DSL._
          import idealised.DPIA.FunctionalPrimitives._
          import idealised.DPIA.Types._

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

          Some(Split(n, m, dt,
            Reorder(n*m, dt, transposeFunction, transposeInverseFunction,
              Join(n, m, dt, FromSurfaceLanguage.asPhrase[ExpType](array)))))

        case Some(ArrayType(n, DepArrayType(m, DependentFunctionType(i: SurfaceLanguage.NatIdentifier, dt)))) =>
          Some(TransposeDepArray(n, m, NatIdentifier(i.name, i.range), dt, FromSurfaceLanguage.asPhrase[ExpType](array)))
      }

      case Generate(f, _) => (f.t: @unchecked) match {
        case Some(FunctionType(IndexType(n), dt : DataType)) =>
          Some(FunctionalPrimitives.Generate(n, dt,
            FromSurfaceLanguage.asPhrase[Types.FunType[ExpType, ExpType]](f)))
      }

      case IndexAsNat(e, _) => (e.t: @unchecked) match {
        case Some(IndexType(n)) =>
          Some(FunctionalPrimitives.IndexAsNat(n, FromSurfaceLanguage.asPhrase[ExpType](e)))
      }

      case AsIndex(n, e, _) => (e.t: @unchecked) match {
        case Some(NatType) =>
          Some(FunctionalPrimitives.AsIndex(n, FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](e)))
      }
      case Idx(e, idx, _) => Some(
        ImperativePrimitives.Idx(
          FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](idx),
          FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](e)
        ))

      case Cast(bt, e, _) => (e.t: @unchecked) match {
        case Some(edt: BasicType) =>
          def toDPIABasicType(bt: DataType): DPIA.Types.BasicType = {
            bt match {
              case SurfaceLanguage.Types.NatType => DPIA.Types.NatType
              case SurfaceLanguage.Types.IndexType(n) => DPIA.Types.IndexType(n)
              case SurfaceLanguage.Types.bool => DPIA.Types.bool
              case SurfaceLanguage.Types.int => DPIA.Types.int
              case SurfaceLanguage.Types.float => DPIA.Types.float
              case SurfaceLanguage.Types.double => DPIA.Types.double
              case dt => throw new Exception(s"Expected BasicType but found $dt")
            }
          }

          Some(FunctionalPrimitives.Cast(toDPIABasicType(edt), toDPIABasicType(bt),
            FromSurfaceLanguage.asPhrase[ExpType](e)))
      }

      case Tuple(fst, snd, _) => ( (fst.t, snd.t) : @unchecked) match {
        case (Some(dt1: DataType), Some(dt2: DataType)) =>
          Some(FunctionalPrimitives.Record(dt1, dt2,
            FromSurfaceLanguage.asPhrase[ExpType](fst),
            FromSurfaceLanguage.asPhrase[ExpType](snd)))
      }

      case Unzip(e, _) => (e.t: @unchecked) match {
        case Some(ArrayType(n, TupleType(dt1, dt2))) =>
          Some(FunctionalPrimitives.Unzip(n, dt1, dt2, FromSurfaceLanguage.asPhrase[ExpType](e)))
      }

      case VectorFromScalar(n, arg, _) => (arg.t: @unchecked) match {
        case Some(dt: ScalarType) =>
          Some(FunctionalPrimitives.VectorFromScalar(n, dt, FromSurfaceLanguage.asPhrase[ExpType](arg)))
      }

      case Zip(lhs, rhs, _) => ( (lhs.t, rhs.t) : @unchecked) match {
        case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) if n == m =>
          Some(FunctionalPrimitives.Zip(n, dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](lhs),
            FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](rhs)))
      }

      case DepZip(lhs, rhs, _) => ( (lhs.t, rhs.t) : @unchecked) match {
        case (Some(DepArrayType(n, _)), Some(DepArrayType(m, ft2))) if n == m =>
          val lhsDPIA = FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](lhs)
          val ft1 = lhsDPIA.t.dataType match  {
            case DPIA.Types.DepArrayType(_, ft1) => ft1
            case _ => throw new Exception("This should not happen")
          }
          val rhsDPIA = FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](rhs)
          val ft2 = rhsDPIA.t.dataType match {
            case DPIA.Types.DepArrayType(_, ft2) => ft2
            case _ => throw new Exception("This should not happen")
          }
          Some(FunctionalPrimitives.DepZip(n, ft1, ft2, lhsDPIA, rhsDPIA))
      }

      case OpenCLFunction(name, inTs, outT, args) =>
        Some(idealised.OpenCL.FunctionalPrimitives.OpenCLFunction(
          name, inTs.map(DPIA.Types.DataType(_)), DPIA.Types.DataType(outT),
          args.map(FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType])))

      case OpenCLReduceSeq(f, init, initAddrSpace, array, _) => ( (f.t, init.t, array.t) : @unchecked) match {
        case (Some(FunctionType(t1, FunctionType(t2, t3))), Some(dt2: DataType), Some(ArrayType(n, dt1)))
          if dt1 == t1 && dt2 == t2 && dt2 == t3 =>
          Some(idealised.OpenCL.FunctionalPrimitives.OpenCLReduceSeq(n, dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[DPIA.Types.ExpType, DPIA.Types.FunType[DPIA.Types.ExpType, DPIA.Types.ExpType]]](f),
            FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](init),
            initAddrSpace,
            FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](array), unroll = false))
      }

      case to: To => ( (to.f.t, to.input.t) : @unchecked) match {
        case (Some(FunctionType(dt1: DataType, dt2: DataType)), Some(t1)) if dt1 == t1 =>
          Some(makeDPIATo(to)(dt1, dt2,
            FromSurfaceLanguage.asPhrase[DPIA.Types.FunType[DPIA.Types.ExpType, DPIA.Types.ExpType]](to.f),
            FromSurfaceLanguage.asPhrase[DPIA.Types.ExpType](to.input)))
      }

      case _ => None
    }
  }

  def makeDPIAMap(map: AbstractMap): (Nat,
    DataType,
    DataType,
    Phrases.Phrase[DPIA.Types.FunType[ExpType, ExpType]],
    Phrases.Phrase[ExpType]
    ) => FunctionalPrimitives.AbstractMap = map match {
    case _: Map =>
      (n, dt1, dt2, f, array) => idealised.DPIA.FunctionalPrimitives.Map(n, dt1, dt2, f, array)

    case _: MapSeq =>
      (n, dt1, dt2, f, array) => idealised.DPIA.FunctionalPrimitives.MapSeq(n, dt1, dt2, f, array)
    case _: MapPar =>
      (n, dt1, dt2, f, array) => idealised.OpenMP.FunctionalPrimitives.MapPar(n, dt1, dt2, f, array)

    case MapGlobal(dim) =>
      (n, dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.MapGlobal(dim)(n, dt1, dt2, f, array)
    case MapLocal(dim, addressSpace) =>
      (n, dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.MapLocal(dim, addressSpace)(n, dt1, dt2, f, array)
    case MapWorkGroup(dim) =>
      (n, dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.MapWorkGroup(dim)(n, dt1, dt2, f, array)
  }

  def makeDPIADepMap(map: AbstractDepMap): (Nat,
    NatToDataLambda,
    NatToDataLambda,
    Phrases.Phrase[DPIA.Types.DepFunType[DPIA.Types.NatKind, DPIA.Types.FunType[ExpType, ExpType]]],
    Phrases.Phrase[ExpType]
    ) => FunctionalPrimitives.AbstractDepMap = map match {
    case _: DepMapSeq =>
      idealised.DPIA.FunctionalPrimitives.DepMapSeq
    case _: DepMapSeqUnroll =>
      idealised.DPIA.FunctionalPrimitives.DepMapSeqUnroll
    case _: DepMapPar =>
      idealised.OpenMP.FunctionalPrimitives.DepMapPar

    case DepMapGlobal(dim) =>
      idealised.OpenCL.FunctionalPrimitives.DepMapGlobal(dim)
    case DepMapLocal(dim) =>
      idealised.OpenCL.FunctionalPrimitives.DepMapLocal(dim)
    case DepMapWorkGroup(dim) =>
      idealised.OpenCL.FunctionalPrimitives.DepMapWorkGroup(dim)
  }

  def makeDPIAReduce(red: AbstractReduce): (Nat,
    DataType,
    DataType,
    Phrases.Phrase[DPIA.Types.FunType[ExpType, DPIA.Types.FunType[ExpType, ExpType]]],
    Phrases.Phrase[ExpType],
    Phrases.Phrase[ExpType]
    ) => FunctionalPrimitives.AbstractReduce = red match {
    case _: ReduceSeq =>
      (n, dt1, dt2, f, init, array) => idealised.DPIA.FunctionalPrimitives.ReduceSeq(n, dt1, dt2, f, init, array)
    case _: ReducePar =>
      (n, dt1, dt2, f, init, array) => idealised.OpenMP.FunctionalPrimitives.ReducePar(n, dt1, dt2, f, init, array)
  }

  def makeDPIAScan(scan: AbstractScan): (Nat,
    DataType,
    DataType,
    Phrase[DPIA.Types.FunType[ExpType, DPIA.Types.FunType[ExpType, ExpType]]],
    Phrase[ExpType],
    Phrase[ExpType]
    ) => FunctionalPrimitives.AbstractScan = scan match {
    case _: ScanSeq =>
      (n, dt1, dt2, f, init, array) => idealised.DPIA.FunctionalPrimitives.ScanSeq(n, dt1, dt2, f, init, array)
  }


  def makeDPIASlide(slide: AbstractSlide): (Nat, Nat, Nat, DataType, Phrase[ExpType]) => Phrase[ExpType] = slide match {
    case _: Slide =>
      (n, sz, sp, dt, input) => DPIA.FunctionalPrimitives.Slide(n, sz, sp, dt, input)
    case _: SlideSeq =>
      (n, sz, sp, dt, input) => DPIA.FunctionalPrimitives.SlideSeq(lift.core.primitives.slideSeq.Values, n, sz, sp, dt, input)
  }

  def makeDPIATo(to: To): (DataType, DataType, Phrase[DPIA.Types.FunType[ExpType, ExpType]], Phrase[ExpType])
    => idealised.OpenCL.FunctionalPrimitives.To = to match {
    case _: ToPrivate =>
      (dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.ToPrivate(dt1, dt2, f, array)
    case _: ToLocal =>
      (dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.ToLocal(dt1, dt2, f, array)
    case _: ToGlobal =>
      (dt1, dt2, f, array) => idealised.OpenCL.FunctionalPrimitives.ToGlobal(dt1, dt2, f, array)
  }

}
