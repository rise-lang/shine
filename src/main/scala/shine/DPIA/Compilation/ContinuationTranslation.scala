package shine.DPIA.Compilation

import rise.core.types.{AddressSpace, DataType, NatKind, read, write}
import rise.core.types.DataType._
import rise.core.DSL.Type._
import rise.core.substitute.{natInType => substituteNatInType}
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL.{comment, _}
import shine.DPIA.Phrases._
import rise.core.types.DataTypeOps._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{Seq => _, _}
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenMP.primitives.{functional => omp}
import shine.OpenCL.primitives.{functional => ocl}
import shine.cuda.primitives.{functional => cuda}
import shine.cuda.primitives.{imperative => cudaIm}

object ContinuationTranslation {
  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case n: Natural => C(n)

      case u@UnaryOp(op, e) =>
        con(e)(fun(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(fun(b.t)(x =>
          con(e2)(fun(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => primitive(ep)(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => con(Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(kind, fun, arg) => arg match {
        case a: Nat =>
          con(Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(nat)->:`[ExpType]]])(a))(C)
        case a: DataType =>
          con(Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(dt)->:`[ExpType]]])(a))(C)
      }

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(fun(cond.t) { x =>
          `if`(x) `then` con(thenP)(C) `else` con(elseP)(C)
        })

      case Proj1(_) | Proj2(_) => C(E)

      case LetNat(_, _, _) => throw new Exception("This should never happen")
    }
  }

  def primitive(E: ExpPrimitive)
               (C: Phrase[ExpType ->: CommType])
               (implicit context: TranslationContext): Phrase[CommType] = E match {
    case AsScalar(n, m, dt, access, array) =>
      con(array)(fun(array.t)(x =>
        C(AsScalar(n, m, dt, access, x))))

    case AsVector(n, m, dt, access, array) =>
      con(array)(fun(array.t)(x =>
        C(AsVector(n, m, dt, access, x))))

    case AsVectorAligned(n, m, w, dt, array) =>
      con(array)(fun(array.t)(x =>
        C(AsVectorAligned(n, m, w, dt, x)) ))

    case Cast(dt1, dt2, e) =>
      con(e)(fun(e.t)(x =>
        C(Cast(dt1, dt2, x))))

    case Cycle(n, m, dt, input) =>
      con(input)(fun(expT(m`.`dt, read))(x =>
        C(Cycle(n, m, dt, x))))

    case DepIdx(n, ft, index, array) =>
      con(array)(fun(expT(n`.d`ft, read))(e =>
        C(DepIdx(n, ft, index, e))))

    case DepJoin(n, lenF, dt, array) =>
      con(array)(fun(expT(n `.d` { i => lenF(i)`.`dt }, read))(x =>
        C(DepJoin(n, lenF, dt, x))))

    case depMapSeq: DepMapSeq =>
      val (n, _, ft2, _, _) = depMapSeq.unwrap
      `new`(n`.d`ft2, fun(varT(n`.d`ft2))(tmp =>
        acc(depMapSeq)(tmp.wr) `;` C(tmp.rd) ))

    case DepZip(n, ft1, ft2, e1, e2) =>
      con(e1)(fun(ExpType(DepArrayType(n, ft1), read))(x =>
        con(e2)(fun(ExpType(DepArrayType(n, ft2), read))(y =>
          C(DepZip(n, ft1, ft2, x, y)) )) ))

    case DMatch(x, elemT, outT, a, f, input) =>
      // Turn the f imperative by means of forwarding the continuation translation
      con(input)(fun(expT(DepPairType(NatKind, x, elemT), read))(pair =>
        DMatchI(x, elemT, outT,
          depFun(NatKind)((fst: NatIdentifier) =>
            fun(expT(substituteNatInType(fst, x, elemT), read))(snd =>
              con(f(fst)(snd))(C)
            )), pair)))

    case Drop(n, m, dt, array) =>
      con(array)(fun(expT((n + m)`.` dt, read))(x =>
        C(Drop(n, m, dt, x))))

    case ffc@ForeignFunctionCall(funDecl, n) =>
      def rec(ts: Seq[(Phrase[ExpType], DataType)],
              exps: Seq[Phrase[ExpType]],
              inTs: Seq[DataType]): Phrase[CommType] = {
        ts match {
          // with only one argument left to process return the assignment of the function call
          case Seq( (arg, inT) ) =>
            con(arg)(fun(expT(inT, read))(e =>
              ffc.outT match {
                // TODO: this is an ugly fix to avoid calling the function multiple times
                //  for pair assignment, see:
                // https://github.com/rise-lang/shine/issues/58
                case PairType(_, _) =>
                  val backendNew: (DataType, Phrase[VarType] => Phrase[CommType])
                    => Phrase[CommType] =
                    context match {
                      case _: shine.OpenCL.Compilation.TranslationContext =>
                        shine.OpenCL.DSL.`new`(AddressSpace.Private)
                      case _ =>
                        `new`.apply
                    }
                  backendNew(ffc.outT, tmp =>
                    Assign(ffc.outT, tmp.wr, ForeignFunctionCall(funDecl, n)( inTs :+ inT, ffc.outT, exps :+ e)) `;`
                      C(tmp.rd))
                case _ => C( ForeignFunctionCall(funDecl, n)(inTs :+ inT, ffc.outT, exps :+ e) )
              }))
          // with a `tail` of arguments left, rec
          case Seq( (arg, inT), tail@_* ) =>
            con(arg)(fun(expT(inT, read))(e =>
              rec(tail, exps :+ e, inTs :+ inT) ))
        }
      }

      rec(ffc.args zip ffc.inTs, Seq(), Seq())

    case Fst(dt1, dt2, pair) =>
      con(pair)(fun(expT(dt1 x dt2, read))(x =>
        C(Fst(dt1, dt2, x))))

    case Gather(n, m, dt, indices, input) =>
      con(indices)(fun(expT(m`.`idx(n), read))(y =>
        con(input)(fun(expT(n`.`dt, read))(x =>
          C(Gather(n, m, dt, y, x))))))

    case Generate(n, dt, f) =>
      // note: would not be necessary if generate was defined as indices + map
      C(GenerateCont(n, dt,
        fun(expT(idx(n), read))(i =>
          fun(expT(dt, read) ->: (comm: CommType))(cont =>
            con(f(i))(fun(expT(dt, read))(g => Apply(cont, g)))))))

    case Idx(n, dt, index, array) =>
      con(array)(fun(expT(n`.`dt, read))(e =>
        con(index)(fun(index.t)(i =>
          C(Idx(n, dt, i, e))))))

    case idx@shine.OpenCL.primitives.imperative.IdxDistribute(level) =>
      val (m, n, stride, dt, array) = idx.unwrap
      con(array)(fun(expT(m`.`dt, read))(e =>
        C(shine.OpenCL.primitives.imperative.IdxDistribute(level)(m, n, stride, dt, e))))

    case IdxVec(n, st, index, vector) =>
      con(vector)(fun(expT(vec(n, st), read))(e =>
        C(IdxVec(n, st, index, e))))

    case IndexAsNat(n, e) =>
      con(e)(fun(expT(idx(n), read))(x =>
        C(IndexAsNat(n, x))))

    case Join(n, m, w, dt, array) =>
      con(array)(fun(expT(n`.`(m`.`dt), read))(x =>
        C(Join(n, m, w, dt, x))))

    case Let(dt1, dt2, access, value, f) =>
      con(value)(fun(value.t)(x =>
        con(f(x))(C)))

    case ma@MakeArray(_) =>
      def rec(func: Seq[Phrase[ExpType]], imp: Seq[Phrase[ExpType]]): Phrase[CommType] = {
        func match {
          case xf +: func => con(xf)(fun(expT(ma.dt, read))(xi =>
            rec(func, imp :+ xi)
          ))
          case _ => C(MakeArray(ma.n)(ma.dt, imp))
        }
      }

      rec(ma.elements, Seq())

    case makeDepPair@MakeDepPair(a, fst, sndT, snd) =>
      // Allocate for the resulting dependent pair,
      // then imperatively write the first element,
      // acc-translate and write the second element
      // and call the continuation on the result
      // TODO(federico) - This is allocating eagerly. Make it allocate lazily by adding a suitable primitive:
      //  ideally Dmatch(..,..., MkDPair(x, y))
      // should not allocate
      `new`(makeDepPair.t.dataType, outVar => {
        MkDPairFstI(fst, outVar.wr) `;`
          acc(snd)(MkDPairSndAcc(fst, sndT, outVar.wr)) `;`
          C(outVar.rd)
      })

    case MakePair(dt1, dt2, access, fst, snd) =>
      con(fst)(fun(expT(dt1, read))(x =>
        con(snd)(fun(expT(dt2, read))(y =>
          C(MakePair(dt1, dt2, access, x, y))))))

    case Map(n, dt1, dt2, access, f, array) =>
      con(array)(fun(expT(n`.`dt1, read))(x =>
        C(MapRead(n, dt1, dt2,
          fun(expT(dt1, read))(a =>
            fun(expT(dt2, read) ->: (comm: CommType))(cont =>
              con(f(a))(fun(expT(dt2, read))(b => Apply(cont, b))))),
          x))))

    case MapFst(w, dt1, dt2, dt3, f, record) =>
      // assumption: f does not need to be translated, it does indexing only
      con(record)(fun(record.t)(x => C(MapFst(w, dt1, dt2, dt3, f, x))))

    case mapSeq: MapSeq =>
      val (n, _, dt2, _, _) = mapSeq.unwrap
      println("WARNING: map loop continuation translation allocates memory")
      // TODO should be removed
      `new`(n`.`dt2, fun(varT(n`.`dt2))(tmp =>
        acc(mapSeq)(tmp.wr) `;` C(tmp.rd) ))

    case MapSnd(w, dt1, dt2, dt3, f, record) =>
      // assumption: f does not need to be translated, it does indexing only
      con(record)(fun(record.t)(x => C(MapSnd(w, dt1, dt2, dt3, f, x))))

    case mapVec@MapVec(n, dt1, dt2, f, array) =>
      println("WARNING: map loop continuation translation allocates memory")
      // TODO should be removed
      `new`(vec(n, dt2),
        fun(varT(vec(n, dt2)))(tmp =>
          acc(mapVec)(tmp.wr) `;`
            C(tmp.rd)))

    case NatAsIndex(n, e) =>
      con(e)(fun(e.t)(x =>
        C(NatAsIndex(n, x))))

    case PadCst(n, l, r, dt, padExp, array) =>
      con(array)(fun(expT(n`.`dt, read))(x =>
        con(padExp)(fun(expT(dt, read))(p =>
          C(PadCst(n, l, r, dt, p, x))))))

    case PadClamp(n, l, r, dt, array) =>
      con(array)(fun(expT(n`.`dt, read))(x =>
        C(PadClamp(n, l, r, dt, x))))

    case Partition(n, m, lenF, dt, array) =>
      con(array)(fun(expT(n`.`dt, read))(x =>
        C(Partition(n, m, lenF, dt, x))))

    case PrintType(msg, dt, access, input) =>
      con(input)(C)

    case reduceSeq@ReduceSeq(unroll) =>
      val (n, dt1, dt2, f, init, array) = reduceSeq.unwrap
      con(array)(fun(expT(n`.`dt1, read))(X => {
        comment("reduceSeq") `;`
          `new`(dt2, accum =>
            acc(init)(accum.wr) `;`
              `for`(unroll, n, i =>
                              acc(f(accum.rd)(X `@` i))(accum.wr)) `;`
              C(accum.rd))
      }))

    case Reorder(n, dt, access, idxF, idxFinv, input) =>
      con(input)(fun(expT(n`.`dt, read))(x =>
        C(Reorder(n, dt, access, idxF, idxFinv, x))))

    case scanSeq@ScanSeq(n, dt1, dt2, f, init, array) =>
      `new`(n`.`dt2, fun(varT(n`.`dt2))(tmp =>
        acc(scanSeq)(tmp.wr) `;` C(tmp.rd) ))

    case slide@Slide(n, sz, sp, dt, input) =>
      val inputSize = sp*n+sz
      con(input)(fun(expT(inputSize`.`dt, read))(x =>
        C(Slide(n, sz, sp, dt, x)) ))

    case Snd(dt1, dt2, pair) =>
      con(pair)(fun(expT(dt1 x dt2, read))(x =>
        C(Snd(dt1, dt2, x))))

    case Split(n, m, w, dt, array) =>
      con(array)(fun(expT((m * n)`.`dt, read))(x =>
        C(Split(n, m, w, dt, x))))

    case Take(n, m, dt, array) =>
      con(array)(fun(expT((n + m)`.`dt, read))(x =>
        C(Take(n, m, dt, x))))

    case ToMem(dt, input) =>
      `new`(dt, tmp => acc(input)(tmp.wr) `;` C(tmp.rd))

    case Transpose(n, m, dt, access, array) =>
      con(array)(fun(array.t)(x =>
        C(Transpose(n, m, dt, access, x))))

    case TransposeDepArray(n, m, f, array) =>
      con(array)(fun(expT(n`.`(m`.d`f), read))(x =>
        C(TransposeDepArray(n, m, f, x))))

    case Unzip(n, dt1, dt2, access, e) =>
      con(e)(fun(expT(n`.`(dt1 x dt2), read))(x =>
        C(Unzip(n, dt1, dt2, access, x))))

    case VectorFromScalar(n, dt, arg) =>
      con(arg)(fun(expT(dt, read))(e =>
        C(VectorFromScalar(n, dt, e)) ))

    case Zip(n, dt1, dt2, access, e1, e2) =>
      con(e1)(fun(expT(n`.`dt1, read))(x =>
        con(e2)(fun(expT(n`.`dt2, read))(y =>
          C(Zip(n, dt1, dt2, access, x, y)) )) ))

    // OpenMP
    case depMapPar@omp.DepMapPar(n, ft1, ft2, f, array) =>
      `new`(n`.d`ft2, fun(varT(n`.d`ft2))(tmp =>
        acc(depMapPar)(tmp.wr) `;` C(tmp.rd) ))

    case mapPar@omp.MapPar(n, dt1, dt2, f, array) =>
      println("WARNING: map loop continuation translation allocates memory")
      // TODO should be removed
      `new`(n`.`dt2, fun(varT(n`.`dt2))(tmp =>
        acc(mapPar)(tmp.wr) `;` C(tmp.rd) ))

    case omp.ReducePar(n, dt1, dt2, f, init, array) =>
      con(array)(fun(expT(n`.`dt1, read))(X =>
        con(init)(fun(expT(dt2, read))(Y =>
          ???
        ))))

    // OpenCL
    case depMap: ocl.DepMap =>
      val (n, _, ft2, _, _) = depMap.unwrap
      `new`(n`.d`ft2, fun(varT(n`.d`ft2))(tmp =>
        acc(depMap)(tmp.wr) `;` C(tmp.rd) ))

    case map@ocl.Map(level, dim) =>
      println("WARNING: map loop continuation translation allocates memory")
      // TODO should be removed
      `new`(map.n `.` map.dt2, fun(varT(map.n `.` map.dt2))(tmp =>
        acc(map)(tmp.wr) `;` C(tmp.rd)))

    case fc@ocl.OpenCLFunctionCall(name, n) =>
      def rec(ts: Seq[(Phrase[ExpType], DataType)],
              es: Seq[Phrase[ExpType]],
              inTs: Seq[DataType]): Phrase[CommType] = {
        ts match {
          // with only one argument left to process continue with the OpenCLFunction call
          case Seq( (arg, inT) ) =>
            con(arg)(fun(expT(inT, read))(e =>
              C(ocl.OpenCLFunctionCall(name, n)(inTs :+ inT, fc.outT, es :+ e)) ))
          // with a `tail` of arguments left, rec
          case Seq( (arg, inT), tail@_* ) =>
            con(arg)(fun(expT(inT, read))(e => rec(tail, es :+ e, inTs :+ inT) ))
        }
      }

      rec(fc.args zip fc.inTs, Seq(), Seq())

    case reduceSeq@ocl.ReduceSeq(unroll) =>
      val (n, a, dt1, dt2, f, init, array) = reduceSeq.unwrap

      con(array)(fun(expT(n`.`dt1, read))(X => {
        val adj = AdjustArraySizesForAllocations(init, dt2, a)

        comment("oclReduceSeq") `;`
        (shine.OpenCL.DSL.`new` (a) (adj.dt, accumulator =>
          acc(init)(adj.accF(accumulator.wr)) `;`
            `for`(unroll, n, i =>
              acc( f(adj.exprF(accumulator.rd))(X `@` i) )(adj.accF(accumulator.wr)) ) `;`
            C(adj.exprF(accumulator.rd))
        ))
      }))

    case ocl.ToMem(addrSpace, dt, input) =>
      val adj = AdjustArraySizesForAllocations(input, dt, addrSpace)

      shine.OpenCL.DSL.`new` (addrSpace) (adj.dt, tmp =>
        acc(input)(adj.accF(tmp.wr)) `;` C(adj.exprF(tmp.rd)))

    // CUDA
    case cuda.GlobalToShared(dt, inputGlobal) =>
      val adj = AdjustArraySizesForAllocations(inputGlobal, dt, AddressSpace.Local)

      shine.OpenCL.DSL.`new` (AddressSpace.Private) (OpaqueType("pipeline"), pipeline =>
        shine.OpenCL.DSL.`new` (AddressSpace.Local) (adj.dt, tmp =>
          acc(inputGlobal)(cudaIm.GlobalToSharedAcc(dt, pipeline.rd, tmp.wr)) `;`
            cudaIm.SyncPipeline(pipeline.rd) `;`
          C(adj.exprF(tmp.rd))))

    case map@cuda.Map(level, dim) =>
      val (n, dt1, dt2, f, array) = (map.n, map.dt1, map.dt2, map.f, map.array)

      println("WARNING: map loop continuation translation allocates memory")
      // TODO should be removed
      `new`(n `.` dt2, fun(varT(n `.` dt2))(tmp =>
        acc(map)(tmp.wr) `;` C(tmp.rd)))

    case m@cuda.MapFragment(rows, columns, layers, dt, frag, layout, f, input) =>
      val fragType = FragmentType(rows, columns, layers, dt, frag, layout)
      shine.OpenCL.primitives.imperative.New(AddressSpace.Private, fragType,
        fun(VarType(fragType))(fragmentAcc =>
          (if (input.t.accessType.toString == write.toString)
            acc(input)(fragmentAcc.wr) `;`
              cudaIm.ForFragment(rows, columns, layers, dt, frag, layout, fragmentAcc.rd, fragmentAcc.wr,
                fun(expT(dt, read))(x =>
                  fun(accT(dt))(o =>
                    acc(f(x))(o))))
          else
            acc(m)(fragmentAcc.wr)) `;`
            C(fragmentAcc.rd)))
  }
}
