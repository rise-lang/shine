package shine.DPIA.Compilation

import arithexpr.arithmetic.{NamedVar, RangeAdd}
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL.{comment, _}
import shine.DPIA.Phrases._
import rise.core.types.{DataType, Fragment, MatrixLayout, NatIdentifier, NatKind, read, write}
import rise.core.DSL.Type._
import rise.core.types.DataType._
import rise.core.substitute.{natInType => substituteNatInType}
import shine.DPIA.Types.{AccType, CommType, ExpType, TypeCheck, comm}
import rise.core.types.DataTypeOps._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{Seq => _, _}
import shine.OpenMP.primitives.{functional => omp}
import shine.OpenCL.primitives.{functional => ocl}
import shine.OpenCL.primitives.{imperative => oclImp}
import shine.cuda.primitives.{functional => cuda}
import shine.cuda.primitives.{imperative => cudaImp}

object AcceptorTranslation {
  def acc(E: Phrase[ExpType])
         (A: Phrase[AccType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    E match {
      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun).reducing(arg))(A)
      case DepApply(kind, fun, arg) => arg match {
        case a: Nat =>
          acc(Lifting.liftDependentFunction(
            fun.asInstanceOf[ Phrase[`(nat)->:`[ExpType]]])(a))(A)
        case a: DataType =>
          acc(Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(dt)->:`[ExpType]]])(a))(A)
      }

      case e
        if TypeCheck.notContainingArrayType(e.t.dataType)
          && e.t.accessType == read =>
        //FIXME
        // The pattern matching is needed in order to generate separate
        // assignments to elements of pairs (structs), because the AMD SDK
        // cannot deal with literal struct assignments or definitions (C99).
        e match {
          case MakePair(dt1, dt2, _, fst, snd) =>
            acc(fst)(pairAcc1(dt1, dt2, A)) `;`
              acc(snd)(pairAcc2(dt1, dt2, A))
          case _ =>
            con(e)(fun(e.t)(a => A :=| e.t.dataType | a))
        }

      case c: Literal => A :=|c.t.dataType| c

      case x: Identifier[ExpType] => A :=|x.t.dataType| x

      case n: Natural => A :=|n.t.dataType| n

      case u@UnaryOp(op, e) =>
        con(e)(fun(u.t)(x =>
          A :=|u.t.dataType| UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(fun(b.t)(x =>
          con(e2)(fun(b.t)(y =>
            A :=|b.t.dataType| BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => primitive(ep)(A)

      case LetNat(binder, defn, body) => LetNat(binder, defn, acc(body)(A))

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(fun(cond.t) { x =>
          `if` (x) `then` acc(thenP)(A) `else` acc(elseP)(A)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def primitive(E: ExpPrimitive)
               (A: Phrase[AccType])
               (implicit context: TranslationContext): Phrase[CommType] = E match {
    case AsScalar(n, m, dt, access, array) =>
      acc(array)(AsScalarAcc(n, m, dt, A))

    case AsVector(n, m, dt, access, array) =>
      acc(array)(AsVectorAcc(n, m, dt, A))

    case AsVectorAligned(n, m, dt, access, array) =>
      acc(array)(AsVectorAcc(n, m, dt, A))

    case DepIdx(n, ft, index, array) =>
      con(array)(fun(expT(n`.d`ft, read))(x =>
        A :=| ft(index) | DepIdx(n, ft, index, x)))

    case DepJoin(n, lenF, dt, array) =>
      acc(array)(DepJoinAcc(n, lenF, dt, A))

    case depMapSeq@DepMapSeq(unroll) =>
      val (n, ft1, ft2, f, array) = depMapSeq.unwrap
      con(array)(fun(expT(n`.d`ft1, read))(x =>
        forNat(unroll, n, i =>
          acc(f(i)(x `@d` i))(A `@d` i))
      ))

    case DepTile(n, tileSize, haloSize, dt1, dt2, processTiles, array) =>
      ???

    case DMatch(x, elemT, outT, a, f, input) =>
      // Turn the f imperative by means of forwarding the acceptor translation
      con(input)(fun(expT(DepPairType(NatKind, x, elemT), read))(pair =>
        DMatchI(x, elemT, outT,
          depFun(NatKind)((fst: NatIdentifier) =>
            fun(expT(substituteNatInType(fst, x, elemT), read))(snd =>
              acc(f(fst)(snd))(A)
            )), pair)))

    case IdxVec(n, st, index, vector) =>
      con(vector)(fun(expT(vec(n, st), read))(x =>
        A :=| st | IdxVec(n, st, index, x)))

    case Iterate(n, m, k, dt, f, array) =>
      con(array)(fun(expT((m * n.pow(k))`.`dt, read))(x => {
        val sz = n.pow(k) * m

        newDoubleBuffer(sz`.`dt, m`.`dt, sz`.`dt, x, A,
          (v: Phrase[VarType],
           swap: Phrase[CommType],
           done: Phrase[CommType]) => {
            `for`(k, ip => {
              val i = NamedVar(ip.name)

              val isz = n.pow(k - i) * m
              val osz = n.pow(k - i - 1) * m
              acc(f(osz)(Take(isz, sz - isz, dt, v.rd)))(TakeAcc(osz, sz - osz, dt, v.wr)) `;`
                IfThenElse(ip < NatAsIndex(k, Natural(k - 2)), swap, done)
            })
          })
      }))

    case IterateStream(n, dt1, dt2, f, array) =>
      val fI = fun(expT(dt1, read))(x => fun(accT(dt2))(o => acc(f(x))(o)))
      val i = NatIdentifier(freshName("i"))
      str(array)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(next =>
        comment("iterateStream") `;`
          forNat(n, i =>
            streamNext(next, i, fun(expT(dt1, read))(x => fI(x)(A `@` i))))
      ))

    case Join(n, m, w, dt, array) =>
      acc(array)(JoinAcc(n, m, dt, A))

    case Let(dt1, dt2, access, value, f) =>
      con(value)(fun(value.t)(x =>
        acc(f(x))(A)))

    case MakeDepPair(a, fst, sndT, snd) =>
      // We have the acceptor already, so simply write the first element and then
      // the second element in sequentially
      MkDPairFstI(fst, A) `;`
        acc(snd)(MkDPairSndAcc(fst, sndT, A))

    case MakePair(dt1, dt2, access, fst, snd) =>
      acc(fst)(pairAcc1(dt1, dt2, A)) `;`
        acc(snd)(pairAcc2(dt1, dt2, A))

    case Map(n, dt1, dt2, access, f, array) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt1, write))

      val otype = AccType(dt2)
      val o = Identifier(freshName("fede_o"), otype)

      acc(array)(MapAcc(n, dt2, dt1,
        Lambda(o, fedAcc(scala.Predef.Map((x, o)))(f(x))(fun(otype)(x => x))),
        A))

    case MapFst(w, dt1, dt2, dt3, f, record) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt1, write))

      val otype = AccType(dt3)
      val o = Identifier(freshName("fede_o"), otype)

      acc(record)(MapFstAcc(dt1, dt2, dt3,
        Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
        A))

    case mapSeq@MapSeq(unroll) =>
      val (n, dt1, dt2, f, array) = mapSeq.unwrap
      con(array)(fun(expT(n`.`dt1, read))(x =>
        comment("mapSeq")`;`
          `for`(unroll, n, i => acc(f(x `@` i))(A `@` i))
      ))

    case MapSnd(w, dt1, dt2, dt3, f, record) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt2, write))

      val otype = AccType(dt3)
      val o = Identifier(freshName("fede_o"), otype)

      acc(record)(MapSndAcc(dt1, dt2, dt3,
        Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
        A))

    case MapVec(n, dt1, dt2, f, array) =>
      con(array)(fun(expT(vec(n, dt1), read))(x =>
        shine.OpenMP.DSL.parForVec(n, dt2, A, i => a => acc(f(x `@v` i))(a))
      ))

    case PadEmpty(n, r, dt, array) =>
      acc(array)(TakeAcc(n, r, dt, A))

    case PrintType(msg, dt, access, input) =>
      acc(input)(A)

    case reduceSeq@ReduceSeq(unroll) =>
      val (n, dt1, dt2, f, init, array) = reduceSeq.unwrap
      con(reduceSeq)(fun(expT(dt2, write))(r =>
        acc(r)(A)))

    case Reorder(n, dt, access, idxF, idxFinv, input) =>
      acc(input)(ReorderAcc(n, dt, idxFinv, A))

    case ScanSeq(n, dt1, dt2, f, init, array) =>
      con(array)(fun(expT(n`.`dt1, read))(x =>
        con(init)(fun(expT(dt2, read))(y =>
          comment("scanSeq")`;`
          `new`(dt2, accumulator =>
            acc(y)(accumulator.wr) `;`
            `for`(n, i =>
              acc(f(x `@` i)(accumulator.rd))(accumulator.wr) `;`
              //FIXME remove general assignment
              ((A `@` i) :=| dt2 | accumulator.rd) ))))))

    case Scatter(n, m, dt, indices, input) =>
      con(indices)(fun(expT(n`.`idx(m), read))(y =>
        acc(input)(ScatterAcc(n, m, dt, y, A))))

    case slide@Slide(n, sz, sp, dt, input) =>
      con(slide)(fun(expT(n`.`(sz`.`dt), read))(x =>
        A :=|(n`.`(sz`.`dt))| x ))

    case Split(n, m, w, dt, array) =>
      acc(array)(SplitAcc(n, m, dt, A))

    case Transpose(n, m, dt, access, array) =>
      acc(array)(TransposeAcc(n, m, dt, A))

    case Unzip(n, dt1, dt2, access, e) =>
      acc(e)(UnzipAcc(n, dt1, dt2, A))

    case VectorFromScalar(n, dt, arg) =>
      con(arg)(fun(expT(dt, read))(e =>
        A :=|vec(n, dt)| VectorFromScalar(n, dt, e)))

    case Zip(n, dt1, dt2, access, e1, e2) =>
      acc(e1)(ZipAcc1(n, dt1, dt2, A)) `;`
        acc(e2)(ZipAcc2(n, dt1, dt2, A))

    // OpenMP
    case omp.DepMapPar(n, ft1, ft2, f, array) =>
      con(array)(fun(expT(n`.d`ft1, read))(x => {
        shine.OpenMP.DSL.parForNat(n, ft2, A, idx => a => acc(f(idx)(x `@d` idx))(a))
      }))

    case omp.MapPar(n, dt1, dt2, f, array) =>
      con(array)(fun(expT(n`.`dt1, read))(x =>
        shine.OpenMP.DSL.parFor(n, dt2, A, i => a => acc(f(x `@` i))(a))))

    case reducePar@omp.ReducePar(n, dt1, dt2, f, init, array) =>
      con(reducePar)(fun(expT(dt2, write))(r =>
        acc(r)(A)))

    // OpenCL
    case depMap@ocl.DepMap(level, dim) =>
      val (n, ft1, ft2, f, array) = depMap.unwrap
      con(array)(fun(expT(n`.d`ft1, read))(x => {
        import shine.OpenCL.DSL._
        level match {
          case shine.OpenCL.Global =>
            parForNatGlobal(dim)(n, ft2, A, idx => a => acc(f(idx)(x `@d` idx))(a))
          case shine.OpenCL.Local =>
            parForNatLocal(dim)(n, ft2, A, idx => a => acc(f(idx)(x `@d` idx))(a)) `;` barrier()
          case shine.OpenCL.WorkGroup =>
            parForNatWorkGroup(dim)(n, ft2, A, idx => a => acc(f(idx)(x `@d` idx))(a))
          case shine.OpenCL.Sequential | shine.OpenCL.Warp | shine.OpenCL.Lane =>
            throw new Exception("This should not happen")
        }}))

    case ocl.Iterate(a, n, m, k, dt, f, array) =>
      con(array)(fun(expT({m * n.pow(k)}`.`dt, read))(x => {
        import arithexpr.arithmetic.Cst
        val sz = n.pow(k) * m

        shine.OpenCL.DSL.newDoubleBuffer(a, sz`.`dt, m`.`dt, sz`.`dt, x, A,
          (v, swap, done) => {
            shine.DPIA.DSL.`for`(k, ip => {
              val i = NamedVar(ip.name, RangeAdd(Cst(0), k, Cst(1)))

              val isz = n.pow(k - i) * m
              val osz = n.pow(k - i - 1) * m
              acc(f(osz)(Take(isz, sz - isz, dt, v.rd)))(TakeAcc(osz, sz - osz, dt, v.wr)) `;`
                IfThenElse(ip < NatAsIndex(k, Natural(k - 2)), swap, done)
            })
          })
      }))

    case kc@ocl.KernelCall(name, localSize, globalSize, n) =>
      def rec(ts: Seq[Phrase[ExpType]],
              es: Seq[Phrase[ExpType]]): Phrase[CommType] = {
        ts match {
          case Nil =>
            oclImp.KernelCallCmd(name, localSize, globalSize, n)(kc.inTs, kc.outT, kc.args, A)
          case Seq(arg, tail@_*) =>
            con(arg)(fun(expT(arg.t.dataType, read))(e => rec(tail, es :+ e)))
        }
      }

      rec(kc.args, Seq())

    case map@ocl.Map(level, dim) =>
      val (n, dt1, dt2, f, array) = map.unwrap
      con(array)(fun(expT(n `.` dt1, read))(x => {
        comment(s"map${level.toString}") `;`
          shine.OpenCL.DSL.parFor(level, dim, unroll = false)(n, dt2, A,
            fun(expT(idx(n), read))(i => fun(accT(dt2))(a => acc(f(x `@` i))(a))))
      }))

    case fc@ocl.OpenCLFunctionCall(name, n) =>
      def rec(ts: Seq[(Phrase[ExpType], DataType)],
                  exps: Seq[Phrase[ExpType]],
                  inTs: Seq[DataType]): Phrase[CommType] = {
        ts match {
          // with only one argument left to process return the assignment of the OpenCLFunction call
          case Seq( (arg, inT) ) =>
            con(arg)(fun(expT(inT, read))(e =>
              A :=|fc.outT| ocl.OpenCLFunctionCall(name, n)(inTs :+ inT, fc.outT, exps :+ e) ))
          // with a `tail` of arguments left, recurse
          case Seq( (arg, inT), tail@_* ) =>
            con(arg)(fun(expT(inT, read))(e => rec(tail, exps :+ e, inTs :+ inT) ))
        }
      }

      rec(fc.args zip fc.inTs, Seq(), Seq())

    // CUDA
    case cuda.AsFragment(rows, columns, layers, dataType, fragmentKind, layout, matrix) =>
      con(matrix)(fun(ExpType(ArrayType(rows, ArrayType(columns, dataType)), read))(matrix =>
        cudaImp.WmmaLoad(rows, columns, layers, dataType, fragmentKind, layout, matrix, A)))

    case cuda.AsMatrix(rows, columns, layers, dataType, fragment) =>
      con(fragment)(fun(ExpType(fragment.t.dataType, read))(fragment =>
        cudaImp.WmmaStore(rows, columns, layers, dataType, fragment, A)))

    case cuda.GenerateFragment(rows, columns, layers, dataType, frag, layout, fill) =>
      con(fill)(fun(ExpType(dataType, read))(fill =>
        cudaImp.WmmaFill(rows, columns, layers, dataType, frag, layout, fill, A)))

    case map@cuda.Map(level, dim) =>
      val (n, dt1, dt2, f, array) = map.unwrap
      con(array)(fun(expT(n `.` dt1, read))(x => {
        val forLoop = comment(s"map${level.toString}") `;`
          shine.cuda.DSL.parFor(level, dim, unroll = false)(n, dt2, A,
            fun(expT(idx(n), read))(i => fun(accT(dt2))(a =>
              acc(f(x `@` i))(a))))
        //TODO use other InsertMemoryBarrieres-mechanism
        level match {
          case shine.OpenCL.Local => forLoop `;` cudaImp.SyncThreads()
          case shine.OpenCL.Warp => forLoop `;` cudaImp.SyncThreads()
          case shine.OpenCL.Lane => forLoop `;` cudaImp.SyncWarp()
          case _ => forLoop
        }
      }))

    case cuda.MapFragment(rows, columns, layers, dt, frag, layout, f, input) =>
      con(input)(fun(expT(FragmentType(rows, columns, layers, dt, frag, layout), read))(input =>
        shine.cuda.primitives.imperative.ForFragment(rows, columns, layers, dt, frag, layout, input, A,
          fun(expT(dt, read))(x =>
            fun(accT(dt))(o =>
              acc(f(x))(o))))))

    case cuda.TensorMatMultAdd(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix) =>
      con(aMatrix)(fun(ExpType(FragmentType(m, n, k, dataType, Fragment.AMatrix, layoutA), read))(aMatrix =>
        con(bMatrix)(fun(ExpType(FragmentType(m, n, k, dataType, Fragment.BMatrix, layoutB), read))(bMatrix =>
          con(cMatrix)(fun(ExpType(FragmentType(m, n, k, dataTypeAcc, Fragment.Accumulator, MatrixLayout.None), read))(cMatrix =>
            cudaImp.WmmaMMA(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix, A)))))))

    //GAP8
    case r@shine.GAP8.primitives.functional.Run(cores) => {
      ???
    }


    case kc@shine.GAP8.primitives.functional.KernelCall(name, cores, n) =>
      def rec(ts: Seq[Phrase[ExpType]], es: Seq[Phrase[ExpType]]): Phrase[CommType] = ts match {
        case Nil =>
          shine.GAP8.primitives.imperative.KernelCallCmd(name, cores, n)(kc.inTs, kc.outT, kc.args, A)
        case Seq(arg, tail@_*) =>
          con(arg)(fun(expT(arg.t.dataType, read))(e => rec(tail, es :+ e)))
      }

      rec(kc.args, Seq())
  }
}
