package shine.DPIA.Compilation

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{Seq => _, _}
import shine.DPIA.primitives.intermediate._
import shine.OpenMP.primitives.{functional => omp}
import shine.OpenMP.primitives.{intermediate => ompI}
import shine.OpenCL.primitives.{functional => ocl}
import shine.OpenCL.primitives.{intermediate => oclI}
import shine.OpenCL.primitives.{imperative => oclImp}
import shine.cuda.primitives.{functional => cuda}
import shine.cuda.primitives.{intermediate => cudaI}
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
            con(e)(λ(e.t)(a => A :=| e.t.dataType | a))
        }

      case c: Literal => A :=|c.t.dataType| c

      case x: Identifier[ExpType] => A :=|x.t.dataType| x

      case n: Natural => A :=|n.t.dataType| n

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          A :=|u.t.dataType| UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            A :=|b.t.dataType| BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => primitive(ep)(A)

      case LetNat(binder, defn, body) => LetNat(binder, defn, acc(body)(A))

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
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
      con(array)(λ(expT(n`.d`ft, read))(x =>
        A :=| ft(index) | DepIdx(n, ft, index, x)))

    case DepJoin(n, lenF, dt, array) =>
      acc(array)(DepJoinAcc(n, lenF, dt, A))

    case depMapSeq@DepMapSeq(unroll) =>
      val (n, ft1, ft2, f, array) = depMapSeq.unwrap
      con(array)(λ(expT(n`.d`ft1, read))(x =>
        DepMapSeqI(unroll)(n, ft1, ft2, _Λ_(NatKind)((k: NatIdentifier) =>
          λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
            acc(f(k)(x))(o)
          }))), x, A)))

    case DepTile(n, tileSize, haloSize, dt1, dt2, processTiles, array) =>
      ???

    case DMatch(x, elemT, outT, a, f, input) =>
      // Turn the f imperative by means of forwarding the acceptor translation
      con(input)(λ(expT(DepPairType(x, elemT), read))(pair =>
        DMatchI(x, elemT, outT,
          _Λ_(NatKind)((fst: NatIdentifier) =>
            λ(expT(DataType.substitute(fst, x, elemT), read))(snd =>
              acc(f(fst)(snd))(A)
            )), pair)))

    case IdxVec(n, st, index, vector) =>
      con(vector)(λ(expT(vec(n, st), read))(x =>
        A :=| st | IdxVec(n, st, index, x)))

    case Iterate(n, m, k, dt, f, array) =>
      con(array)(λ(expT((m * n.pow(k))`.`dt, read))(x =>
        IterateIAcc(n, m, k, dt, A,
          _Λ_(NatKind)(l => λ(accT(l `.` dt))(o =>
            λ(expT((l * n)`.`dt, read))(x => acc(f(l)(x))(o)))),
          x)))

    case IterateStream(n, dt1, dt2, f, array) =>
      val fI = λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o)))
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
        Lambda(o, fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
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
      con(array)(λ(expT(n`.`dt1, read))(x =>
        MapSeqI(unroll)(n, dt1, dt2,
          fun(expT(dt1, read))(x =>
            fun(accT(dt2))(o =>
              acc(f(x))(o))),
          x, A)))

    case MapSnd(w, dt1, dt2, dt3, f, record) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt2, write))

      val otype = AccType(dt3)
      val o = Identifier(freshName("fede_o"), otype)

      acc(record)(MapSndAcc(dt1, dt2, dt3,
        Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
        A))

    case MapVec(n, dt1, dt2, f, array) =>
      con(array)(λ(expT(vec(n, dt1), read))(x =>
        MapVecI(n, dt1, dt2, λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))), x, A)))

    case PadEmpty(n, r, dt, array) =>
      acc(array)(TakeAcc(n, r, dt, A))

    case PrintType(msg, dt, access, input) =>
      acc(input)(A)

    case reduceSeq@ReduceSeq(unroll) =>
      val (n, dt1, dt2, f, init, array) = reduceSeq.unwrap
      con(reduceSeq)(λ(expT(dt2, write))(r =>
        acc(r)(A)))

    case Reorder(n, dt, access, idxF, idxFinv, input) =>
      acc(input)(ReorderAcc(n, dt, idxFinv, A))

    case scan@ScanSeq(unroll) =>
      val (n, dt1, dt2, f, init, array) = scan.unwrap
      con(array)(λ(expT(n`.`dt1, read))(x =>
        con(init)(λ(expT(dt2, read))(y =>
          ScanSeqI(unroll)(n, dt1, dt2,
            λ(expT(dt1, read))(x => λ(expT(dt2, read))(y => λ(accT(dt2))(o =>
              acc(f(x)(y))(o)))),
            y, x, A)))))


    case Scatter(n, m, dt, indices, input) =>
      con(indices)(fun(expT(n`.`idx(m), read))(y =>
        acc(input)(ScatterAcc(n, m, dt, y, A))))

    case slide@Slide(n, sz, sp, dt, input) =>
      con(slide)(λ(expT(n`.`(sz`.`dt), read))(x =>
        A :=|(n`.`(sz`.`dt))| x ))

    case Split(n, m, w, dt, array) =>
      acc(array)(SplitAcc(n, m, dt, A))

    case Transpose(n, m, dt, access, array) =>
      acc(array)(TransposeAcc(n, m, dt, A))

    case Unzip(n, dt1, dt2, access, e) =>
      acc(e)(UnzipAcc(n, dt1, dt2, A))

    case VectorFromScalar(n, dt, arg) =>
      con(arg)(λ(expT(dt, read))(e =>
        A :=|VectorType(n, dt)| VectorFromScalar(n, dt, e)))

    case Zip(n, dt1, dt2, access, e1, e2) =>
      acc(e1)(ZipAcc1(n, dt1, dt2, A)) `;`
        acc(e2)(ZipAcc2(n, dt1, dt2, A))

    // OpenMP
    case omp.DepMapPar(n, ft1, ft2, f, array) =>
      con(array)(λ(expT(n`.d`ft1, read))(x =>
        ompI.DepMapParI(n, ft1, ft2, _Λ_(NatKind)((k: NatIdentifier) =>
          λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
            acc(f(k)(x))(o)
          }))), x, A)))

    case omp.MapPar(n, dt1, dt2, f, array) =>
      con(array)(λ(expT(n`.`dt1, read))(x =>
        ompI.MapParI(n, dt1, dt2,
          λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))),
          x, A)))

    case reducePar@omp.ReducePar(n, dt1, dt2, f, init, array) =>
      con(reducePar)(λ(expT(dt2, write))(r =>
        acc(r)(A)))

    // OpenCL
    case depMap@ocl.DepMap(level, dim) =>
      val (n, ft1, ft2, f, array) = depMap.unwrap
      con(array)(λ(expT(n`.d`ft1, read))(x =>
        oclI.DepMapI(level, dim)(n, ft1, ft2, _Λ_(NatKind)((k: NatIdentifier) =>
          λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
            acc(f(k)(x))(o)
          }))), x, A)))

    case ocl.Iterate(a, n, m, k, dt, f, array) =>
      con(array)(λ(expT({m * n.pow(k)}`.`dt, read))(x =>
        oclI.IterateIAcc(a, n, m, k, dt, A,
          _Λ_(NatKind)(l => λ(accT(l`.`dt))(o =>
            λ(expT({l * n}`.`dt, read))(x => acc(f(l)(x))(o)))),
          x)))

    case kc@ocl.KernelCall(name, localSize, globalSize, n) =>
      def rec(ts: Seq[Phrase[ExpType]],
              es: Seq[Phrase[ExpType]]): Phrase[CommType] = {
        ts match {
          case Nil =>
            oclImp.KernelCallCmd(name, localSize, globalSize, n)(kc.inTs, kc.outT, kc.args, A)
          case Seq(arg, tail@_*) =>
            con(arg)(λ(expT(arg.t.dataType, read))(e => rec(tail, es :+ e)))
        }
      }

      rec(kc.args, Seq())

    case map@ocl.Map(level, dim) =>
      val (n, dt1, dt2, f, array) = map.unwrap
      con(array)(λ(expT(n `.` dt1, read))(x =>
        oclI.MapI(level, dim)(n, dt1, dt2,
          λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))),
          x, A)))

    case fc@ocl.OpenCLFunctionCall(name, n) =>
      def rec(ts: Seq[(Phrase[ExpType], DataType)],
                  exps: Seq[Phrase[ExpType]],
                  inTs: Seq[DataType]): Phrase[CommType] = {
        ts match {
          // with only one argument left to process return the assignment of the OpenCLFunction call
          case Seq( (arg, inT) ) =>
            con(arg)(λ(expT(inT, read))(e =>
              A :=|fc.outT| ocl.OpenCLFunctionCall(name, n)(inTs :+ inT, fc.outT, exps :+ e) ))
          // with a `tail` of arguments left, recurse
          case Seq( (arg, inT), tail@_* ) =>
            con(arg)(λ(expT(inT, read))(e => rec(tail, exps :+ e, inTs :+ inT) ))
        }
      }

      rec(fc.args zip fc.inTs, Seq(), Seq())

    case scan@ocl.ScanSeq(unroll) =>
      val (n, a, dt1, dt2, f, init, array) = scan.unwrap
      con(array)(λ(expT(n`.`dt1, read))(x =>
        con(init)(λ(expT(dt2, read))(y =>
          oclI.ScanSeqI(unroll)(n, a, dt1, dt2,
            λ(expT(dt1, read))(x => λ(expT(dt2, read))(y => λ(accT(dt2))(o =>
              acc(f(x)(y))(o)))),
            y, x, A)))))

    case ocl.ScanSeqInclusive(n, a, dt1, dt2, f, init, array) =>
      con(array)(λ(expT(n`.`dt1, read))(x =>
        con(init)(λ(expT(dt2, read))(y =>
          oclI.ScanSeqInclusiveI(n, a, dt1, dt2,
            λ(expT(dt1, read))(x => λ(expT(dt2, read))(y => λ(accT(dt2))(o =>
              acc(f(x)(y))(o)))),
            y, x, A)))))

    // CUDA
    case cuda.AsFragment(rows, columns, layers, dataType, fragmentKind, layout, matrix) =>
      con(matrix)(λ(ExpType(ArrayType(rows, ArrayType(columns, dataType)), read))(matrix =>
        cudaImp.WmmaLoad(rows, columns, layers, dataType, fragmentKind, layout, matrix, A)))

    case cuda.AsMatrix(rows, columns, layers, dataType, fragment) =>
      con(fragment)(λ(ExpType(fragment.t.dataType, read))(fragment =>
        cudaImp.WmmaStore(rows, columns, layers, dataType, fragment, A)))

    case cuda.GenerateFragment(rows, columns, layers, dataType, frag, layout, fill) =>
      con(fill)(λ(ExpType(dataType, read))(fill =>
        cudaImp.WmmaFill(rows, columns, layers, dataType, frag, layout, fill, A)))

    case map@cuda.Map(level, dim) =>
      val (n, dt1, dt2, f, array) = map.unwrap
      con(array)(λ(expT(n `.` dt1, read))(x =>
        cudaI.MapI(level, dim)(n, dt1, dt2,
          λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))),
          x, A)))

    case cuda.MapFragment(rows, columns, layers, dt, frag, layout, fun, input) =>
      con(input)(λ(expT(FragmentType(rows, columns, layers, dt, frag, layout), read))(input =>
        shine.cuda.primitives.imperative.ForFragment(rows, columns, layers, dt, frag, layout, input, A,
          λ(expT(dt, read))(x =>
            λ(accT(dt))(o =>
              acc(fun(x))(o))))))

    case cuda.TensorMatMultAdd(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix) =>
      con(aMatrix)(λ(ExpType(FragmentType(m, n, k, dataType, FragmentKind.AMatrix, layoutA), read))(aMatrix =>
        con(bMatrix)(λ(ExpType(FragmentType(m, n, k, dataType, FragmentKind.BMatrix, layoutB), read))(bMatrix =>
          con(cMatrix)(λ(ExpType(FragmentType(m, n, k, dataTypeAcc, FragmentKind.Accumulator, MatrixLayout.None), read))(cMatrix =>
            cudaImp.WmmaMMA(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix, A)))))))
  }
}
