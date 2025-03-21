package shine.DPIA

import elevate.core.strategies.Traversable
import elevate.core.strategies.basic.normalize
import rise.core.types.{AddressSpaceKind, DataKind, DataType, NatKind, NatToNatKind, NatToNatLambda, TypeIdentifier, TypePlaceholder, read, write}
import rise.core.DSL.Type._
import rise.core.types.DataType._
import rise.elevate.Rise
import rise.elevate.rules._
import rise.core.{Opaque, TypeAnnotation, TypeAssertion, semantics => rs, types => rt}
import rise.{core => r}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.functional._
import util.monads

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable

object fromRise {
  def apply(expr: r.Expr)(implicit ev: Traversable[Rise]): Phrase[_ <: PhraseType] = {
    if (!r.IsClosedForm(expr)) {
      val (fV, fT) = r.IsClosedForm.varsToClose(expr)
      throw new Exception(s"expression is not in closed form: $expr\n\n with type ${expr.t}\n free vars: $fV\n free type vars: $fT\n\n")
    }
    val bnfExpr = normalize(ev).apply(betaReduction)(expr).get
    val nExpr = normalizeEqualNats(bnfExpr)
    val rwMap = inferAccess(nExpr)
    expression(nExpr, rwMap)
  }

  // NOTE: this is required because unify(nat1, nat2) can succeed while
  // normalize(nat1) != normalize(nat2)
  private def normalizeEqualNats(e: r.Expr): r.Expr = {
    // inspired from union find algorithm
    var map = mutable.Map[Nat, Nat]()

    @tailrec
    def getBest(n: Nat): Nat = {
      val n2 = map.getOrElse(n, n)
      if (n == n2) { return n }
      val n3 = map.getOrElse(n2, n2)
      map(n) = n3
      getBest(n3)
    }

    r.traverse.traverse(e, new r.traverse.PureTraversal {
      override def expr: r.Expr => util.monads.Pure[r.Expr] = { e =>
        e match {
          case r.App(f, arg) =>
            val ft = f.t.asInstanceOf[rt.FunType[_ <: rt.ExprType, _ <: rt.ExprType]]
            sameType(arg.t, ft.inT)
            sameType(e.t, ft.outT)
          case _ => ()
        }
        super.expr(e)
      }

      private def sameType(a: rt.ExprType, b: rt.ExprType): Unit = {
        def unwrapb(f: PartialFunction[rt.ExprType, Unit]): Unit = {
          f.lift(b) match {
            case Some(()) => ()
            case None => throw new Exception(s"Unexpected type for $b")
          }
        }
        a match {
          case TypePlaceholder | TypeIdentifier(_) =>
            throw new Exception("this should not happen")
          case rt.FunType(inT, outT) => unwrapb {
            case rt.FunType(inT2, outT2) =>
                sameType(inT, inT2)
                sameType(outT, outT2)
            }
          case rt.DepFunType(kind, x, t) => unwrapb {
            case rt.DepFunType(kind2, x2, t2) =>
              assert(kind == kind2)
              assert(x == x2)
              sameType(t, t2)
          }
          case dataType: DataType => dataType match {
            case DataTypeIdentifier(_) => ()
            case scalarType: ScalarType => ()
            case DataType.NatType => ()
            case OpaqueType(_) => ()
            case VectorType(size, elemType) => unwrapb {
              case VectorType(size2, elemType2) =>
                sameNat(size, size2)
                sameType(elemType, elemType2)
            }
            case IndexType(size) => unwrapb {
              case IndexType(size2) =>
                sameNat(size, size2)
            }
            case PairType(dt1, dt2) => unwrapb {
              case PairType(dt12, dt22) =>
                sameType(dt1, dt12)
                sameType(dt2, dt22)
            }
            case FragmentType(rows, columns, d3, dataType, fragmentKind, layout) => unwrapb {
              case FragmentType(_, _, d32, dataType2, _, _) =>
                sameNat(d3, d32)
                sameType(dataType, dataType2)
            }
            case ManagedBufferType(dt) => unwrapb {
              case ManagedBufferType(dt2) =>
                sameType(dt, dt2)
            }
            case DepPairType(kind, x, t) => unwrapb {
              case DepPairType(kind2, x2, t2) =>
                assert(kind == kind2)
                assert(x == x2)
                sameType(t, t2)
            }
            case apply: NatToDataApply => ???
            case ArrayType(size, elemType) => unwrapb {
              case ArrayType(size2, elemType2) =>
                sameNat(size, size2)
                sameType(elemType, elemType2)
            }
            case DepArrayType(size, fdt) => ???
          }
        }
      }

      private def sameNat(a: Nat, b: Nat): Unit = {
        val bestA = getBest(a)
        val bestB = getBest(b)
        if (bestA != bestB) {
          println(s"WARNING: $bestA != $bestB")
          val best = if (natSize(bestA) < natSize(bestB)) { bestA } else { bestB }
          println(s" --> assuming they are equal and using $best")
          map(bestA) = best
          map(bestB) = best
        }
      }
    })

    r.traverse.traverse(e, new r.traverse.PureTraversal {
      override def nat: Nat => monads.Pure[Nat] = n => return_(getBest(n))
    })
  }

  private def natSize(n: Nat): Int = {
    var i = 0
    arithexpr.arithmetic.ArithExpr.visit(n, {
      _ => i += 1
    })
    i
  }

  def expression(
    expr: r.Expr,
    ptMap: java.util.IdentityHashMap[r.Expr, PhraseType]): Phrase[_ <: PhraseType] = expr match {

    case r.Identifier(name) =>
      Identifier(name, ptMap.get(expr))

    case r.Lambda(x, e) =>
      Lambda(Identifier(x.name, ptMap.get(x)), expression(e, ptMap))

    case r.App(f, e) =>
      val ef = expression(f, ptMap)
        .asInstanceOf[Phrase[FunType[PhraseType, PhraseType]]]
      val ee = expression(e, ptMap).asInstanceOf[Phrase[PhraseType]]
      Apply(ef, ee)

    case r.DepLambda(kind, x, e) => DepLambda(kind, x)(expression(e, ptMap))

    case r.DepApp(kind, f, x) =>
      def depApp[T, I](kind: rt.Kind[T, I], f: r.Expr, arg: T): DepApply[T, I, PhraseType] =
        DepApply[T, I, PhraseType](kind,
          expression(f, ptMap).asInstanceOf[Phrase[DepFunType[I, PhraseType]]],
          arg)

      depApp(kind, f, x)

    case r.Literal(d) => d match {
      case rs.NatData(n) => Natural(n)
      case rs.IndexData(i, n) => NatAsIndex(n, Natural(i))
      case _ => Literal(data(d))
    }

    case e@TypeAnnotation(_, _) => throw new Exception(s"Missing rule for ${e.getClass}")
    case e@TypeAssertion(_, _) => throw new Exception(s"Missing rule for ${e.getClass}")
    case e@Opaque(_, _) => throw new Exception(s"Missing rule for ${e.getClass}")
    case p: r.Primitive => primitive(p, ptMap.get(p))
  }

  def data(d: rs.Data): Data = d match {
    case rs.ArrayData(a) => ArrayData(a.map(data).toVector)
    case rs.PairData(a, b) => PairData(data(a), data(b))
    case rs.BoolData(b) => BoolData(b)
    case rs.IntData(i) => IntData(i)
    case rs.FloatData(f) => FloatData(f)
    case rs.DoubleData(d) => DoubleData(d)
    case rs.VectorData(v) => VectorData(v.map(data(_)).toVector)
    case rs.IndexData(i, n) => IndexData(i, n)
    case rs.NatData(n) => NatData(n)
  }

  import rise.core.{primitives => core}
  import shine.DPIA.primitives.functional._

  def fun[T <: PhraseType](
    t: T,
    f: Phrase[T] => Phrase[_ <: PhraseType]
  ): Lambda[T, _ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  case class depFun[T, I](kind: rt.Kind[T, I], x: I) {
    def apply[U <: PhraseType](body: Phrase[U]): DepLambda[T, I, U] = DepLambda(kind, x, body)
  }

  private def primitive(p: r.Primitive,
                        t: PhraseType): Phrase[_ <: PhraseType] = {
    import rise.openCL.{primitives => rocl}
    import rise.openMP.{primitives => romp}
    import rise.Cuda.{primitives => rcuda}
    import shine.OpenCL.primitives.{functional => ocl}
    import shine.OpenMP.primitives.{functional => omp}
    import shine.cuda.primitives.{functional => cuda}
    import shine.DPIA.Types.MatchingDSL._
    import shine.OpenCL.{Global, Warp, WorkGroup, Lane, Local}

    def fromType(f: PartialFunction[PhraseType, Phrase[_ <: PhraseType]]): Phrase[_ <: PhraseType] = {
      f.lift(t) match {
        case Some(p) => p
        case None => throw new Exception(s"Unexpected type for $p : $t")
      }
    }

    p match {
      case core.printType(msg) => fromType {
        case expT(dt: DataType, w) ->: _
        =>
        fun[ExpType](expT(dt, w), e => PrintType(msg, dt, w, e))
      }

      case core.natAsIndex() => fromType {
        case nFunT(n, expT(`NatType`, `read`) ->:
          expT(IndexType(_), `read`))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT(NatType, read), e =>
            NatAsIndex(n, e)))
      }

      case core.map() => fromType {
        case ( expT(s, ai) ->: expT(t, _) ) ->:
          expT(ArrayType(n, _), _) ->:
          expT(ArrayType(_, _), _)
        =>
        fun[ExpType ->: ExpType](expT(s, ai) ->: expT(t, ai), f =>
          fun[ExpType](expT(ArrayType(n, s), ai), e =>
            Map(n, s, t, ai, f, e)))
      }

      case core.mapSeq() => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapSeq(unroll = false)(n, s, t, f, e)))
      }

      case core.mapStream() => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `read`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapStream(n, s, t, f, e)))
      }

      case core.iterateStream() => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            IterateStream(n, s, t, f, e)))
      }

      case core.mapSeqUnroll() => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            MapSeq(unroll = true)(n, s, t, f, e)))
      }

      case romp.mapPar() => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            omp.MapPar(n, s, t, f, e)))
      }

      case rocl.mapGlobal(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            ocl.Map(Global, dim)(n, s, t, f, e)))
      }

      case rocl.mapLocal(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
          fun[ExpType](expT(n`.`s, read), e =>
            ocl.Map(Local, dim)(n, s, t, f, e)))
      }

      case rocl.mapWorkGroup(dim) => fromType {
        case (expT(s, `read`) ->: expT(t, `write`)) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n `.` s, read), e =>
              ocl.Map(WorkGroup, dim)(n, s, t, f, e)))
      }

      case core.depMapSeq() => fromType {
        case nFunT(k, expT(_, `read`) ->: expT(_, `write`)) ->:
          expT(DepArrayType(n, ft1), `read`) ->:
          expT(DepArrayType(_, ft2), `write`)
        =>
        fun[`(nat)->:`[ExpType ->: ExpType]](
          k ->: (ExpType(ft1(k), read) ->: ExpType(ft2(k), write)), f =>
            fun[ExpType](ExpType(DepArrayType(n, ft1), read), e =>
              DepMapSeq(unroll = false)(n, ft1, ft2, f, e)))
      }

      case core.reduceSeq() => fromType {
        case (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`)
        =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ReduceSeq(unroll = false)(n, s, t, f, i, e))))
      }

      case core.reduceSeqUnroll() => fromType {
        case (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`)
        =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ReduceSeq(unroll = true)(n, s, t, f, i, e))))
      }

      case rocl.oclReduceSeq() => fromType {
        case aFunT(a,
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`))
        =>
        depFun(AddressSpaceKind, a)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(t, write), i =>
                fun[ExpType](expT(n`.`s, read), e =>
                  ocl.ReduceSeq(unroll = false)(n, a, s, t, f, i, e)))))
      }

      case rocl.oclReduceSeqUnroll() => fromType {
        case aFunT(a,
        (expT(t, `read`) ->: expT(s, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(_, `read`))
        =>
        depFun(AddressSpaceKind, a)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(t, read) ->: expT(s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(t, write), i =>
                fun[ExpType](expT(n`.`s, read), e =>
                  ocl.ReduceSeq(unroll = true)(n, a, s, t, f, i, e)))))
      }

      case core.scanSeq() => fromType {
        case (expT(s, `read`) ->: expT(t, `read`) ->: expT(_, `write`)) ->:
          expT(_, `write`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
        fun[ExpType ->: ExpType ->: ExpType](
          expT(s, read) ->: expT(t, read) ->: expT(t, write), f =>
            fun[ExpType](expT(t, write), i =>
              fun[ExpType](expT(n`.`s, read), e =>
                ScanSeq(n, s, t, f, i, e))))
      }

      case core.depJoin() => fromType {
        case expT(DepArrayType(n, n2d), `read`) ->:
          expT(ArrayType(_, dt), `read`)
        =>
        val lenF: NatToNatLambda = ??? // fromLift(n2d)
        fun[ExpType](expT(n`.d`{ i => lenF(i)`.`dt }, read), e =>
          DepJoin(n, lenF, dt, e))
      }

      case core.join() => fromType {
        case expT(ArrayType(n, ArrayType(m, t)), a) ->:
          expT(ArrayType(_, _), _)
        =>
        fun[ExpType](expT(n`.`(m`.`t), a), e =>
          Join(n, m, a, t, e))
      }

      case core.split() => fromType {
        case nFunT(n, expT(ArrayType(_, t), a) ->:
          expT(ArrayType(m, ArrayType(_, _)), _))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT({m*n}`.`t, a), e =>
            Split(n, m, a, t, e)))
      }

      case core.slide() => fromType {
        case nFunT(sz, nFunT(sp,
          expT(ArrayType(insz, t), `read`) ->:
          expT(ArrayType(np1, ArrayType(_, _)), `read`)))
        =>
        depFun(NatKind, sz)(
          depFun(NatKind, sp)(
            fun[ExpType](expT(insz`.`t, read), e =>
              Slide(np1-1, sz, sp, t, e))))
      }

      case core.circularBuffer() => fromType {
        case nFunT(alloc, nFunT(sz,
          (expT(s, `read`) ->: expT(t, `write`)) ->:
            expT(ArrayType(insz, _), `read`) ->:
            expT(ArrayType(n, _), `read`)))
        =>
        depFun(NatKind, alloc)(
          depFun(NatKind, sz)(
            fun[ExpType ->: ExpType](
              expT(s, read) ->: expT(t, write), load =>
                fun[ExpType](expT(insz`.`s, read), e =>
                  CircularBuffer(n, alloc, sz, s, t, load, e)))))
      }

      case core.depTile() => fromType {
        case nFunT(tile,
          ((fa/*: ExpType*/) ->: (fb/*: ExpType*/)) ->:
          (inT @ expT(ArrayType(m, s), `read`)) ->:
          expT(ArrayType(n, t), `write`))
        =>
          depFun(NatKind, tile)(
            fun[ExpType ->: ExpType](fa.asInstanceOf[ExpType] ->: fb.asInstanceOf[ExpType], f =>
              fun[ExpType](inT, e =>
                DepTile(n, tile, m-n, s, t, f, e))))
      }

      case core.rotateValues() => fromType {
        case nFunT(sz,
          (expT(s, `read`) ->: expT(_, `write`)) ->:
            expT(ArrayType(insz, _), `read`) ->:
            expT(ArrayType(n, _), `read`))
        =>
        depFun(NatKind, sz)(
          fun[ExpType ->: ExpType](
            expT(s, read) ->: expT(s, write), wr =>
              fun[ExpType](expT(insz`.`s, read), e =>
                RotateValues(n, sz, s, wr, e))))
      }

      case rocl.oclCircularBuffer() => fromType {
        case aFunT(a, nFunT(alloc, nFunT(sz,
        (expT(s, `read`) ->: expT(t, `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(n, _), `read`))))
        =>
        depFun(AddressSpaceKind, a)(
          depFun(NatKind, alloc)(
            depFun(NatKind, sz)(
              fun[ExpType ->: ExpType](
                expT(s, read) ->: expT(t, write), load =>
                  fun[ExpType](expT(insz`.`s, read), e =>
                    ocl.CircularBuffer(a, n, alloc, sz, s, t, load, e))))))
      }

      case rocl.oclRotateValues() => fromType {
        case aFunT(a, nFunT(sz,
        (expT(s, `read`) ->: expT(t, `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(n, _), `read`)))
        =>
        depFun(AddressSpaceKind, a)(
          depFun(NatKind, sz)(
              fun[ExpType ->: ExpType](
                expT(t, read) ->: expT(t, write), write_t =>
                  fun[ExpType](expT(insz`.`t, read), e =>
                    ocl.RotateValues(a, n, sz, t, write_t, e)))))
      }

      case core.reorder() => fromType {
        case nFunT(n, n2nFunT(idxF, n2nFunT(idxFinv,
          expT(ArrayType(_, t), a) ->: expT(ArrayType(_, _), _))))
        =>
        depFun(NatKind, n)(
          depFun(NatToNatKind, idxF)(
            depFun(NatToNatKind, idxFinv)(
              fun[ExpType](expT(n`.`t, a), e =>
                Reorder(n, t, a, idxF, idxFinv, e)))))
      }

      case core.gather() => fromType {
        case expT(ArrayType(m, IndexType(n)), `read`) ->:
          expT(ArrayType(_, t), `read`) ->:
          expT(ArrayType(_, _), `read`)
        =>
        fun[ExpType](expT(m`.`idx(n), read), y =>
          fun[ExpType](expT(n`.`t, read), x =>
            Gather(n, m, t, y, x)))
      }

      case core.scatter() => fromType {
        case expT(ArrayType(n, IndexType(m)), `read`) ->:
          expT(ArrayType(_, t), `write`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType](expT(n`.`idx(m), read), y =>
            fun[ExpType](expT(n`.`t, write), x =>
              Scatter(n, m, t, y, x)))
      }

      case core.transpose() => fromType {
        case expT(ArrayType(n, ArrayType(m, t)), a) ->:
          expT(ArrayType(_, ArrayType(_, _)), _)
        =>
        fun[ExpType](expT(n`.`(m`.`t), a), e =>
          Transpose(n, m, t, a, e))
      }

      case core.take() => fromType {
        case nFunT(n, expT(ArrayType(nm, t), `read`) ->:
          expT(ArrayType(_, _), `read`))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT(nm`.`t, read), e => Take(n, nm-n, t, e)))
      }

      case core.drop() => fromType {
        case nFunT(n, expT(ArrayType(nm, t), `read`) ->:
          expT(ArrayType(_, _), `read`))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT(nm`.`t, read), e =>
            Drop(n, nm-n, t, e)))
      }

      case core.padCst() => fromType {
        case nFunT(l, nFunT(q,
          expT(t, `read`) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `read`)))
        =>
        depFun(NatKind, l)(
          depFun(NatKind, q)(
            fun[ExpType](expT(t, read), cst =>
              fun[ExpType](expT(n`.`t, read), e =>
                PadCst(n, l, q, t, cst, e)))))
      }

      case core.padEmpty() => fromType {
        case nFunT(r,
          expT(ArrayType(n, t), `write`) ->: _)
        =>
        depFun(NatKind, r)(
          fun[ExpType](expT(n`.`t, `write`), e =>
            PadEmpty(n, r, t, e)))
      }

      case core.padClamp() => fromType {
        case nFunT(l, nFunT(q,
          expT(ArrayType(n, t), `read`) ->:
          expT(ArrayType(_, _), `read`)))
        =>
        depFun(NatKind, l)(
          depFun(NatKind, q)(
            fun[ExpType](expT(n`.`t, read), e =>
              PadClamp(n, l, q, t, e))))
      }

      case core.unzip() => fromType {
        case expT(ArrayType(n, PairType(s, t)), a) ->:
          expT(PairType(ArrayType(_, _), ArrayType(_, _)), _)
        =>
        fun[ExpType](expT(n`.`(s x t), a), e =>
          Unzip(n, s, t, a, e))
      }

      case core.zip() => fromType {
        case expT(ArrayType(n, s), a) ->:
          expT(ArrayType(_, t), _) ->:
          expT(ArrayType(_, PairType(_, _)), _)
        =>
        fun[ExpType](expT(n`.`s, a), x =>
          fun[ExpType](expT(n`.`t, a), y =>
            Zip(n, s, t, a, x, y)))
      }

      case core.fst() => fromType {
        case expT(PairType(s, t), `read`) ->: expT(_, read)
        =>
        fun[ExpType](expT(s x t, read), e => Fst(s, t, e))
      }

      case core.snd() => fromType {
        case expT(PairType(s, t), `read`) ->: expT(_, read)
        =>
        fun[ExpType](expT(s x t, read), e => Snd(s, t, e))
      }

      case core.mapFst() => fromType {
        case (expT(s, a) ->: expT(s2, _)) ->:
          expT(PairType(_, t), _) ->:
          expT(PairType(_, _), _)
        =>
        fun[ExpType ->: ExpType](expT(s, a) ->: expT(s2, a), f =>
          fun[ExpType](expT(s x t, a), e => MapFst(a, s, t, s2, f, e)))
      }

      case core.mapSnd() => fromType {
        case (expT(t, a) ->: expT(t2, _)) ->:
          expT(PairType(s, _), _) ->:
          expT(PairType(_, _), _) =>
        fun[ExpType ->: ExpType](expT(t, a) ->: expT(t2, a), f =>
          fun[ExpType](expT(s x t, a), e => MapSnd(a, s, t, t2, f, e)))
      }

      case core.makePair() => fromType {
        case expT(s, a) ->:
          expT(t, _) ->:
          expT(PairType(_, _), _)
        =>
        fun[ExpType](expT(s, a), x =>
          fun[ExpType](expT(t, a), y =>
            MakePair(s, t, a, x, y)))
        }

      case core.idx() => fromType {
        case expT(IndexType(n), `read`) ->:
          expT(ArrayType(_, t), `read`) ->:
          expT(_, `read`)
        =>
        fun[ExpType](expT(idx(n), read), i =>
          fun[ExpType](expT(n`.`t, read), e =>
            Idx(n, t, i, e)))
      }

      case core.select() => fromType {
        case expT(`bool`, `read`) ->:
          expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](ExpType(bool, read), cond =>
          fun[ExpType](ExpType(t, read), tExpr =>
            fun[ExpType](ExpType(t, read), fExpr =>
              IfThenElse(cond, tExpr, fExpr))))
      }

      case core.neg() => fromType {
        case expT(t, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e => UnaryOp(Operators.Unary.NEG, e))
      }

      case core.not() => fromType {
        case expT(`bool`, `read`) ->: expT(`bool`, `read`)
        =>
        fun[ExpType](expT(bool, read), e => UnaryOp(Operators.Unary.NOT, e))
      }

      case core.add() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.ADD, e1, e2)))
      }

      case core.sub() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.SUB, e1, e2)))
      }

      case core.mul() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.MUL, e1, e2)))
      }

      case core.div() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.DIV, e1, e2)))
      }

      case core.mod() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.MOD, e1, e2)))
      }

      case core.gt() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.GT, e1, e2)))
      }

      case core.lt() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.LT, e1, e2)))
      }

      case core.equal() => fromType {
        case expT(t, `read`) ->: expT(_, `read`) ->: expT(`bool`, `read`)
        =>
        fun[ExpType](expT(t, read), e1 =>
          fun[ExpType](expT(t, read), e2 =>
            BinOp(Operators.Binary.EQ, e1, e2)))
      }

      case core.cast() => fromType {
        case expT(s, `read`) ->: expT(t, `read`)
        =>
        fun[ExpType](ExpType(s, read), x => Cast(s, t, x))
      }

      case core.let() => fromType {
        case expT(s, `read`) ->:
          (expT(_, `read`) ->: expT(t, a)) ->: expT(_, _)
        =>
        fun[ExpType](ExpType(s, read), x =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, a), f =>
            Let(s, t, a, x, f)))
      }

      case core.foreignFunction(decl, n) =>
        def collectTypes(t: PhraseType): (Seq[DataTypeIdentifier], DataTypeIdentifier) = {
          t match {
            case ExpType(dt: DataTypeIdentifier, `read`) => (Seq(), dt)
            case FunType(ExpType(dt: DataTypeIdentifier, `read`), out) =>
              val (i, o) = collectTypes(out)
              (dt +: i, o)
            case DepFunType(_, _, t) => collectTypes(t)
            case _ => throw new Exception("This should not be possible")
          }
        }
        val (inTs, outT) = collectTypes(t)
        assert(inTs.length == n)

        inTs.foldRight[Phrase[_ <: PhraseType]](
          depFun(DataKind, outT)({
            val args = Seq.tabulate(n)(i => Identifier(freshName("x"), ExpType(inTs(i), read)))
            args.foldRight[Phrase[_ <: PhraseType]](
              ForeignFunctionCall(decl, n)(inTs, outT, args)
            ) {
              case (arg, f) => Lambda(arg, f)
            }
          })
        ) {
          case (t, f) => depFun(DataKind, t)(f)
        }

      case core.generate() => fromType {
        case (expT(IndexType(n), `read`) ->: expT(t, `read`)) ->:
          expT(ArrayType(n_, _), `read`)
        =>
        fun[ExpType ->: ExpType](
          expT(idx(n), read) ->: expT(t, read), f =>
            Generate(n, t, f))
      }

      case core.makeArray(_) =>
        def buildArrayPrimitive(t: PhraseType, elements: Vector[Phrase[ExpType]]
                     ): Phrase[_ <: PhraseType] = t match {
          case FunType(in: ExpType, out) =>
            fun[ExpType](in, e => buildArrayPrimitive(out, elements :+ e))
          case ExpType(ArrayType(_, et), _) => MakeArray(elements.size)(et, elements)
          case _ => error(s"did not expect $t")
        }
        buildArrayPrimitive(t, Vector())

      case core.iterate() => fromType {
        case nFunT(k,
          nFunT(l, expT(ArrayType(ln, t), `read`) ->:
            expT(ArrayType(_, _), `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(m, _), `write`) )
        =>
        depFun(NatKind, k)(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`t, read) ->: expT(l`.`t, write)), f =>
              fun[ExpType](expT(insz`.`t, read), e =>
                Iterate(ln /^ l, m, k, t, f, e))))
      }

      case rocl.oclIterate() => fromType {
        case aFunT(a, nFunT(k,
          nFunT(l, expT(ArrayType(ln, t), `read`) ->:
            expT(ArrayType(_, _), `write`)) ->:
          expT(ArrayType(insz, _), `read`) ->:
          expT(ArrayType(m, _), `write`) ))
        =>
        depFun(AddressSpaceKind, a)(depFun(NatKind, k)(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`t, read) ->: expT(l`.`t, write)), f =>
              fun[ExpType](expT(insz`.`t, read), e =>
                ocl.Iterate(a, ln /^ l, m, k, t, f, e)))))
      }

      case core.asVector() => fromType {
        case nFunT(n,
          expT(ArrayType(mn, _), a) ->:
          expT(ArrayType(m, VectorType(_, t)), _))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT(mn`.`t, a), e =>
            AsVector(n, m, t, a, e)))
      }

      case core.asVectorAligned() => fromType {
        case nFunT(n,
          expT(ArrayType(mn, _), a) ->:
          expT(ArrayType(m, VectorType(_, t)), _))
        =>
        depFun(NatKind, n)(
          fun[ExpType](expT(mn`.`t, a), e =>
            AsVectorAligned(n, m, t, a, e)))
      }

      case core.asScalar() => fromType {
        case expT(ArrayType(m, VectorType(n, t)), a) ->:
          expT(ArrayType(_, _), _)
        =>
        fun[ExpType](expT(m`.`vec(n, t), a), e =>
          AsScalar(n, m, t, a, e))
      }

      case core.vectorFromScalar() => fromType {
        case expT(_, `read`) ->:
          expT(VectorType(n, t), `read`)
        =>
        fun[ExpType](expT(t, read), e =>
          VectorFromScalar(n, t, e))
      }

      case core.indexAsNat() => fromType {
        case expT(IndexType(n), `read`) ->:
          expT(`NatType`, `read`)
        =>
        fun[ExpType](expT(idx(n), read), e =>
          IndexAsNat(n, e))
      }

      case core.toMem() => fromType {
        case expT(t, `write`) ->: expT(_, `read`)
        =>
        fun[ExpType](expT(t, write), e => ToMem(t, e))
      }

      case rocl.oclToMem() => fromType {
        case aFunT(a, expT(t, `write`) ->: expT(_, `read`))
        =>
        depFun(AddressSpaceKind, a)(
          fun[ExpType](expT(t, write), e =>
            ocl.ToMem(a, t, e)))
      }

      case rocl.oclRunPrimitive() => fromType {
        case nFunT(ls1, nFunT(ls2, nFunT(ls3,
          nFunT(gs1, nFunT(gs2, nFunT(gs3,
          expT(t, `write`) ->: _))))))
        =>
          import shine.OpenCL.{LocalSize, GlobalSize}
          depFun(NatKind, ls1)(depFun(NatKind, ls2)(depFun(NatKind, ls3)(
            depFun(NatKind, gs1)(depFun(NatKind, gs2)(depFun(NatKind, gs3)(
              fun[ExpType](expT(t, write), e =>
                ocl.Run(LocalSize(ls1, ls2, ls3), GlobalSize(gs1, gs2, gs3))(t, e))))))))
      }

      case core.dmatch() => fromType {
        case expT(DepPairType(NatKind, x: NatIdentifier, elemT), `read`) ->:
          nFunT(i, expT(elem_iT, `read`) ->: expT(outT, a))
          ->: expT(_, _) =>
          fun[ExpType](ExpType(DepPairType(NatKind, x, elemT), read), pair =>
            fun[`(nat)->:`[ExpType ->: ExpType]](i ->: (ExpType(elem_iT, read) ->: ExpType(outT, a)),f =>
              DMatch(x, elemT, outT, a, f, pair)
            )
          )
      }

      case core.makeDepPair() => fromType {
        case nFunT(fst, expT(sndT, a) ->: expT(_, _)) =>
          depFun(NatKind, fst)(fun[ExpType](expT(sndT, a), snd => MakeDepPair(a, fst, sndT, snd)))
      }

      case rcuda.globalToShared() => fromType {
        case expT(dt, write) ->: _ =>
          fun[ExpType](expT(dt, write), e =>
            cuda.GlobalToShared(dt, e))
      }

      case rcuda.asFragment() => fromType {
        case expT(ArrayType(rows, ArrayType(columns, dt)), `read`) ->:
          expT(FragmentType(_, _, d3, _, fragType, layout), _) =>
          fun[ExpType](expT(ArrayType(rows, ArrayType(columns, dt)), read), a =>
            cuda.AsFragment(rows, columns, d3, dt, fragType, layout, a))
      }

      case rcuda.asMatrix() => fromType {
        case expT(FragmentType(rows, columns, layers, dt, rt.Fragment.Accumulator, _), `read`) ->:
          expT(ArrayType(_, ArrayType(_, _)), `write`) =>
          fun[ExpType](expT(FragmentType(rows, columns, layers, dt, rt.Fragment.Accumulator, rt.MatrixLayout.None), read), dFrag =>
            cuda.AsMatrix(rows, columns, layers, dt, dFrag))
      }

      case rcuda.generateFragment() => fromType {
        case expT(dt, `read`) ->: expT(FragmentType(rows, columns, layers, _, frag, layout), read) =>
          fun[ExpType](expT(dt, read), fill =>
            cuda.GenerateFragment(rows, columns, layers, dt, frag, layout, fill))
      }

      case rcuda.tensorMMA() => fromType {
        case expT(FragmentType(_, _, _, dt, rt.Fragment.AMatrix, layoutA), `read`) ->:
          expT(FragmentType(_, _, _, _, rt.Fragment.BMatrix,layoutB), `read`) ->:
          expT(FragmentType(m, n, k, dtResult, rt.Fragment.Accumulator, _), `read`) ->:
          expT(FragmentType(_, _, _, _, rt.Fragment.Accumulator, _), `write`) =>
          fun[ExpType](expT(FragmentType(m, k, n, dt, rt.Fragment.AMatrix, layoutA), read), a =>
            fun[ExpType](expT(FragmentType(k, n, m, dt, rt.Fragment.BMatrix, layoutB), read), b =>
              fun[ExpType](expT(FragmentType(m, n, k, dtResult, rt.Fragment.Accumulator, rt.MatrixLayout.None), read), c =>
                cuda.TensorMatMultAdd(m, n, k, layoutA, layoutB, dt, dtResult, a, b, c))))
      }

      case rcuda.mapFragment() => fromType {
        case (expT(dt: DataType, `read`) ->: expT(_, `write`)) ->:
          expT(fragType : FragmentType, `read`) ->: expT(_, _) =>
          fun[ExpType ->: ExpType](ExpType(dt, read) ->: ExpType(dt, write), f =>
            fun[ExpType](ExpType(fragType, read), input =>
              cuda.MapFragment(fragType.rows, fragType.columns, fragType.d3, fragType.dataType,
                fragType.fragmentKind, fragType.layout, f, input)))
      }

      case rcuda.mapGlobal(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n`.`s, read), e =>
              cuda.Map(Global, dim)(n, s, t, f, e)))
      }

      case rcuda.mapBlock(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n`.`s, read), e =>
              cuda.Map(WorkGroup, dim)(n, s, t, f, e)))
      }

      case rcuda.mapWarp(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n`.`s, read), e =>
              cuda.Map(Warp, dim)(n, s, t, f, e)))
      }

      case rcuda.mapThreads(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n`.`s, read), e =>
              cuda.Map(Local, dim)(n, s, t, f, e)))
      }

      case rcuda.mapLane(dim) => fromType {
        case ( expT(s, `read`) ->: expT(t, `write`) ) ->:
          expT(ArrayType(n, _), `read`) ->:
          expT(ArrayType(_, _), `write`)
        =>
          fun[ExpType ->: ExpType](expT(s, read) ->: expT(t, write), f =>
            fun[ExpType](expT(n`.`s, read), e =>
              cuda.Map(Lane, dim)(n, s, t, f, e)))
      }

      case core.reduce() =>
        throw new Exception(s"$p has no implementation")

      case rise.GAP8.primitives.gap8RunPrimitive() => fromType {
        case nFunT(cores, expT(t, `write`) ->: _) =>
          depFun(NatKind, cores)(fun[ExpType](expT(t, write), e =>
            shine.GAP8.primitives.functional.Run(cores)(t, e)
          ))
      }


      case _ => throw new Exception(s"Missing rule for $p")
    }
  }

//  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
//    case rt.AddressSpace.Global => AddressSpace.Global
//    case rt.AddressSpace.Local => AddressSpace.Local
//    case rt.AddressSpace.Private => AddressSpace.Private
//    case rt.AddressSpace.Constant => AddressSpace.Constant
//    case rt.AddressSpaceIdentifier(name) => AddressSpaceIdentifier(name)
//  }

//  def nat2nat(n2n: rt.NatToNat): NatToNat = n2n match {
//    case rt.NatToNatIdentifier(name) =>
//      NatToNatIdentifier(name)
//    case rt.NatToNatLambda(x, body) =>
//      NatToNatLambda(x.range, NatIdentifier(x.name), body)
//  }

//  def scalarType(t: rt.ScalarType): ScalarType = t match {
//    case rt.bool => bool
//    case rt.int => int
//    case rt.i8 => i8
//    case rt.i16 => i16
//    case rt.i32 => i32
//    case rt.i64 => i64
//    case rt.u8 => u8
//    case rt.u16 => u16
//    case rt.u32 => u32
//    case rt.u64 => u64
//    case rt.f16 => f16
//    case rt.f32 => f32
//    case rt.f64 => f64
//  }

//  def ntd(ntd: rt.NatToData): NatToData= ntd match {
//    case rt.NatToDataLambda(n, body) =>
//      NatToDataLambda(n, dataType(body))
//    case rt.NatToDataIdentifier(x) => NatToDataIdentifier(x)
//  }
//
//  def ntn(ntn: rt.NatToNat): NatToNat= ntn match {
//    case rt.NatToNatLambda(n, body) => NatToNatLambda(n, body)
//    case rt.NatToNatIdentifier(x) => NatToNatIdentifier(x)
//  }

//  def dataTypeIdentifier(dt: rt.DataTypeIdentifier): DataTypeIdentifier = DataTypeIdentifier(dt.name)
//  def addressSpaceIdentifier(a: rt.AddressSpaceIdentifier): AddressSpaceIdentifier = AddressSpaceIdentifier(a.name)
//  def natToNatIdentifier(n: rt.NatToNatIdentifier): NatToNatIdentifier = NatToNatIdentifier(n.name)
//  def natToDataIdentifier(n: rt.NatToDataIdentifier): NatToDataIdentifier = NatToDataIdentifier(n.name)

  def accessTypeIdentifier(): rt.AccessIdentifier = rt.AccessIdentifier(freshName("access"))
}
