package rise.core

import rise.core.types._
import rise.core.types.infer._
import rise.core.semantics._
import rise.core.primitives._
import rise.core.traversal.{Continue, Result, Stop}
import rise.core.TypeLevelDSL._

import scala.collection.mutable

import scala.language.implicitConversions

object TypedDSL {

  implicit class TrivialSolutionConcat(a: Solution) {
    def <>(b: Solution): Solution = new Solution(a.ts ++ b.ts, a.ns ++ b.ns, a.as ++ b.as, a.n2ds ++ b.n2ds)
  }

  final case class Opaque(e: Expr, ftvSubs: Solution = Solution())(override val t: Type = e.t) extends Primitive {
    import TypedDSL.Opaque._
    override def typeScheme: Type = e.t
    override def setType(t: Type): Opaque = {
      val ftvSubs = getFTVSubs(t)
      this.copy(ftvSubs = ftvSubs)(freeze(ftvSubs, t))
    }
    override def name: String = s"{Opaque Expr: $t}"
  }

  object Opaque {
    def freeze(ftvSubs: Solution, t: Type): Type = new Solution(
      ftvSubs.ts.mapValues(dt => dt.asInstanceOf[DataTypeIdentifier].asExplicit),
      ftvSubs.ns.mapValues(n => n.asInstanceOf[NatIdentifier].asExplicit),
      ftvSubs.as.mapValues(a => a.asInstanceOf[AddressSpaceIdentifier].asExplicit),
      ftvSubs.n2ds.mapValues(n2d => n2d.asInstanceOf[NatToDataIdentifier].asExplicit)
    )(t)

    def getFTVSubs(t: Type): Solution = {
      import scala.collection.immutable.Map
      val emptySubs: (Map[Type, Type],
        Map[NatIdentifier, Nat],
        Map[AddressSpaceIdentifier, AddressSpace],
        Map[NatToDataIdentifier, NatToData]) = (Map(), Map(), Map(), Map())
      val ftvs = TopLevel.getFTVs(t)
      val subs = ftvs.foldLeft(emptySubs)((subs, ftv) => subs match {
        case (ts, ns, as, n2ds) => ftv match {
          case _: TypeIdentifier => throw TypeException("TypeIdentifier cannot be frozen")
          case i: DataTypeIdentifier => (ts ++ Map(i -> i), ns, as, n2ds)
          case i: NatIdentifier => (ts, ns ++ Map(i -> i), as, n2ds)
          case i: AddressSpaceIdentifier => (ts, ns, as ++ Map(i -> i), n2ds)
          case i: NatToDataIdentifier => (ts, ns, as, n2ds ++ Map(i -> i))
          case i => throw TypeException(s"${i.getClass} is not supported yet")
        }
      })
      new Solution(subs._1, subs._2, subs._3, subs._4)
    }
  }

  final case class TopLevel(e: Expr, inst: Solution = Solution())(override val t: Type = e.t) extends Primitive {
    import TypedDSL.TopLevel._
    override def typeScheme: Type = e.t
    override def setType(t: Type): TopLevel = {
      val subs = instantiate(t)
      this.copy(inst = subs)(subs(t))
    }
    override def name: String = s"TopLevel Expr: $t"
  }

  object TopLevel {
    def instantiate(t: Type): Solution = {
      import scala.collection.immutable.Map
      val emptySubs: (Map[Type, Type],
          Map[NatIdentifier, Nat],
          Map[AddressSpaceIdentifier, AddressSpace],
          Map[NatToDataIdentifier, NatToData]) = (Map(), Map(), Map(), Map())
      val ftvs = getFTVs(t)
      val subs = ftvs.foldLeft(emptySubs)((subs, ftv) => subs match {
        case (ts, ns, as, n2ds) => ftv match {
          case i: TypeIdentifier => (ts ++ Map(i -> implT(identity)), ns, as, n2ds)
          case i: DataTypeIdentifier => (ts ++ Map(i -> implDT(identity)), ns, as, n2ds)
          case i: NatIdentifier => (ts, ns ++ Map(i -> implN(identity)), as, n2ds)
          case i: AddressSpaceIdentifier => (ts, ns, as ++ Map(i -> implA(identity)), n2ds)
          case i: NatToDataIdentifier => (ts, ns, as, n2ds ++ Map(i -> implN2DT(identity)))
          case i => throw TypeException(s"${i.getClass} is not supported yet")
        }
      })
      new Solution(subs._1, subs._2, subs._3, subs._4)
    }

    def getFTVs(t: Type): Seq[Kind.Identifier] = {
      val ftvs = mutable.ListBuffer[Kind.Identifier]()
      traversal.types.DepthFirstLocalResult(t, new traversal.Visitor {
        override def visitType[T <: Type](t: T): Result[T] = {
          t match {
            case i: DataTypeIdentifier if ! i.isExplicit => ftvs += i
            case i: TypeIdentifier => ftvs += i
            case _ =>
          }
          Continue(t, this)
        }
        override def visitNat(ae: Nat): Result[Nat] = Stop(ae.visitAndRebuild({
          case i: NatIdentifier if ! i.isExplicit => ftvs += i
            i
          case n => n
        }))
        override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = {
          a match {
            case i: AddressSpaceIdentifier if ! i.isExplicit => ftvs += i
            case _ =>
          }
          Continue(a, this)
        }
        override def visitN2N(n2n: NatToNat): Result[NatToNat] = {
          n2n match {
            case i: NatToNatIdentifier if ! i.isExplicit => ftvs += i
            case _ =>
          }
          Continue(n2n, this)
        }
        override def visitN2D(n2d: NatToData): Result[NatToData] = {
          n2d match {
            case i: NatToDataIdentifier if ! i.isExplicit => ftvs += i
            case _ =>
          }
          Continue(n2d, this)
        }
      })
      ftvs.distinct.toSeq
    }

    case class Visitor(ftvSubs: Solution, sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
      override def visitNat(ae: Nat): Result[Nat] = Stop(cascadedApply(ftvSubs, sol, ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(cascadedApply(ftvSubs, sol, t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(cascadedApply(ftvSubs, sol, a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(cascadedApply(ftvSubs, sol, n2d))
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, t: Type): Type = {
      traversal.types.DepthFirstLocalResult(t, new Visitor(ftvSubs, sol) {
        override def visitType[T <: Type](t: T): Result[T] = t match {
          case i: TypeIdentifier => ftvSubs.ts.get(i) match {
            case Some(i) => sol.ts.get(i.asInstanceOf[TypeIdentifier]) match {
              case Some(x) => Stop(x.asInstanceOf[T])
              case None => Continue(t, this)
            }
            case None => Continue(t, this)
          }
          case _ => Continue(t, this)
        }
      })
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, n: Nat): Nat = {
      n.visitAndRebuild {
        case i: NatIdentifier => ftvSubs.ns.get(i) match {
          case Some(n) => sol.ns.get(n.asInstanceOf[NatIdentifier]) match {
            case Some(x) => x
            case None => n
          }
          case None => i
        }
        case n => n
      }
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, a: AddressSpace): AddressSpace = {
      a match {
        case i: AddressSpaceIdentifier => ftvSubs.as.get(i) match {
          case Some(a) => sol.as.get(a.asInstanceOf[AddressSpaceIdentifier]) match {
            case Some(x) => x
            case None => a
          }
          case None => i
        }
        case a => a
      }
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, n2d: NatToData): NatToData = {
      n2d match {
        case i: NatToDataIdentifier => ftvSubs.n2ds.get(i) match {
          case Some(n2d) => sol.n2ds.get(n2d.asInstanceOf[NatToDataIdentifier]) match {
            case Some(x) => x
            case None => n2d
          }
          case None => i
        }
        case n2d => n2d
      }
    }
  }

  final case class TDSL[+T <: Expr](private val e: T) {
    def toExpr: Expr = TDSL.infer(e)
    def >>=[X <: Expr](f: T => TDSL[X]): TDSL[X] = f(e)
  }

  implicit def typed[T <: Expr](e: T): TDSL[Opaque] = TDSL(Opaque(e)())

  def toTDSL[T <: Expr](e: T): TDSL[T] = TDSL(e)

  implicit def toExpr[T <: Expr](d: TDSL[T]): Expr = d.toExpr

  def topLevel(e: Expr): TopLevel = TopLevel(e)()

  implicit def tdslTopLevel[T <: Expr](d: TDSL[T]): TDSL[TopLevel] = toTDSL(topLevel(toExpr(d)))

  def erase[T <: Expr](e: T): T = traversal.DepthFirstLocalResult(e, new traversal.Visitor {
    override def visitExpr(e: Expr): Result[Expr] = e match {
      case l: Literal => Continue(l, this)
      case _ => Continue(e.setType(TypePlaceholder), this)
    }
  }).asInstanceOf[T]

  def untyped[T <: Expr](e: T): TDSL[T] = toTDSL(erase(e))

  object TDSL {
    case class Visitor(sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = e match {
        case Opaque(x, _) => Stop(x)
        case TopLevel(x, inst) => Stop(traversal.DepthFirstLocalResult(x, TopLevel.Visitor(inst, sol)))
        case _ => Continue(e, this)
      }
      override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(sol(t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(sol(a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))
    }

    def constrainTypes(expr: Expr,
                       constraints: mutable.ArrayBuffer[Constraint],
                       env: mutable.Map[String, Type]
                      ): (Expr, Solution) = {
      def constrained(e: Expr): (Expr, Solution) = constrainTypes(e, constraints, env)
      def genType(e: Expr): Type = if (e.t == TypePlaceholder) freshTypeIdentifier else e.t

      expr match {
        case i: Identifier =>
          val t = env
            .getOrElse(i.name, error(s"$i has no type in the environment"))
          (i.setType(t), Solution())

        case Lambda(x, e) =>
          val tx = x.setType(genType(x))
          env update (tx.name, tx.t)
          val (te, ftvSubsE) = constrained(e)
          env remove tx.name
          val ft = FunType(tx.t, te.t)
          val exprT = genType(expr)
          val constraint = TypeConstraint(exprT, ft)
          constraints += constraint
          (Lambda(tx, te)(ft), ftvSubsE)

        case App(f, e) =>
          val (tf, ftvSubsF) = constrained(f)
          val (te, ftvSubsE) = constrained(e)
          val exprT = genType(expr)
          val constraint = TypeConstraint(tf.t, FunType(te.t, exprT))
          constraints += constraint
          (App(tf, te)(exprT), ftvSubsF <> ftvSubsE)

        case DepLambda(x, e) =>
          val (te, ftvSubsE) = constrained(e)
          val exprT = genType(expr)
          val tf = x match {
            case n: NatIdentifier =>
              DepLambda[NatKind](n, te)(DepFunType[NatKind, Type](n, te.t))
            case dt: DataTypeIdentifier =>
              DepLambda[DataKind](dt, te)(DepFunType[DataKind, Type](dt, te.t))
            case ad: AddressSpaceIdentifier =>
              DepLambda[AddressSpaceKind](ad, te)(DepFunType[AddressSpaceKind, Type](ad, te.t))
            case n2n: NatToNatIdentifier =>
              DepLambda[NatToNatKind](n2n, te)(DepFunType[NatToNatKind, Type](n2n, te.t))
          }
          val constraint = TypeConstraint(exprT, tf.t)
          constraints += constraint
          (tf, ftvSubsE)

        case DepApp(f, x) =>
          val (tf, ftvSubsF) = constrained(f)
          val exprT = genType(expr)
          val constraint = DepConstraint(tf.t, x, exprT)
          constraints += constraint
          (DepApp(tf, x)(exprT), ftvSubsF)

        case Annotation(e, t) =>
          val (te, ftvSubsE) = constrained(e)
          val ftvSubsT = Opaque.getFTVSubs(t)
          val constraint = TypeConstraint(te.t, Opaque.freeze(ftvSubsT, t))
          constraints += constraint
          (te, ftvSubsE <> ftvSubsT)

        case o: Opaque =>
          val frozenExpr = o.setType(o.typeScheme)
          (frozenExpr, frozenExpr.ftvSubs)

        case l: Literal => (l, Solution())

        case p: Primitive => (p.setType(p.typeScheme), Solution())
      }
    }

    def infer(e: Expr): Expr = {
      val constraints = mutable.ArrayBuffer[Constraint]()
      val (typed_e, ftvSubs) = constrainTypes(e, constraints, mutable.Map())
      val solution = solve(constraints) match {
        case Solution(ts, ns, as, n2ds) => Solution(
          ts.mapValues(t => ftvSubs(t)),
          ns.mapValues(n => ftvSubs(n)),
          as.mapValues(a => ftvSubs(a)),
          n2ds.mapValues(n2d => ftvSubs(n2d))
        )
      }
      val r = traversal.DepthFirstLocalResult(typed_e, Visitor(solution))
      r
    }
  }

  def identifier(name: String): TDSL[Identifier] = toTDSL(Identifier(name)())
  def lambda(x: TDSL[Identifier], e: TDSL[Expr]): TDSL[Lambda] =
    x >>= (x => e >>= (e => toTDSL(Lambda(x, e)())))
  def app(f: TDSL[Expr], e: TDSL[Expr]): TDSL[App] = f >>= (f => e >>= (e => toTDSL(App(f, e)())))
  def depLambda[K <: Kind : KindName](x: K#I with Kind.Explicitness, e: TDSL[Expr]): TDSL[DepLambda[K]] =
    e >>= (e => toTDSL(DepLambda[K](x, e)()))
  def depApp[K <: Kind](f: TDSL[Expr], x: K#T): TDSL[DepApp[K]] = f >>= (f => toTDSL(DepApp[K](f, x)()))
  def literal(d: semantics.Data): TDSL[Literal] = toTDSL(Literal(d))

  def array(n: Int): TDSL[MakeArray] = toTDSL(primitives.MakeArray(n)())
  def cast: TDSL[Cast] = toTDSL(primitives.Cast()())
  def depJoin: TDSL[DepJoin] = toTDSL(primitives.DepJoin()())
  def depMapSeq: TDSL[DepMapSeq] = toTDSL(primitives.DepMapSeq()())
  def depZip: TDSL[DepZip] = toTDSL(primitives.DepZip()())
  def drop: TDSL[Drop] = toTDSL(primitives.Drop()())
  def fst: TDSL[Fst] = toTDSL(primitives.Fst()())
  def gather: TDSL[Gather] = toTDSL(primitives.Gather()())
  def generate: TDSL[Generate] = toTDSL(primitives.Generate()())
  def idx: TDSL[Idx] = toTDSL(primitives.Idx()())
  def id: TDSL[Id] = toTDSL(primitives.Id()())
  def indexAsNat: TDSL[IndexAsNat] = toTDSL(primitives.IndexAsNat()())
  def iterate: TDSL[Iterate] = toTDSL(primitives.Iterate()())
  def join: TDSL[Join] = toTDSL(primitives.Join()())
  def let: TDSL[Let] = toTDSL(primitives.Let()())
  def map: TDSL[Map] = toTDSL(primitives.Map()())
  def mapFst: TDSL[MapFst] = toTDSL(primitives.MapFst()())
  def mapSnd: TDSL[MapSnd] = toTDSL(primitives.MapSnd()())
  def mapSeq: TDSL[MapSeq] = toTDSL(primitives.MapSeq()())
  def mapSeqUnroll: TDSL[MapSeqUnroll] = toTDSL(primitives.MapSeqUnroll()())
  def natAsIndex: TDSL[NatAsIndex] = toTDSL(primitives.NatAsIndex()())
  def padCst: TDSL[PadCst] = toTDSL(primitives.PadCst()())
  def padClamp: TDSL[PadClamp] = toTDSL(primitives.PadClamp()())
  def partition: TDSL[Partition] = toTDSL(primitives.Partition()())
  def pair: TDSL[Pair] = toTDSL(primitives.Pair()())
  def reduce: TDSL[Reduce] = toTDSL(primitives.Reduce()())
  def reduceSeq: TDSL[ReduceSeq] = toTDSL(primitives.ReduceSeq()())
  def reduceSeqUnroll: TDSL[ReduceSeqUnroll] = toTDSL(primitives.ReduceSeqUnroll()())
  def reorder: TDSL[Reorder] = toTDSL(primitives.Reorder()())
  def scanSeq: TDSL[ScanSeq] = toTDSL(primitives.ScanSeq()())
  def slide: TDSL[Slide] = toTDSL(primitives.Slide()())
  def slideSeq(roprimT: SlideSeq.Rotate): TDSL[SlideSeq] = toTDSL(primitives.SlideSeq(roprimT)())
  def snd: TDSL[Snd] = toTDSL(primitives.Snd()())
  def split: TDSL[Split] = toTDSL(primitives.Split()())
  def take: TDSL[Take] = toTDSL(primitives.Take()())
  def transpose: TDSL[Transpose] = toTDSL(primitives.Transpose()())
  def select: TDSL[Select] = toTDSL(primitives.Select()())
  def unzip: TDSL[Unzip] = toTDSL(primitives.Unzip()())
  def zip: TDSL[Zip] = toTDSL(primitives.Zip()())

  def neg: TDSL[Neg] = toTDSL(primitives.Neg()())
  def add: TDSL[Add] = toTDSL(primitives.Add()())
  def sub: TDSL[Sub] = toTDSL(primitives.Sub()())
  def mul: TDSL[Mul] = toTDSL(primitives.Mul()())
  def div: TDSL[Div] = toTDSL(primitives.Div()())
  def mod: TDSL[Mod] = toTDSL(primitives.Mod()())
  def gt: TDSL[Gt] = toTDSL(primitives.Gt()())
  def lt: TDSL[Lt] = toTDSL(primitives.Lt()())
  def equal: TDSL[Equal] = toTDSL(primitives.Equal()())

  def asVector: TDSL[AsVector] = toTDSL(primitives.AsVector()())
  def asVectorAligned: TDSL[AsVectorAligned] = toTDSL(primitives.AsVectorAligned()())
  def asScalar: TDSL[AsScalar] = toTDSL(primitives.AsScalar()())
  def vectorFromScalar: TDSL[VectorFromScalar] = toTDSL(primitives.VectorFromScalar()())

  def printType(msg: String): TDSL[PrintType] = toTDSL(PrintType(msg)())
  def typeHole(msg: String): TDSL[TypeHole] = toTDSL(TypeHole(msg)())

  implicit class Ops(lhs: TDSL[Expr]) {

    // binary
    def +(rhs: TDSL[Expr]): TDSL[App] = add(lhs)(rhs)
    def -(rhs: TDSL[Expr]): TDSL[App] = sub(lhs)(rhs)
    def *(rhs: TDSL[Expr]): TDSL[App] = mul(lhs)(rhs)
    def /(rhs: TDSL[Expr]): TDSL[App] = div(lhs)(rhs)
    def %(rhs: TDSL[Expr]): TDSL[App] = mod(lhs)(rhs)
    def >(rhs: TDSL[Expr]): TDSL[App] = gt(lhs)(rhs)
    def <(rhs: TDSL[Expr]): TDSL[App] = lt(lhs)(rhs)
    def =:=(rhs: TDSL[Expr]): TDSL[App] = equal(lhs)(rhs)

    // unary
    def unary_- : TDSL[App] = neg(lhs)

    // pair accesses
    def _1: TDSL[App] = fst(lhs)
    def _2: TDSL[App] = snd(lhs)
  }

  implicit class Indexing(e: TDSL[Expr]) {
    def `@`(i: TDSL[Expr]): TDSL[App] = idx(i)(e)
  }

  /*
  implicit class TypeAnnotation(t: Type) {
    def ::[T <: Expr](e: TDSL[T]): TDSL[T] = e >>= (e =>
      if (e.t == TypePlaceholder) tdsl(e.setType(t).asInstanceOf[T])
      else if (e.t == t) tdsl(e) else
        throw TypeException(s"tried to replace ${e.t} with ${t}, but type annotation can only replace a TypePlaceholder"))
    def `:`[T <: Expr](e: TDSL[T]): TDSL[T] = e :: t
  }
   */
  implicit class TypeAnnotation(t: Type) {
    def ::[T <: Expr](e: TDSL[T]): TDSL[Expr] = e >>= (e => toTDSL(Annotation(e, t)))
    def `:`[T <: Expr](e: TDSL[T]): TDSL[Expr] = e :: t
  }

  implicit class FunCall(f: TDSL[Expr]) {

    def apply(e: TDSL[Expr]): TDSL[App] = app(f, e)
    def apply(n: Nat): TDSL[DepApp[NatKind]] = depApp[NatKind](f, n)
    def apply(dt: DataType): TDSL[DepApp[DataKind]] = depApp[DataKind](f, dt)
    def apply(a: AddressSpace): TDSL[DepApp[AddressSpaceKind]] = depApp[AddressSpaceKind](f, a)

    def apply(n2n: NatToNat): TDSL[DepApp[NatToNatKind]] = depApp[NatToNatKind](f, n2n)

    def apply(e1: TDSL[Expr], e2: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr], e4: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)(e4)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr], e4: TDSL[Expr], e5: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)(e4)(e5)
    }
  }

  implicit class FunPipe(e: TDSL[Expr]) {
    def |>(f: TDSL[Expr]): TDSL[App] = f.apply(e)
  }

  implicit class FunPipeReverse(f: TDSL[Expr]) {
    def $(e: TDSL[Expr]): TDSL[App] = f.apply(e)
  }

  implicit class FunComp(f: TDSL[Expr]) {
    def >>(g: TDSL[Expr]): TDSL[Lambda] = fun(x => g(f(x)))
  }

  implicit class FunCompReverse(f: TDSL[Expr]) {
    def o(g: TDSL[Expr]): TDSL[Lambda] = fun(x => f(g(x)))
  }

  // function values
  object fun {
    def apply(t: Type)(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = {
      val x = identifier(freshName("e")) >>= (i => toTDSL(i.setType(t)))
      lambda(x, f(x))
    }

    def apply(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)

    private def untyped(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    //noinspection TypeAnnotation
    def apply(ft: FunType[Type, Type]) = new {
      def apply(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
    }
  }

  object nFun {
    def apply(r: rise.arithmetic.Range, f: NatIdentifier => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val x = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      nFun(rise.arithmetic.RangeAdd(0, rise.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = rise.arithmetic.RangeAdd(0, rise.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun(f(n, _)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = rise.arithmetic.RangeAdd(0, rise.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2) => f(n, n1, n2)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = rise.arithmetic.RangeAdd(0, rise.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3) => f(n, n1, n2, n3)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = rise.arithmetic.RangeAdd(0, rise.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3, n4) => f(n, n1, n2, n3, n4)))
    }
  }

  object dtFun {
    def apply(f: DataTypeIdentifier => TDSL[Expr]): TDSL[DepLambda[DataKind]] = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      depLambda[DataKind](x, f(x))
    }
  }

  implicit def wrapInNatExpr(n: Nat): TDSL[Literal] = literal(NatData(n))

  def l(i: Int): TDSL[Literal] = literal(IntData(i))
  def l(f: Float): TDSL[Literal] = literal(FloatData(f))
  def l(d: Double): TDSL[Literal] = literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): TDSL[Literal] = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): TDSL[Literal] = literal(VectorData(v))
  def larr(a: Seq[Data]): TDSL[Literal] = literal(ArrayData(a))

  object foreignFun {
    def apply(name: String, t: Type): TDSL[ForeignFunction] = {
      toTDSL(ForeignFunction(ForeignFunction.Decl(name, None))(t))
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): TDSL[ForeignFunction] = {
      toTDSL(ForeignFunction(ForeignFunction.Decl(name,
        Some(ForeignFunction.Def(params, body))))(t))
    }
  }
}
