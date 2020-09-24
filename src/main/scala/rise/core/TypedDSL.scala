package rise.core

import rise.core.TypeLevelDSL._
import rise.core.primitives._
import rise.core.semantics._
import rise.core.traversal.{Continue, Result, Stop}
import rise.core.types._
import rise.core.types.InferenceException.error

import scala.collection.mutable
import scala.language.implicitConversions

// scalastyle:off number.of.methods
object TypedDSL {

  implicit class TrivialSolutionConcat(a: Solution) {
    def <>(b: Solution): Solution =
      new Solution(
        a.ts ++ b.ts,
        a.ns ++ b.ns,
        a.as ++ b.as,
        a.n2ds ++ b.n2ds,
        a.n2ns ++ b.n2ns,
        a.natColls ++ b.natColls
      )
  }

  final case class Opaque(e: Expr, ftvSubs: Solution = Solution())(
      override val t: Type = e.t
  ) extends Primitive {
    import TypedDSL.Opaque._
    override def typeScheme: Type = e.t
    override def setType(t: Type): Opaque = {
      val ftvSubs = getFTVSubs(t)
      this.copy(ftvSubs = ftvSubs)(freeze(ftvSubs, t))
    }
    override def name: String = s"{Opaque Expr: $t}"
  }

  object Opaque {
    def freeze(ftvSubs: Solution, t: Type): Type =
      new Solution(
        ftvSubs.ts.mapValues(dt =>
          dt.asInstanceOf[DataTypeIdentifier].asExplicit
        ),
        ftvSubs.ns.mapValues(n => n.asInstanceOf[NatIdentifier].asExplicit),
        ftvSubs.as.mapValues(a =>
          a.asInstanceOf[AddressSpaceIdentifier].asExplicit
        ),
        ftvSubs.n2ds.mapValues(n2d =>
          n2d.asInstanceOf[NatToDataIdentifier].asExplicit
        ),
        ftvSubs.n2ns.mapValues(n2n =>
          n2n.asInstanceOf[NatToNatIdentifier].asExplicit
        ),
        ftvSubs.natColls.mapValues(natColl =>
          natColl.asInstanceOf[NatCollectionIdentifier].asExplicit
      ))(t)

    def getFTVSubs(t: Type): Solution = {
      import scala.collection.immutable.Map
      val emptySubs: (
          Map[Type, Type],
          Map[NatIdentifier, Nat],
          Map[AddressSpaceIdentifier, AddressSpace],
          Map[NatToDataIdentifier, NatToData],
          Map[NatToNatIdentifier, NatToNat],
          Map[NatCollectionIdentifier, NatCollection]
      ) = (Map(), Map(), Map(), Map(), Map(), Map())
      val ftvs = TopLevel.getFTVs(t)
      val subs = ftvs.foldLeft(emptySubs)((subs, ftv) =>
        subs match {
          case (ts, ns, as, n2ds, n2ns, natColls) =>
            ftv match {
              case _: TypeIdentifier =>
                throw TypeException("TypeIdentifier cannot be frozen")
              case i: DataTypeIdentifier => (ts ++ Map(i -> i), ns, as, n2ds, n2ns, natColls)
              case i: NatIdentifier      => (ts, ns ++ Map(i -> i), as, n2ds,  n2ns, natColls)
              case i: AddressSpaceIdentifier =>
                (ts, ns, as ++ Map(i -> i), n2ds,  n2ns, natColls)
              case i: NatToDataIdentifier => (ts, ns, as, n2ds ++ Map(i -> i), n2ns,  natColls)
              case i: NatToNatIdentifier => (ts, ns, as, n2ds, n2ns ++ Map(i -> i),  natColls)
              case i: NatCollectionIdentifier => (ts, ns, as, n2ds,  n2ns, natColls ++ Map(i -> i))
              case i =>
                throw TypeException(s"${i.getClass} is not supported yet")
            }
        }
      )
      new Solution(subs._1, subs._2, subs._3, subs._4, subs._5, subs._6)
    }
  }

  final case class TopLevel(e: Expr, inst: Solution = Solution())(
      override val t: Type = e.t
  ) extends Primitive {
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
      val emptySubs: (
          Map[Type, Type],
          Map[NatIdentifier, Nat],
          Map[AddressSpaceIdentifier, AddressSpace],
          Map[NatToDataIdentifier, NatToData],
          Map[NatToNatIdentifier, NatToNat],
          Map[NatCollectionIdentifier, NatCollection]
      ) = (Map(), Map(), Map(), Map(), Map(), Map())
      val ftvs = getFTVs(t)
      val subs = ftvs.foldLeft(emptySubs)((subs, ftv) =>
        subs match {
          case (ts, ns, as, n2ds, n2ns, natColls) =>
            ftv match {
              case i: TypeIdentifier =>
                (ts ++ Map(i -> implType(identity)), ns, as, n2ds, n2ns, natColls)
              case i: DataTypeIdentifier =>
                (ts ++ Map(i -> implDT(identity)), ns, as, n2ds, n2ns,  natColls)
              case i: NatIdentifier =>
                (ts, ns ++ Map(i -> implNat(identity)), as, n2ds, n2ns,  natColls)
              case i: AddressSpaceIdentifier =>
                (ts, ns, as ++ Map(i -> implAddr(identity)), n2ds, n2ns,  natColls)
              case i: NatToDataIdentifier =>
                (ts, ns, as, n2ds ++ Map(i -> implN2DT(identity)), n2ns,  natColls)
              case i: NatToNatIdentifier =>
                (ts, ns, as, n2ds, n2ns ++ Map(i -> implN2N(identity)), natColls)
              case i: NatCollectionIdentifier =>
                (ts, ns, as, n2ds,  n2ns, natColls ++ Map(i -> implNatColl(identity)))
              case i =>
                throw TypeException(s"${i.getClass} is not supported yet")
            }
        }
      )
      new Solution(subs._1, subs._2, subs._3, subs._4, subs._5, subs._6)
    }

    def getFTVs(t: Type): Seq[Kind.Identifier] = {
      val ftvs = mutable.ListBuffer[Kind.Identifier]()
      traversal.types.DepthFirstLocalResult(
        t,
        new traversal.Visitor {
          override def visitType[T <: Type](t: T): Result[T] = {
            t match {
              case i: DataTypeIdentifier if !i.isExplicit => ftvs += i
              case i: TypeIdentifier                      => ftvs += i
              case _                                      =>
            }
            Continue(t, this)
          }
          override def visitNat(ae: Nat): Result[Nat] =
            Stop(ae.visitAndRebuild({
              case i: NatIdentifier if !i.isExplicit =>
                ftvs += i
                i
              case n => n
            }))
          override def visitAddressSpace(
              a: AddressSpace
          ): Result[AddressSpace] = {
            a match {
              case i: AddressSpaceIdentifier if !i.isExplicit => ftvs += i
              case _                                          =>
            }
            Continue(a, this)
          }
          override def visitN2N(n2n: NatToNat): Result[NatToNat] = {
            n2n match {
              case i: NatToNatIdentifier if !i.isExplicit => ftvs += i
              case _                                      =>
            }
            Continue(n2n, this)
          }
          override def visitN2D(n2d: NatToData): Result[NatToData] = {
            n2d match {
              case i: NatToDataIdentifier if !i.isExplicit => ftvs += i
              case _                                       =>
            }
            Continue(n2d, this)
          }
        }
      )
      ftvs.distinct
    }

    case class Visitor(ftvSubs: Solution, sol: Solution)
        extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
      override def visitNat(ae: Nat): Result[Nat] =
        Stop(cascadedApply(ftvSubs, sol, ae))
      override def visitType[T <: Type](t: T): Result[T] =
        Stop(cascadedApply(ftvSubs, sol, t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
        Stop(cascadedApply(ftvSubs, sol, a))
      override def visitN2D(n2d: NatToData): Result[NatToData] =
        Stop(cascadedApply(ftvSubs, sol, n2d))
    }

    private def cascadedApply(
        ftvSubs: Solution,
        sol: Solution,
        t: Type
    ): Type = {
      traversal.types.DepthFirstLocalResult(
        t,
        new Visitor(ftvSubs, sol) {
          override def visitType[T <: Type](t: T): Result[T] = t match {
            case i: TypeIdentifier =>
              ftvSubs.ts.get(i) match {
                case Some(i) =>
                  sol.ts.get(i.asInstanceOf[TypeIdentifier]) match {
                    case Some(x) => Stop(x.asInstanceOf[T])
                    case None    => Continue(t, this)
                  }
                case None => Continue(t, this)
              }
            case _ => Continue(t, this)
          }
        }
      )
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, n: Nat): Nat = {
      n.visitAndRebuild {
        case i: NatIdentifier =>
          ftvSubs.ns.get(i) match {
            case Some(n) =>
              sol.ns.get(n.asInstanceOf[NatIdentifier]) match {
                case Some(x) => x
                case None    => n
              }
            case None => i
          }
        case n => n
      }
    }

    private def cascadedApply(
        ftvSubs: Solution,
        sol: Solution,
        a: AddressSpace
    ): AddressSpace = {
      a match {
        case i: AddressSpaceIdentifier =>
          ftvSubs.as.get(i) match {
            case Some(a) =>
              sol.as.get(a.asInstanceOf[AddressSpaceIdentifier]) match {
                case Some(x) => x
                case None    => a
              }
            case None => i
          }
        case a => a
      }
    }

    private def cascadedApply(
        ftvSubs: Solution,
        sol: Solution,
        n2d: NatToData
    ): NatToData = {
      n2d match {
        case i: NatToDataIdentifier =>
          ftvSubs.n2ds.get(i) match {
            case Some(n2d) =>
              sol.n2ds.get(n2d.asInstanceOf[NatToDataIdentifier]) match {
                case Some(x) => x
                case None    => n2d
              }
            case None => i
          }
        case n2d => n2d
      }
    }
  }

  final case class ToBeTyped[+T <: Expr](private val e: T) {
    def toExpr: Expr = TDSL.infer(e)
    def >>=[X <: Expr](f: T => ToBeTyped[X]): ToBeTyped[X] = f(e)
  }

  implicit def isTyped[T <: Expr](e: T): ToBeTyped[Opaque] = ToBeTyped(Opaque(e)())

  def toBeTyped[T <: Expr](e: T): ToBeTyped[T] = ToBeTyped(e)

  implicit def toExpr[T <: Expr](d: ToBeTyped[T]): Expr = d.toExpr

  def topLevel(e: Expr): TopLevel = TopLevel(e)()

  implicit def untypedTopLevel[T <: Expr](d: ToBeTyped[T]): ToBeTyped[TopLevel] =
    toBeTyped(topLevel(toExpr(d)))

  def eraseTypeFromExpr[T <: Expr](e: T): T =
    traversal
      .DepthFirstLocalResult(
        e,
        new traversal.Visitor {
          override def visitExpr(e: Expr): Result[Expr] = e match {
            case l: Literal => Continue(l, this)
            case _          => Continue(e.setType(TypePlaceholder), this)
          }
        }
      )
      .asInstanceOf[T]

  def eraseType[T <: Expr](e: T): ToBeTyped[T] = toBeTyped(eraseTypeFromExpr(e))

  object TDSL {
    case class Visitor(sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = e match {
        case Opaque(x, _) => Stop(x)
        case TopLevel(x, inst) =>
          Stop(traversal.DepthFirstLocalResult(x, TopLevel.Visitor(inst, sol)))
        case _ => Continue(e, this)
      }
      override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
      override def visitType[T <: Type](t: T): Result[T] =
        Stop(sol(t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
        Stop(sol(a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))

      override def visitN2N(n2n: NatToNat): Result[NatToNat] =
        Stop(sol(n2n))
    }

    def constrainTypes(
        expr: Expr,
        constraints: mutable.ArrayBuffer[Constraint],
        env: mutable.Map[String, Type]
    ): (Expr, Solution) = {
      def constrained(e: Expr): (Expr, Solution) =
        constrainTypes(e, constraints, env)
      def genType(e: Expr): Type =
        if (e.t == TypePlaceholder) freshTypeIdentifier else e.t

      expr match {
        case i: Identifier =>
          val t = env.getOrElseUpdate(i.name,
            if (i.t == TypePlaceholder) {
              error(s"$i has no type")(Seq())
            } else {
              i.t
            })
          constraints += TypeConstraint(t, i.t)
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
              DepLambda[AddressSpaceKind](ad, te)(
                DepFunType[AddressSpaceKind, Type](ad, te.t)
              )
            case n2n: NatToNatIdentifier =>
              DepLambda[NatToNatKind](n2n, te)(
                DepFunType[NatToNatKind, Type](n2n, te.t)
              )
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

        case TypeAnnotation(e, t) =>
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

    def inferDependent(e: ToBeTyped[Expr]): Expr = this.infer(e match {
      case ToBeTyped(e) => e
    }, Flags.ExplicitDependence.On)

    def infer(e: Expr,
              explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off
             ): Expr = {
      val constraints = mutable.ArrayBuffer[Constraint]()
      val (typed_e, ftvSubs) = constrainTypes(e, constraints, mutable.Map())
      val solution = Constraint.solve(constraints, Seq())(explDep) match {
        case Solution(ts, ns, as, n2ds, n2ns, natColls) =>
          Solution(
            ts.mapValues(t => ftvSubs(t)),
            ns.mapValues(n => ftvSubs(n)),
            as.mapValues(a => ftvSubs(a)),
            n2ds.mapValues(n2d => ftvSubs(n2d)),
            n2ns.mapValues(n2n => ftvSubs(n2n)),
            natColls.mapValues(ftvSubs(_))
          )
      }
      traversal.DepthFirstLocalResult(typed_e, Visitor(solution))
    }
  }

  def identifier(name: String): ToBeTyped[Identifier] = toBeTyped(Identifier(name)())
  def lambda(x: ToBeTyped[Identifier], e: ToBeTyped[Expr]): ToBeTyped[Lambda] =
    x >>= (x => e >>= (e => toBeTyped(Lambda(x, e)())))
  def app(f: ToBeTyped[Expr], e: ToBeTyped[Expr]): ToBeTyped[App] =
    f >>= (f => e >>= (e => toBeTyped(App(f, e)())))
  def depLambda[K <: Kind: KindName](
      x: K#I with Kind.Explicitness,
      e: ToBeTyped[Expr]
  ): ToBeTyped[DepLambda[K]] =
    e >>= (e => toBeTyped(DepLambda[K](x, e)()))
  def depApp[K <: Kind](f: ToBeTyped[Expr], x: K#T): ToBeTyped[DepApp[K]] =
    f >>= (f => toBeTyped(DepApp[K](f, x)()))
  def literal(d: semantics.Data): ToBeTyped[Literal] = toBeTyped(Literal(d))

  def array(n: Int): ToBeTyped[Primitive] = primitives.makeArray(n).apply
  def store(cont: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(e))(fun(cont)))
  def store(how: ToBeTyped[Expr])
           (in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e)))(fun(in)))
  def store2(how: ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e)))(fun(x => x)))

  implicit class Ops(lhs: ToBeTyped[Expr]) {

    // binary
    def +(rhs: ToBeTyped[Expr]): ToBeTyped[App] = add(lhs)(rhs)
    def -(rhs: ToBeTyped[Expr]): ToBeTyped[App] = sub(lhs)(rhs)
    def *(rhs: ToBeTyped[Expr]): ToBeTyped[App] = mul(lhs)(rhs)
    def /(rhs: ToBeTyped[Expr]): ToBeTyped[App] = div(lhs)(rhs)
    def %(rhs: ToBeTyped[Expr]): ToBeTyped[App] = mod(lhs)(rhs)
    def >(rhs: ToBeTyped[Expr]): ToBeTyped[App] = gt(lhs)(rhs)
    def <(rhs: ToBeTyped[Expr]): ToBeTyped[App] = lt(lhs)(rhs)
    def =:=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = equal(lhs)(rhs)

    // scalastyle:off disallow.space.before.token
    // unary
    def unary_- : ToBeTyped[App] = neg(lhs)
    // scalastyle:on disallow.space.before.token

    // pair accesses
    def _1: ToBeTyped[App] = fst(lhs)
    def _2: ToBeTyped[App] = snd(lhs)
  }

  implicit class Indexing(e: ToBeTyped[Expr]) {
    def `@`(i: ToBeTyped[Expr]): ToBeTyped[App] = idx(i)(e)
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
  implicit class TypeAnnotationHelper(t: Type) {
    def ::[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] =
      e >>= (e => toBeTyped(TypeAnnotation(e, t)))
    def `:`[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] = e :: t
  }

  implicit class FunCall(f: ToBeTyped[Expr]) {

    def apply(e: ToBeTyped[Expr]): ToBeTyped[App] = app(f, e)
    def apply(n: Nat): ToBeTyped[DepApp[NatKind]] = depApp[NatKind](f, n)
    def apply(dt: DataType): ToBeTyped[DepApp[DataKind]] = depApp[DataKind](f, dt)
    def apply(a: AddressSpace): ToBeTyped[DepApp[AddressSpaceKind]] =
      depApp[AddressSpaceKind](f, a)

    def apply(n2n: NatToNat): ToBeTyped[DepApp[NatToNatKind]] =
      depApp[NatToNatKind](f, n2n)

    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr]): ToBeTyped[App] = {
      f(e1)(e2)
    }

    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr], e3: ToBeTyped[Expr]): ToBeTyped[App] = {
      f(e1)(e2)(e3)
    }

    def apply(
               e1: ToBeTyped[Expr],
               e2: ToBeTyped[Expr],
               e3: ToBeTyped[Expr],
               e4: ToBeTyped[Expr]
    ): ToBeTyped[App] = {
      f(e1)(e2)(e3)(e4)
    }

    def apply(
               e1: ToBeTyped[Expr],
               e2: ToBeTyped[Expr],
               e3: ToBeTyped[Expr],
               e4: ToBeTyped[Expr],
               e5: ToBeTyped[Expr]
    ): ToBeTyped[App] = {
      f(e1)(e2)(e3)(e4)(e5)
    }
  }

  implicit class FunPipe(e: ToBeTyped[Expr]) {
    def |>(f: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunPipeReverse(f: ToBeTyped[Expr]) {
    def $(e: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunPipeReversePrimitiveBuilde(f: Builder) {
    def $(e: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunComp(f: ToBeTyped[Expr]) {
    def >>(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => g(f(x)))
  }

  implicit class FunCompPrimitiveBuilder(f: Builder) {
    def >>(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => g(f.apply(x)))
  }

  implicit class FunCompReverse(f: ToBeTyped[Expr]) {
    def o(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => f(g(x)))
  }

  implicit class FunCompReversePrimitiveBuilder(f: Builder) {
    def o(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => f.apply(g(x)))
  }

  // function values
  object fun {
    def apply(t: Type)(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Lambda] = {
      val x = identifier(freshName("e")) >>= (i => TypedDSL.toBeTyped(i.setType(t)))
      lambda(x, f(x))
    }

    def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Lambda] = untyped(f)
    def apply(
        f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = untyped(f)
    def apply(
        f: (ToBeTyped[Identifier], ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = untyped(f)
    def apply(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = untyped(f)
    def apply(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = untyped(f)
    def apply(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = untyped(f)

    private def untyped(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    private def untyped(
        f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(
        f: (ToBeTyped[Identifier], ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(
        f: (
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier],
            ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
    ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    // noinspection TypeAnnotation
    // scalastyle:off structural.type
    def apply(ft: FunType[Type, Type]): Object {
      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                    ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Expr]
    } = new {
      def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Expr] =
        untyped(f) :: ft
      def apply(
          f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
      ): ToBeTyped[Expr] = untyped(f) :: ft
      def apply(
          f: (
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier]
          ) => ToBeTyped[Expr]
      ): ToBeTyped[Expr] = untyped(f) :: ft
      def apply(
          f: (
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier]
          ) => ToBeTyped[Expr]
      ): ToBeTyped[Expr] = untyped(f) :: ft
      def apply(
          f: (
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier]
          ) => ToBeTyped[Expr]
      ): ToBeTyped[Expr] = untyped(f) :: ft
      def apply(
          f: (
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier],
              ToBeTyped[Identifier]
          ) => ToBeTyped[Expr]
      ): ToBeTyped[Expr] = untyped(f) :: ft
    }
    // scalastyle:on structural.type
  }

  // noinspection DuplicatedCode
  object nFun {
    def apply(
        r: arithexpr.arithmetic.Range,
        f: NatIdentifier => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[NatKind]] = {
      val x = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => ToBeTyped[Expr]): ToBeTyped[DepLambda[NatKind]] = {
      nFun(arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1), f)
    }

    def apply(
        f: (NatIdentifier, NatIdentifier) => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun(f(n, _)))
    }

    def apply(
        f: (NatIdentifier, NatIdentifier, NatIdentifier) => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2) => f(n, n1, n2)))
    }

    def apply(
        f: (
            NatIdentifier,
            NatIdentifier,
            NatIdentifier,
            NatIdentifier
        ) => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3) => f(n, n1, n2, n3)))
    }

    def apply(
        f: (
            NatIdentifier,
            NatIdentifier,
            NatIdentifier,
            NatIdentifier,
            NatIdentifier
        ) => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3, n4) => f(n, n1, n2, n3, n4)))
    }
  }

  object dtFun {
    def apply(
        f: DataTypeIdentifier => ToBeTyped[Expr]
    ): ToBeTyped[DepLambda[DataKind]] = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      depLambda[DataKind](x, f(x))
    }
  }

  object letf {
    def apply(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(fun(in)))
    }
    def apply(in: ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(in))
    }
  }

  implicit def wrapInNatExpr(n: Nat): ToBeTyped[Literal] = literal(NatData(n))

  def l(i: Int): ToBeTyped[Literal] = literal(IntData(i))
  def l(f: Float): ToBeTyped[Literal] = literal(FloatData(f))
  def l(d: Double): ToBeTyped[Literal] = literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): ToBeTyped[Literal] = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): ToBeTyped[Literal] = literal(VectorData(v))
  def larr(a: Seq[Data]): ToBeTyped[Literal] = literal(ArrayData(a))

  object foreignFun {
    def apply(name: String, t: Type): ToBeTyped[ForeignFunction] = {
      toBeTyped(ForeignFunction(ForeignFunction.Decl(name, None))(t))
    }

    def apply(
        name: String,
        params: Seq[String],
        body: String,
        t: Type
    ): ToBeTyped[ForeignFunction] = {
      toBeTyped(
        ForeignFunction(
          ForeignFunction.Decl(name, Some(ForeignFunction.Def(params, body)))
        )(t)
      )
    }
  }
}
// scalastyle:on number.of.methods