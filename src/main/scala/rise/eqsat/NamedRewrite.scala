package rise.eqsat

import rise.core.types.TypePlaceholder
import rise.{core => rc}
import rise.core.{types => rct}
import rise.core.{primitives => rcp}
import shine.Pipe

object NamedRewrite {
  def init(name: String,
           rule: (NamedRewriteDSL.Pattern, NamedRewriteDSL.Pattern)
          ): Rewrite[DefaultAnalysisData] = {
    import rise.core.DSL.infer.{preservingWithEnv, collectFreeEnv}
    import arithexpr.{arithmetic => ae}

    val (lhs, rhs) = rule
    val untypedFreeV = collectFreeEnv(lhs).map { case (name, t) =>
      assert(t == rct.TypePlaceholder)
      name -> rct.TypeIdentifier("t" + name)
    }
    val typedLhs = preservingWithEnv(lhs, untypedFreeV, Set())
    val freeV = collectFreeEnv(typedLhs)
    val (_, freeT) = rise.core.IsClosedForm.freeVars(typedLhs)
    if (name == "drop-before-map") {
      print("x")
    }
    val typedRhs = preservingWithEnv(rc.TypeAnnotation(rhs, typedLhs.t), freeV, freeT)

    // from var name to var index and whether it is matched
    // (depending on local index shift)
    type PatternVarMap[S, V] = HashMap[String, HashMap[S, (V, Boolean)]]
    type ExprShift = (Int, Int, Int) // Expr, Nat, Data
    type NatShift = Int // Nat
    type TypeShift = (Int, Int) // Nat, Data
    val patVars: PatternVarMap[ExprShift, PatternVar] = HashMap()
    val natPatVars: PatternVarMap[NatShift, NatPatternVar] = HashMap()
    val dataTypePatVars: PatternVarMap[TypeShift, DataTypePatternVar] = HashMap()
    val typePatVars: PatternVarMap[TypeShift, TypePatternVar] = HashMap()

    def makePatVar[S, V](name: String,
                         shift: S,
                         pvm: PatternVarMap[S, V],
                         constructor: Int => V,
                         isRhs: Boolean): V = {
      val shiftMap = pvm.getOrElseUpdate(name, HashMap())
      val (pv, matched) = shiftMap.getOrElseUpdate(shift, {
        val pvCount = pvm.values.map(m => m.size).sum
        (constructor(pvCount), false)
      })
      shiftMap(shift) = (pv, matched || !isRhs)
      pv
    }

    def makePat(expr: rc.Expr, bound: Expr.Bound, isRhs: Boolean): Pattern =
      Pattern(expr match {
        case i: rc.Identifier if freeV.contains(i.name) =>
          makePatVar(i.name,
            (bound.expr.size, bound.nat.size, bound.data.size),
            patVars, PatternVar, isRhs)
        case i: rc.Identifier => PatternNode(Var(bound.indexOf(i)))
        case rc.Lambda(x, e) =>
          PatternNode(Lambda(makePat(e, bound + x, isRhs)))
        case rc.App(f, e) =>
          PatternNode(App(makePat(f, bound, isRhs), makePat(e, bound, isRhs)))
        case rc.DepLambda(x: rct.NatIdentifier, e) =>
          PatternNode(NatLambda(makePat(e, bound + x, isRhs)))
        case rc.DepLambda(x: rct.DataTypeIdentifier, e) =>
          PatternNode(DataLambda(makePat(e, bound + x, isRhs)))
        case rc.DepLambda(_, _) => ???
        case rc.DepApp(f, x: rct.Nat) =>
          PatternNode(NatApp(makePat(f, bound, isRhs), makeNPat(x, bound, isRhs)))
        case rc.DepApp(f, x: rct.DataType) =>
          PatternNode(DataApp(makePat(f, bound, isRhs), makeDTPat(x, bound, isRhs)))
        case rc.DepApp(_, _) => ???
        case rc.Literal(d) => PatternNode(Literal(d))
        case p: rc.Primitive => PatternNode(Primitive(p))
      }, makeTPat(expr.t, bound, isRhs))

    def makeNPat(n: rct.Nat, bound: Expr.Bound, isRhs: Boolean): NatPattern =
      n match {
        case i: rct.NatIdentifier if freeT(i) =>
          makePatVar(i.name, bound.nat.size, natPatVars, NatPatternVar, isRhs)
        case i: rct.NatIdentifier =>
          NatPatternNode(NatVar(bound.indexOf(i)))
        case ae.Cst(c) =>
          NatPatternNode(NatCst(c))
        case ae.Sum(Nil) => NatPatternNode(NatCst(0))
        case ae.Sum(t +: ts) => ts.foldRight(makeNPat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatAdd(makeNPat(t, bound, isRhs), acc))
        }
        case ae.Prod(Nil) => NatPatternNode(NatCst(1))
        case ae.Prod(t +: ts) => ts.foldRight(makeNPat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatMul(makeNPat(t, bound, isRhs), acc))
        }
        case _ => throw new Exception(s"did not expect $n")
      }

    def makeDTPat(dt: rct.DataType, bound: Expr.Bound, isRhs: Boolean): DataTypePattern =
      dt match {
        case i: rct.DataTypeIdentifier if freeT(i) =>
          makePatVar(i.name, (bound.nat.size, bound.data.size),
            dataTypePatVars, DataTypePatternVar, isRhs)
        case i: rct.DataTypeIdentifier =>
          DataTypePatternNode(DataTypeVar(bound.indexOf(i)))
        case rct.NatType =>
          DataTypePatternNode(NatType)
        case rct.VectorType(s, et) =>
          DataTypePatternNode(VectorType(makeNPat(s, bound, isRhs), makeDTPat(et, bound, isRhs)))
        case rct.IndexType(s) =>
          DataTypePatternNode(IndexType(makeNPat(s, bound, isRhs)))
        case rct.PairType(dt1, dt2) =>
          DataTypePatternNode(PairType(makeDTPat(dt1, bound, isRhs), makeDTPat(dt2, bound, isRhs)))
        case rct.ArrayType(s, et) =>
          DataTypePatternNode(ArrayType(makeNPat(s, bound, isRhs), makeDTPat(et, bound, isRhs)))
        case _: rct.DepArrayType | _: rct.DepPairType[_] |
             _: rct.NatToDataApply | _: rct.FragmentType =>
          throw new Exception(s"did not expect $dt")
      }

    def makeTPat(t: rct.Type, bound: Expr.Bound, isRhs: Boolean): TypePattern =
      t match {
        case dt: rct.DataType => makeDTPat(dt, bound, isRhs)
        case rct.FunType(a, b) =>
          TypePatternNode(FunType(makeTPat(a, bound, isRhs), makeTPat(b, bound, isRhs)))
        case rct.DepFunType(x: rct.NatIdentifier, t) =>
          TypePatternNode(NatFunType(makeTPat(t, bound + x, isRhs)))
        case rct.DepFunType(x: rct.DataTypeIdentifier, t) =>
          TypePatternNode(DataFunType(makeTPat(t, bound + x, isRhs)))
        case rct.DepFunType(_, _) => ???
        case i: rct.TypeIdentifier =>
          assert(freeT(i))
          makePatVar(i.name, (bound.nat.size, bound.data.size),
            typePatVars, TypePatternVar, isRhs)
        case rct.TypePlaceholder =>
          throw new Exception(s"did not expect $t, something was not infered")
      }

    val lhsPat = makePat(typedLhs, Expr.Bound.empty, isRhs = false)
    val rhsPat = makePat(typedRhs, Expr.Bound.empty, isRhs = true)

    // TODO: deal with different shifts on lhs
    def getLhsShifts[S, V](pvm: PatternVarMap[S, V]): HashMap[String, (S, V)] = {
      pvm.map { case (name, shiftMap) =>
        val lhs = shiftMap.toSeq.flatMap { case (s, (v, matched)) =>
          if (matched) Some((s, v)) else None
        }
        assert(lhs.size == 1)
        name -> lhs(0)
      }
    }

    type Applier = rise.eqsat.Applier[DefaultAnalysisData]
    def shiftAppliers[S, V](pvm: PatternVarMap[S, V],
                            mkShift: (S, V) => (S, V) => Applier => Applier
                           ): Applier => Applier = {
      val bases = getLhsShifts(pvm)
      pvm.foldRight { a: Applier => a } { case ((name, shiftMap), acc) =>
        val base = bases(name)
        shiftMap.foldRight(acc) {
          case ((_, (_, true)), acc) => acc
          case ((shift, (pv, false)), acc) =>
            a: Applier => mkShift.tupled(base)(shift, pv)(acc(a))
        }
      }
    }

    def patMkShift(s1: ExprShift, pv1: PatternVar)
                  (s2: ExprShift, pv2: PatternVar)
                  (applier: Applier): Applier = {
      assert(s1 != s2)
      if (s1._2 != s2._2 || s1._3 != s2._3) {
        throw new Exception("TOOD: implement nat/type shift for exprs")
      }
      if (s1._1 != s2._1) {
        val cutoff = s1._1
        val shift = s2._2 - s1._1
        ShiftedApplier(pv1, pv2, shift, cutoff, applier)
      } else {
        applier
      }
    }

    def natPatMkShift(s1: NatShift, pv1: NatPatternVar)
                     (s2: NatShift, pv2: NatPatternVar)
                     (applier: Applier): Applier = {
      assert(s1 != s2)
      throw new Exception("TOOD: implement nat/type shift for nats")
    }

    def dataTypePatMkShift(s1: TypeShift, pv1: DataTypePatternVar)
                          (s2: TypeShift, pv2: DataTypePatternVar)
                          (applier: Applier): Applier = {
      assert(s1 != s2)
      throw new Exception("TOOD: implement nat/type shift for datatypes")
    }

    def typePatMkShift(s1: TypeShift, pv1: TypePatternVar)
                      (s2: TypeShift, pv2: TypePatternVar)
                      (applier: Applier): Applier = {
      assert(s1 != s2)
      throw new Exception("TOOD: implement nat/type shift for types")
    }

    Rewrite.init(name,
      CompiledPattern.patternToSearcher(lhsPat.compile()) -> (
        Pattern.patternToApplier[DefaultAnalysisData](rhsPat) |>
        shiftAppliers(patVars, patMkShift) |>
        shiftAppliers(natPatVars, natPatMkShift) |>
        shiftAppliers(dataTypePatVars, dataTypePatMkShift) |>
        shiftAppliers(typePatVars, typePatMkShift)))
  }
}

object NamedRewriteDSL {
  type Pattern = rc.Expr
  type NatPattern = rct.Nat
  type DataTypePattern = rct.DataType
  type TypePattern = rct.Type

  import scala.language.implicitConversions

  val `_`: rct.TypePlaceholder.type = rct.TypePlaceholder

  implicit final class RewriteArrow(private val lhs: Pattern) extends AnyVal {
    @inline def -->(rhs: Pattern): (Pattern, Pattern) = lhs -> rhs
  }

  implicit def stringAsIdentifier(name: String): Pattern =
    rc.Identifier(name)(TypePlaceholder)
  def app(a: Pattern, b: Pattern): Pattern =
    rc.App(a, b)(TypePlaceholder)
  def lam(name: String, e: Pattern): Pattern =
    rc.Lambda(rc.Identifier(name)(TypePlaceholder), e)(TypePlaceholder)
  def nApp(f: Pattern, x: NatPattern): Pattern =
    rc.DepApp[rct.NatKind](f, x)(TypePlaceholder)
  def nLam(name: String, e: Pattern): Pattern = {
    val n = rct.NatIdentifier(name, isExplicit = true)
    rc.DepLambda[rct.NatKind](n, e)(TypePlaceholder)
  }
  def l(d: rc.semantics.Data): Pattern = rc.Literal(d)

  def slide: Pattern = rcp.slide.primitive
  def map: Pattern = rcp.map.primitive
  def reduce: Pattern = rcp.reduce.primitive
  def transpose: Pattern = rcp.transpose.primitive
  def zip: Pattern = rcp.zip.primitive
  def join: Pattern = rcp.join.primitive
  def fst: Pattern = rcp.fst.primitive
  def snd: Pattern = rcp.snd.primitive
  def add: Pattern = rcp.add.primitive
  def mul: Pattern = rcp.mul.primitive
  def div: Pattern = rcp.div.primitive
  def drop: Pattern = rcp.drop.primitive
  def take: Pattern = rcp.take.primitive

  implicit def placeholderAsNatPattern(p: `_`.type): NatPattern =
    rct.NatIdentifier(rc.freshName("n"), isExplicit = true)
  implicit def stringAsNatPattern(name: String): NatPattern = rct.NatIdentifier(name, isExplicit = true)

  implicit def placeholderAsDataTypePattern(p: `_`.type): DataTypePattern =
    rct.DataTypeIdentifier(rc.freshName("dt"), isExplicit = true)
  implicit def stringAsDataTypePattern(name: String): DataTypePattern =
    rct.DataTypeIdentifier(name, isExplicit = true)

  val int: DataTypePattern = rct.int
  val f32: DataTypePattern = rct.f32

  implicit final class TypeAnnotation(private val t: TypePattern) extends AnyVal {
    @inline def ::(p: Pattern): Pattern = rc.TypeAnnotation(p, t)
  }
  implicit final class FunConstructorT(private val r: TypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern =
      rct.FunType[rct.Type, rct.Type](t, r)
  }
  implicit final class FunConstructorDT(private val r: DataTypePattern) extends AnyVal {
    @inline def ->:(t: TypePattern): TypePattern =
      rct.FunType[rct.Type, rct.Type](t, r)
  }
  implicit final class ArrayConstructor(private val s: NatPattern) extends AnyVal {
    @inline def `.`(et: DataTypePattern): DataTypePattern =
      rct.ArrayType(s, et)
  }
}