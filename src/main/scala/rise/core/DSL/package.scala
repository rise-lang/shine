package rise.core

import util.monads._
import rise.core.primitives._
import rise.core.semantics._
import rise.core.traverse._
import rise.core.types._
import rise.core.types.DataType._

import scala.language.implicitConversions

// scalastyle:off number.of.methods
package object DSL {

  // DSL for Exprs

  def identifier(name: String): ToBeTyped[Identifier] =
    toBeTyped(Identifier(name)(TypePlaceholder))
  def lambda(x: ToBeTyped[Identifier], e: ToBeTyped[Expr]): ToBeTyped[Lambda] =
    x >>= (x => e >>= (e => toBeTyped(Lambda(x, e)(TypePlaceholder))))
  def app(f: ToBeTyped[Expr], e: ToBeTyped[Expr]): ToBeTyped[App] =
    f >>= (f => e >>= (e => toBeTyped(App(f, e)(TypePlaceholder))))
  def depLambda[T, I](kind: Kind[T, I], x: I, e: ToBeTyped[Expr]): ToBeTyped[DepLambda[T, I]] =
    e >>= (e => toBeTyped(DepLambda(kind, x, e)(TypePlaceholder)))
  def depApp[T](kind: Kind[T, _], f: ToBeTyped[Expr], x: T): ToBeTyped[DepApp[T]] =
    f >>= (f => toBeTyped(DepApp(kind, f, x)(TypePlaceholder)))
  def literal(d: semantics.Data): ToBeTyped[Literal] = toBeTyped(Literal(d))

  def store(cont: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(e)) be cont)
  def store(how: ToBeTyped[Expr])
           (in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e))) be in)
  def store2(how: ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e))) be (x => x))

  def toMemFun(f: ToBeTyped[Expr]): ToBeTyped[Expr] = fun(x => toMem(f(x)))

  case class `if`(b: ToBeTyped[Expr]) {
    def `then`(tE: ToBeTyped[Expr]): `if`.`then` = `if`.`then`(b, tE)
  }
  object `if` {
    case class `then`(b: ToBeTyped[Expr], tE: ToBeTyped[Expr]) {
      def `else`(eE: ToBeTyped[Expr]): ToBeTyped[Expr] = {
        select(b)(tE)(eE)
      }
    }
  }

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
    def >=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = not(lhs < rhs)
    def <=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = not(lhs > rhs)

    // scalastyle:off disallow.space.before.token
    // unary
    def unary_- : ToBeTyped[App] = neg(lhs)
    def unary_! : ToBeTyped[App] = not(lhs)
    // scalastyle:on disallow.space.before.token

    // pair accesses
    def `1`: ToBeTyped[App] = fst(lhs)
    def `2`: ToBeTyped[App] = snd(lhs)
  }

  implicit class Indexing(e: ToBeTyped[Expr]) {
    def `@`(i: ToBeTyped[Expr]): ToBeTyped[App] = idx(i)(e)
  }

  implicit class TypeAssertionHelper(t: ExprType) {
    def !:[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] =
      e >>= (e => toBeTyped(TypeAssertion(e, t)))
  }

  implicit class TypeAnnotationHelper(t: ExprType) {
    def ::[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] =
      e >>= (e => toBeTyped(TypeAnnotation(e, t)))
  }

  implicit class FunCall(f: ToBeTyped[Expr]) {
    def apply(e: ToBeTyped[Expr]): ToBeTyped[App] =
      app(f, e)
    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)
    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)
    def apply(e1: ToBeTyped[Expr],
              e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr],
              e4: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)(e4)
    def apply(e1: ToBeTyped[Expr],
              e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr],
              e4: ToBeTyped[Expr],
              e5: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)(e4)(e5)

    def apply(n: Nat): ToBeTyped[DepApp[Nat]] =
      depApp(NatKind, f, n)
    def apply(dt: DataType): ToBeTyped[DepApp[DataType]] =
      depApp(DataKind, f, dt)
    def apply(a: AddressSpace): ToBeTyped[DepApp[AddressSpace]] =
      depApp(AddressSpaceKind, f, a)
    def apply(n2n: NatToNat): ToBeTyped[DepApp[NatToNat]] =
      depApp(NatToNatKind, f, n2n)
    def apply(n2d: NatToData): ToBeTyped[DepApp[NatToData]] =
      depApp(NatToDataKind, f, n2d)
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
    def apply(t: ExprType)
             (f: ToBeTyped[Identifier] => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val x = identifier(freshName("e")) >>= (i => toBeTyped(i.setType(t)))
      lambda(x, f(x))
    }

    def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    def apply(
               f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun(e1 => f(e, e1)))
    }

    def apply(
               f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                 ToBeTyped[Identifier]) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2) => f(e, e1, e2)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    def apply(
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
      lambda(e, fun((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4, e5, e6) => f(e, e1, e2, e3, e4, e5, e6)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4, e5, e6, e7) => f(e, e1, e2, e3, e4, e5, e6, e7)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4, e5, e6, e7, e8) => f(e, e1, e2, e3, e4, e5, e6, e7, e8)))
    }

    def apply(ft: FunType[ExprType, ExprType]): WithFunType = WithFunType(ft)

    case class WithFunType(ft: FunType[ExprType, ExprType]) {
      def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Expr] =
        fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(f: (ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier],
        ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft
    }
  }

  object depFun {
    def apply(r: arithexpr.arithmetic.Range,
              w: NatFunction1Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val n = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n, w.f(n))
    }

    def apply(w: NatFunction1Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n, w.f(n))
    }

    def apply(w: NatFunction2Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n1, depFun((n2: Nat) => w.f(n1, n2)))
    }

    def apply(w: NatFunction3Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n1, depFun((n2: Nat, n3: Nat) => w.f(n1, n2, n3)))
    }

    def apply(w: NatFunction4Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n1, depFun((n2: Nat, n3: Nat, n4: Nat) =>
        w.f(n1, n2, n3, n4)))
    }

    def apply(w: NatFunction5Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[Nat, NatIdentifier]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r)
      depLambda(NatKind, n1, depFun((n2: Nat, n3: Nat, n4: Nat, n5: Nat) =>
        w.f(n1, n2, n3, n4, n5)))
    }

    def apply(w: DataTypeFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[DataType, DataTypeIdentifier]] = {
      val x = DataTypeIdentifier(freshName("dt"))
      depLambda(DataKind, x, w.f(x))
    }

    def apply(w: NatToDataFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatToData, NatToDataIdentifier]] = {
      val x = NatToDataIdentifier(freshName("n2d"))
      depLambda(NatToDataKind, x, w.f(x))
    }

    def apply(w: NatToNatFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatToNat, NatToNatIdentifier]] = {
      val x = NatToNatIdentifier(freshName("n2n"))
      depLambda(NatToNatKind, x, w.f(x))
    }

    def apply(w: AddressSpaceFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[AddressSpace, AddressSpaceIdentifier]] = {
      val x = AddressSpaceIdentifier(freshName("a"))
      depLambda(AddressSpaceKind, x, w.f(x))
    }
  }

  case class  let(e: ToBeTyped[Expr]) {
    def be(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
      primitives.let(e)(fun(in))
    def be(in: ToBeTyped[Expr]): ToBeTyped[Expr] =
      primitives.let(e)(in)
  }

  object letf {
    def apply(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(fun(in)))
    }
    def apply(in: ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(in))
    }

    implicit def toLetf(l: letf.type): ToBeTyped[Expr] =
      fun(e => fun(in => primitives.let(e)(in)))
  }

  case class NatFunction1Wrapper[A](f: Nat => A)
  implicit def toNatFunction1Wrapper[A](f: Nat => A): NatFunction1Wrapper[A] =
    NatFunction1Wrapper(f)

  case class NatFunction2Wrapper[A](f: (Nat, Nat) => A)
  implicit def toNatFunction2Wrapper[A](f: (Nat, Nat) => A
                                       ): NatFunction2Wrapper[A] =
    NatFunction2Wrapper(f)

  case class NatFunction3Wrapper[A](f: (Nat, Nat, Nat) => A)
  implicit def toNatFunction3Wrapper[A](f: (Nat, Nat, Nat) => A
                                       ): NatFunction3Wrapper[A] =
    NatFunction3Wrapper(f)

  case class NatFunction4Wrapper[A](f: (Nat, Nat, Nat, Nat) => A)
  implicit def toNatFunction4Wrapper[A](f: (Nat, Nat, Nat, Nat) => A
                                       ): NatFunction4Wrapper[A] =
    NatFunction4Wrapper(f)

  case class NatFunction5Wrapper[A](f: (Nat, Nat, Nat, Nat, Nat) => A)
  implicit def toNatFunction5Wrapper[A](f: (Nat, Nat, Nat, Nat, Nat) => A
                                       ): NatFunction5Wrapper[A] =
    NatFunction5Wrapper(f)

  case class DataTypeFunctionWrapper[A](f: DataType => A)
  implicit def toDataTypeFunctionWrapper[A](f: DataType => A
                                           ): DataTypeFunctionWrapper[A] =
    DataTypeFunctionWrapper(f)

  case class NatToDataFunctionWrapper[A](f: NatToData => A)
  implicit def toNatToDataFunctionWrapper[A](f: NatToData => A
                                            ): NatToDataFunctionWrapper[A] =
    NatToDataFunctionWrapper(f)

  case class NatToNatFunctionWrapper[A](f: NatToNat => A)
  implicit def toNatToNatFunctionWrapper[A](f: NatToNat => A
                                           ): NatToNatFunctionWrapper[A] =
    NatToNatFunctionWrapper(f)

  case class AddressSpaceFunctionWrapper[A](f: AddressSpace => A)
  implicit def toAddressSpaceFunctionWrapper[A](f: AddressSpace => A
                                               ): AddressSpaceFunctionWrapper[A] =
    AddressSpaceFunctionWrapper(f)

  case class NatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A)
  implicit def toNatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A
                                                ): NatCollectionFunctionWrapper[A] =
    NatCollectionFunctionWrapper(f)

  implicit def wrapInNatExpr(n: Nat): ToBeTyped[Literal] = literal(NatData(n))

  def l(i: Int): ToBeTyped[Literal] = literal(IntData(i))
  def lf16(f: Float): ToBeTyped[Expr] = cast(lf32(f)) :: f16
  def lf32(f: Float): ToBeTyped[Literal] = literal(FloatData(f))
  def lf64(d: Double): ToBeTyped[Literal] = literal(DoubleData(d))
  def l(n: Nat): ToBeTyped[Literal] = literal(NatData(n))
  def lidx(i: Nat, n: Nat): ToBeTyped[Literal] = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): ToBeTyped[Literal] = literal(VectorData(v))
  def larr(a: Seq[Data]): ToBeTyped[Literal] = literal(ArrayData(a))

  def li16(v: Int): ToBeTyped[Expr] = cast(l(v)) :: i16
  def li32(v: Int): ToBeTyped[Expr] = cast(l(v)) :: i32
  def lu8(v: Int): ToBeTyped[Expr] = cast(l(v)) :: u8

  object foreignFun {
    def apply(name: String, t: ExprType): ToBeTyped[Expr] = {
      apply(ForeignFunction.Decl(name, None), t)
    }

    def apply(name: String,
              params: Seq[String],
              body: String,
              t: ExprType
             ): ToBeTyped[Expr] = {
      apply(ForeignFunction.Decl(name, Some(ForeignFunction.Def(params, body))), t)
    }

    def apply(decl: ForeignFunction.Decl, t: ExprType): ToBeTyped[Expr] = {
      def collectTypes(t: ExprType): (Seq[DataType], DataType) = {
        t match {
          case dt: DataType => (Vector(), dt)
          case FunType(dt: DataType, out) =>
            val (i, o) = collectTypes(out)
            (dt +: i, o)
          case _ => throw new Exception("This should not be possible")
        }
      }
      val (inTs, outT) = collectTypes(t)
      val ff: ToBeTyped[Expr] = foreignFunction(decl, inTs.size).apply
      inTs.foldLeft(ff) {
        case (f, t) => f.apply(t)
      }.apply(outT)
    }
  }

  // interface between ToBeTyped and Expr

  implicit def toExpr[T <: Expr](d: ToBeTyped[T]): Expr = d.toExpr

  implicit def preserveType[T <: Expr](e: T): ToBeTyped[Opaque] =
    ToBeTyped(Opaque(e, e.t))

  def toBeTyped[T <: Expr](e: T): ToBeTyped[T] = ToBeTyped(e)

  def topLevel(e: Expr): TopLevel = TopLevel(e)()

  def untypedTopLevel[T <: Expr](d: ToBeTyped[T]): ToBeTyped[TopLevel] =
    toBeTyped(topLevel(toExpr(d)))

  def eraseTypeFromExpr[T <: Expr](e: T): T =
    traverse(e, new PureExprTraversal {
      override def identifier[I <: Identifier] : VarType => I => Pure[I] = vt => i =>
        return_(i.setType(TypePlaceholder).asInstanceOf[I])
      override def expr : Expr => Pure[Expr] = {
        case l : Literal => super.expr(l : Expr)
        case e => super.expr(e.setType(TypePlaceholder))
      }
    }).asInstanceOf[T]

  def eraseType[T <: Expr](e: T): ToBeTyped[T] = toBeTyped(eraseTypeFromExpr(e))
}
// scalastyle:on number.of.methods
