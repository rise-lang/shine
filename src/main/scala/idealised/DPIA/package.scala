package idealised

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{PhraseTypeParser, _}
import lift.arithmetic._
import lift.core

import scala.language.{implicitConversions, reflectiveCalls}

package object DPIA {

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new Exception(msg)
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with Kind.Identifier

  implicit def surfaceToDPINatIdentifier(n: SurfaceLanguage.NatIdentifier): NatIdentifier = NatIdentifier(n.name, n.range)
  implicit def liftToDPIANatIdentifer(n: lift.core.NatIdentifier): NatIdentifier = NatIdentifier(n.name, n.range)

  object NatIdentifier {
    def apply(name: String): NatIdentifier = new NamedVar(name) with Kind.Identifier
    def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range) with Kind.Identifier
  }

  type NatDependentLambda[T <: PhraseType] = DepLambda[NatKind, T]
  object NatDependentLambda {
    def apply[T <: PhraseType](x: NatIdentifier, body: Phrase[T]): NatDependentLambda[T] = DepLambda[NatKind, T](x, body)
  }

  type NatDependentApply[T <: PhraseType] = DepApply[NatKind, T]
  object NatDependentApply {
    def apply[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: Nat): NatDependentApply[T] = DepApply[NatKind, T](fun, arg)
  }

  type TypeDependentLambda[T <: PhraseType] = DepLambda[DataKind, T]
  object TypeDependentLambda {
    def apply[T <: PhraseType](x: DataTypeIdentifier, body: Phrase[T]): TypeDependentLambda[T] = DepLambda[DataKind, T](x, body)
  }

  type TypeDependentApply[T <: PhraseType] = DepApply[DataKind, T]
  object TypeDependentApply {
    def  apply[T <: PhraseType](fun: Phrase[`(dt)->`[T]], arg: DataType): TypeDependentApply[T] = DepApply[DataKind, T](fun, arg)
  }

  case class NatNatTypeFunction private (x:NatIdentifier, body:Nat) {
    //NatNatTypeFunction have an interesting comparison behavior, as we do not define
    //equality for them as simple syntactic equality: we just want to make sure their bodies
    //are equal up-to renaming of the binder.

    //However, just updating equals is not sufficient, as many data structures, such as HashMaps,
    //use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
    //the identifier variable, and just take the hash of the body evaluated at a known point
    override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()

    def apply(n: Nat): Nat = ArithExpr.substitute(body, Map((x, n)))

    override def toString: String = s"($x:nat) -> $body"

    override def equals(obj: Any): Boolean = {
      obj match {
        case other:NatNatTypeFunction => body == other(x)
        case _ => false
      }
    }
  }

  object NatNatTypeFunction {
    def apply(upperBound:Nat, f:NatIdentifier => Nat):NatNatTypeFunction = {
      val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
      NatNatTypeFunction(x, f(x))
    }

    def apply(upperBound:Nat, id:NatIdentifier, body:Nat):NatNatTypeFunction = {
      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
      NatNatTypeFunction(x, x => ArithExpr.substitute(body, Map((id, x))))
    }
  }

  case class NatDataTypeFunction private (x:NatIdentifier, body:DataType) {
    //See hash code of NatNatTypeFunction
    override def hashCode(): Int = this(NamedVar("ComparisonDummy")).hashCode()

    def apply(n:Nat):DataType = DataType.substitute(n, `for`=x, `in`=body)

    override def toString: String = s"($x:nat) -> $body"

    override def equals(obj: Any): Boolean = {
      obj match {
        case other:NatDataTypeFunction =>
          val subbedOther = other(x)
          val eq = body == subbedOther
          eq
        case _ => false
      }
    }
  }

  object NatDataTypeFunction {
    def apply(upperBound:Nat, f:NatIdentifier => DataType):NatDataTypeFunction = {
      val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
      NatDataTypeFunction(x, f(x))
    }

    def apply(upperBound:Nat, id:NatIdentifier, body:DataType):NatDataTypeFunction = {
      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
      NatDataTypeFunction(x, x => DataType.substitute(x, `for`=id, `in`=body))
    }
  }

  object Nat {
    def substitute(ae: Nat, `for`: NatIdentifier, in: Nat): Nat = {
      in.visitAndRebuild {
        case v: Var =>
          if (`for`.name == v.name) {
            ae
          } else {
            v
          }
        case e => e
      }
    }
  }

  // note: this is an easy fix to avoid name conflicts between lift and dpia
  val freshName: core.freshName.type = lift.core.freshName

  type x[T1 <: PhraseType, T2 <: PhraseType] = PairType[T1, T2]
  type ->[T1 <: PhraseType, T2 <: PhraseType] = FunctionType[T1, T2]
  type `->p`[T1 <: PhraseType, T2 <: PhraseType] = PassiveFunctionType[T1, T2]
  type `()->`[K <: Kind, T <: PhraseType] = DependentFunctionType[K, T]
  type `(nat)->`[T <: PhraseType] = NatDependentFunctionType[T]
  type `(dt)->`[T <: PhraseType] = TypeDependentFunctionType[T]
  type VarType = ExpType x AccType

  object VarType {
    def apply(dt: DataType): PairType[ExpType, AccType] = ExpType(dt, read) x AccType(dt)
  }

  //noinspection TypeAnnotation
  implicit class PhraseTypeSubstitutionHelper[T <: PhraseType](t: PhraseType) {
    def `[`(e: Nat) = new {
      def `/`(a: Nat) = new {
        def `]`: PhraseType = PhraseType.substitute(e, `for`=a, in=t)
      }
    }

    def `[`(e: DataType) = new {
      def `/`(a: DataType) = new {
        def `]`: PhraseType = PhraseType.substitute(e, `for`=a, in=t)
      }
    }
  }

  //noinspection TypeAnnotation
  implicit class PhraseSubstitutionHelper[T1 <: PhraseType](in: Phrase[T1]) {
    def `[`[T2 <: PhraseType](p: Phrase[T2]) = new {
      def `/`(`for`: Phrase[T2]) = new {
        def `]`: Phrase[T1] = Phrase.substitute(p, `for`, in)
      }
    }

    def `[`(e: Nat) = new {
      def `/`(`for`: NatIdentifier) = new {
        def `]`: Phrase[T1] = PhraseType.substitute(e, `for`, in)
      }
    }

    def `[`(dt: DataType) = new {
      def `/`(`for`: DataTypeIdentifier) = new {
        def `]`: Phrase[T1] = PhraseType.substitute(dt, `for`, in)
      }
    }
  }

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }

  implicit class DependentFunctionTypeConstructor[K <: Kind](x: K#I) {
    def `()->`[T <: PhraseType](outT: T): K `()->` T = DependentFunctionType[K, T](x, outT)
  }

  implicit class NatDependentFunctionTypeConstructor(x: NatIdentifier) {
    def ->[T <: PhraseType](outT: T): `()->`[NatKind, T] = DependentFunctionType[NatKind, T](x, outT)
  }

  implicit class TypeDependentFunctionTypeConstructor(x: DataTypeIdentifier) {
    def ->[T <: PhraseType](outT: T) = TypeDependentFunctionType(x, outT)
  }

  implicit class PhraseTypeHelper(val sc: StringContext) extends AnyVal {
    def t(args: Any*): PhraseType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts, args.iterator).parsePhraseType
    }

    def exp(args: Any*): ExpType = {
      new PhraseTypeParser("exp" + sc.s(args:_*), sc.parts, args.iterator).parseExpType
    }

    def acc(args: Any*): AccType = {
      new PhraseTypeParser("acc" + sc.s(args:_*), sc.parts, args.iterator).parseAccType
    }

    def dt(args: Any*): DataType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts, args.iterator).parseWrappedDataType
    }
  }

}
