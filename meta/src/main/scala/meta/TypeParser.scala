package meta

import fastparse.ScalaWhitespace._
import fastparse._
import meta.NatParser._

object TypeParser {

  sealed trait TypeAST
  object TypeAST {
    case class Identifier(name: String) extends TypeAST
    case class FunType(inT: TypeAST, outT: TypeAST) extends TypeAST
    case class DepFunType(id: Identifier, kind: String, t: TypeAST) extends TypeAST
    case class ImplicitDepFunType(id: Identifier, kind: String, t: TypeAST) extends TypeAST

    case class ScalarType(t: String) extends TypeAST
    case object NatType extends TypeAST
    case class VectorType(size: NatAST, elemType: TypeAST) extends TypeAST
    case class IndexType(size: NatAST) extends TypeAST
    case class PairType(lhs: TypeAST, rhs: TypeAST) extends TypeAST
    case class DepPairType(id: Identifier, kind: String, t: TypeAST) extends TypeAST
    case class NatToDataApply(f: TypeAST, n: NatAST) extends TypeAST
    case class NatToDataLambda(id: Identifier, t: TypeAST) extends TypeAST
    case class ArrayType(size: NatAST, elemType: TypeAST) extends TypeAST
    case class DepArrayType(size: NatAST, fdt: TypeAST) extends TypeAST

    object Kind extends Enumeration {
      val Data, Nat, Nat2Nat, Nat2Data, Address, Function = Value

      def fromString(s: String): Value = s match {
        case "data" => Data
        case "nat" => Nat
        case "nat2nat" => Nat2Nat
        case "nat2data" => Nat2Data
        case "address" => Address
      }
    }
  }

  def PrimitiveDeclarations[_: P]: P[Seq[(String, Option[(Int, Int)], TypeAST)]] =
    P( Start ~ PrimitiveDeclaration.rep(1) ~ End )

  def ScalaFunArg[_: P]: P[Unit] =
    P( scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type )
  import scalaparse.Scala.TrailingCommaOps
  def ScalaFunArgs[_: P]: P[(Int, Int)] =
    P( "(" ~ Index ~ ScalaFunArg.repTC(1) ~ Index  ~ ")" )

  def PrimitiveDeclaration[_: P]: P[(String, Option[(Int, Int)], TypeAST)] =
    P( "def" ~ Identifier ~ ScalaFunArgs.? ~ ":" ~ TypeSignature)

  case class NamedPredicate(f: Char => Boolean)
                           (implicit name: sourcecode.Name) extends (Char => Boolean){
    def apply(t: Char): Boolean = f(t)
    override def toString(): String = name.value
  }

  def Lower[_: P]: P[String] = {
    val LowerChar = NamedPredicate(CharPredicates.isLower)
    P( CharPred(LowerChar).! )
  }
  def IdRest[_: P]: P[String] = {
    val IdCharacter = NamedPredicate(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c))
    P( CharsWhile(IdCharacter).! )
  }

  def Identifier[_: P]: P[String] = {
    P( (!Keywords ~ Lower ~~ IdRest.?).map{
      case (prefix, Some(suffix)) => prefix ++ suffix
      case (string, None) => string
    } )
  }

  def Keywords[_: P]: P[Unit] =
    P(StringIn(
      "data", "address", "nat2nat", "nat2data", "nat", "natType", "def",
      "bool", "int", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64"
    ) ~~ CharPred(_.isWhitespace))

  def TypeSignature[_: P]: P[TypeAST] = P( DepFunType | ImplicitDepFunType | FunType | LeftTypeSignature )

  def FunType[_: P]: P[TypeAST.FunType] =
    P( NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature ).map(TypeAST.FunType.tupled)

  def DepFunType[_: P]: P[TypeAST.DepFunType] =
    P( "(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature ).map(TypeAST.DepFunType.tupled)

  def ImplicitDepFunType[_: P]: P[TypeAST.ImplicitDepFunType] =
    P( "{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature ).map(TypeAST.ImplicitDepFunType.tupled)

  def IdentifierKindPair[_: P]: P[(TypeAST.Identifier, String)] = P( TypeIdentifier ~ ":" ~ Kind )

  def TypeIdentifier[_: P]: P[TypeAST.Identifier] = P( Identifier ).map(TypeAST.Identifier)

  def Kind[_: P]: P[String] = P( "data".! | "address".! | "nat2nat".! | "nat2data".! | "nat".! )

  // Types that can appear at the left of an function arrow
  def LeftTypeSignature[_: P]: P[TypeAST] = P( DataType | ("(" ~ TypeSignature ~ ")") )

  def DataType[_: P]: P[TypeAST] = P( PairType | LeftDataType )

  def PairType[_: P]: P[TypeAST.PairType] =
    P( "(" ~ NoCut(LeftDataType) ~ "," ~/ DataType ~ ")" ).map(TypeAST.PairType.tupled)

  def LeftDataType[_: P]: P[TypeAST] =
    P(  ScalarType | NatType | IndexType | VectorType | DepArrayType |
        ArrayType | DepPairType | NatToDataApply | TypeIdentifier | ("(" ~ DataType ~ ")") )

  def ScalarType[_: P]: P[TypeAST.ScalarType] =
    P(  "bool".! | "int".! |
        "i8".! | "i16".! | "i32".! | "i64".! |
        "u8".! | "u16".! | "u32".! | "u64".! |
        "f16".! | "f32".! | "f64".! ).map(TypeAST.ScalarType)

  def NatType[_: P]: P[TypeAST.NatType.type] = P( "natType" ).map(_ => TypeAST.NatType)

  def IndexType[_: P]: P[TypeAST.IndexType] = P( "idx[" ~ Nat ~ "]" ).map(TypeAST.IndexType)

  def VectorType[_: P]: P[TypeAST.VectorType] = P( "vec[" ~ DataType ~ "," ~ Nat ~ "]"  ).map(t => TypeAST.VectorType(t._2, t._1))

  def DepArrayType[_: P]: P[TypeAST.DepArrayType] =
    P( Nat ~ ".." ~/ NatToData ).map(TypeAST.DepArrayType.tupled)

  def ArrayType[_: P]: P[TypeAST.ArrayType] =
    P( Nat ~ "." ~~ !"." ~/ DataType ).map(TypeAST.ArrayType.tupled)

  def DepPairType[_: P]: P[TypeAST.DepPairType] =
    P( "(" ~ IdentifierKindPair ~ "**" ~/ DataType ~ ")" ).map(TypeAST.DepPairType.tupled)

  def NatToDataApply[_: P]: P[TypeAST.NatToDataApply] =
    P( NatToData ~ "(" ~ Nat ~ ")" ).map(TypeAST.NatToDataApply.tupled)

  def NatToData[_: P]: P[TypeAST] = P( TypeIdentifier | NatToDataLambda )

  def NatToDataLambda[_: P]: P[TypeAST.NatToDataLambda] =
    P( "(" ~ IdentifierKindPair.filter(_._2 == "nat").map(_._1) ~ "|->" ~/ DataType ~ ")" ).
      map(TypeAST.NatToDataLambda.tupled)

  import TypeAST.Kind._

  def isWellKindedType(typeAST: TypeAST): Boolean = {
    kindOf(typeAST, Map.empty).isDefined
  }

  def kindOf(typeAST: TypeAST,
             env: Map[TypeAST.Identifier, TypeAST.Kind.Value]): Option[TypeAST.Kind.Value] = {
    typeAST match {
      case id: TypeAST.Identifier =>
        env.get(id)
      case TypeAST.FunType(inT, outT) =>
        for {
          _ <- kindOf(inT, env)
          _ <- kindOf(outT, env)
        } yield Function
      case TypeAST.DepFunType(id, kind, t) =>
        if (env.isDefinedAt(id)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id, TypeAST.Kind.fromString(kind)))
        }
      case TypeAST.ImplicitDepFunType(id, kind, t) =>
        if (env.isDefinedAt(id)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id, TypeAST.Kind.fromString(kind)))
        }
      case TypeAST.VectorType(size, elemType) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(elemType, env)
          if k1 == Nat && k2 == Data
        } yield Data
      case TypeAST.IndexType(size) =>
        for {
          k <- kindOf(size, env)
          if k == Nat
        } yield Data
      case TypeAST.PairType(lhs, rhs) =>
        for {
          k1 <- kindOf(lhs, env)
          k2 <- kindOf(rhs, env)
          if k1 == Data && k2 == Data
        } yield Data
      case TypeAST.DepPairType(id, kind, t) =>
        if (env.isDefinedAt(id)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id, TypeAST.Kind.fromString(kind)))
        }
      case TypeAST.NatToDataApply(f, n) =>
        for {
          k1 <- kindOf(f, env)
          k2 <- kindOf(n, env)
          if k1 == Nat2Data && k2 == Nat
        } yield Data
      case TypeAST.NatToDataLambda(id, t) =>
        if (env.isDefinedAt(id)) {
          None // we forbid shadowing
        } else {
          for {
            k <- kindOf(t, env.updated(id, Nat))
            if k == Data
          } yield Nat2Data
        }
      case TypeAST.ArrayType(size, elemType) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(elemType, env)
          if k1 == Nat && k2 == Data
        } yield Data
      case TypeAST.DepArrayType(size, fdt) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(fdt, env)
          if k1 == Nat && k2 == Nat2Data
        } yield Data
      case _: TypeAST.ScalarType | TypeAST.NatType =>
        Some(Data)
    }
  }

  def kindOf(natAST: NatAST,
             env: Map[TypeAST.Identifier, TypeAST.Kind.Value]): Option[TypeAST.Kind.Value] = {
    natAST match {
      case NatAST.Identifier(id) =>
        env.get(id)
      case NatAST.Number(n) =>
        Some(Nat)
      case NatAST.BinaryOp(lhs, _, rhs) =>
        for {
          k1 <- kindOf(lhs, env)
          k2 <- kindOf(rhs, env)
          if k1 == Nat && k2 == Nat
        } yield Nat

      case NatAST.TernaryOp(_, thenN, elseN) =>
        for {
          k1 <- kindOf(thenN, env)
          k2 <- kindOf(elseN, env)
          if k1 == Nat && k2 == Nat
        } yield Nat

      case NatAST.Nat2NatApply(f, n) =>
        for {
          k1 <- kindOf(f, env)
          k2 <- kindOf(n, env)
          if k1 == Nat2Nat && k2 == Nat
        } yield Nat

      case NatAST.Sum(id, from, upTo, body) =>
        val nEnv = env.updated(id.id, Nat)
        for {
          k1 <- kindOf(from, nEnv)
          k2 <- kindOf(upTo, nEnv)
          k3 <- kindOf(body, nEnv)
          if k1 == Nat && k2 == Nat && k3 == Nat
        } yield Nat
    }
  }
}
