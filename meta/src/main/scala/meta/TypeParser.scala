package meta

import fastparse.ScalaWhitespace._
import fastparse._

import NatParser._

object TypeParser {

  sealed trait TypeAST
  object TypeAST {
    case class TypeIdentifier(name: String) extends TypeAST
    case class FunType(inT: TypeAST, outT: TypeAST) extends TypeAST
    case class DepFunType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST
    case class ImplicitDepFunType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST

    case class ScalarType(t: String) extends TypeAST
    case object NatType extends TypeAST
    case class VectorType(size: NatAST, elemType: TypeAST) extends TypeAST
    case class IndexType(size: NatAST) extends TypeAST
    case class PairType(lhs: TypeAST, rhs: TypeAST) extends TypeAST
    case class DepPairType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST
    case class NatToDataApply(f: TypeAST, n: NatAST) extends TypeAST
    case class NatToDataLambda(id: TypeIdentifier, t: TypeAST) extends TypeAST
    case class ArrayType(size: NatAST, elemType: TypeAST) extends TypeAST
    case class DepArrayType(size: NatAST, fdt: TypeAST) extends TypeAST
  }

  def PrimitiveDeclarations[_: P]: P[Seq[(String, Option[(Int, Int)], TypeAST)]] =
    P( Start ~ PrimitiveDeclaration.rep(1) ~ End )

  def ScalaFunArg[_: P]: P[Unit] =
    P( scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type )
  import scalaparse.Scala.TrailingCommaOps
  def ScalaFunArgs[_: P]: P[(Int, Int)] =
    P( "(" ~ Index ~ ScalaFunArg.repTC(1) ~ Index  ~ ")" )

  def PrimitiveDeclaration[_: P]: P[(String, Option[(Int, Int)], TypeAST)] =
    P( "def" ~ Identifier ~ ScalaFunArgs.? ~ ":" ~ TypeSignature ~ ";")

  def Identifier[_: P]: P[String] =
    P( CharPred(_.isLower).! ~~ (CharIn("0-9") | CharIn("a-z") | CharIn("A-Z")).rep.! ).
      map(t => t._1 ++ t._2).
      filter(!_.contains(' ')).
      filter(!Seq(
        "data", "address", "nat2nat", "nat2data", "nat", "natType", "def",
        "bool", "int", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64"
      ).contains(_))

  def TypeSignature[_: P]: P[TypeAST] = P( FunType | DepFunType | ImplicitDepFunType | LeftTypeSignature )

  def FunType[_: P]: P[TypeAST.FunType] =
    P( NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature ).map(TypeAST.FunType.tupled)

  def DepFunType[_: P]: P[TypeAST.DepFunType] =
    P( "(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature ).map(TypeAST.DepFunType.tupled)

  def ImplicitDepFunType[_: P]: P[TypeAST.ImplicitDepFunType] =
    P( "{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature ).map(TypeAST.ImplicitDepFunType.tupled)

  def IdentifierKindPair[_: P]: P[(TypeAST.TypeIdentifier, String)] = P( TypeIdentifier ~ ":" ~ Kind )

  def TypeIdentifier[_: P]: P[TypeAST.TypeIdentifier] = P( Identifier ).map(TypeAST.TypeIdentifier)

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

  def VectorType[_: P]: P[TypeAST.VectorType] = P( "<" ~ Nat ~ ">" ~/ DataType  ).map(TypeAST.VectorType.tupled)

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

//  def isWellFormedType(typeAST: TypeAST, env: Map[TypeAST.Identifier, (String, Explicitness)]): Boolean = {
//    typeAST match {
//      case TypeAST.Identifier(name) =>
//      case TypeAST.FunType(inT, outT) => isWellFormedType(inT, env) && isWellFormedType(outT, env)
//      case TypeAST.DepFunType(id, kind, t) =>
//
//      case TypeAST.ImplicitDepFunType(id, kind, t) =>
//      case TypeAST.ScalarType(t) =>
//      case TypeAST.NatType =>
//      case TypeAST.VectorType(size, elemType) =>
//      case TypeAST.IndexType(size) =>
//      case TypeAST.PairType(lhs, rhs) =>
//      case TypeAST.DepPairType(id, kind, t) =>
//      case TypeAST.NatToDataApply(f, n) =>
//      case TypeAST.NatToDataLambda(id, t) =>
//      case TypeAST.ArrayType(size, elemType) =>
//      case TypeAST.DepArrayType(size, fdt) =>
//    }
//  }
}
