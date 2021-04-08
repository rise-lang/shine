package meta

import fastparse.ScalaWhitespace._
import fastparse._
import meta.NatParser._
import meta.TypeParser.TypeAST.{FragmentAST, MatrixLayoutAST}

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
    case class FragmentType(n: NatAST, m: NatAST, k: NatAST, elemType: TypeAST,
                            fKind: FragmentAST, mLayout: MatrixLayoutAST) extends TypeAST

    sealed trait FragmentAST
    object FragmentAST {
      case class Identifier(id: TypeAST.Identifier) extends FragmentAST
      object ACC extends FragmentAST
      object A extends FragmentAST
      object B extends FragmentAST
    }

    sealed trait MatrixLayoutAST
    object MatrixLayoutAST {
      case class Identifier(id: TypeAST.Identifier) extends MatrixLayoutAST
      object ROW_MAJOR extends MatrixLayoutAST
      object COL_MAJOR extends MatrixLayoutAST
    }

    object Kind extends Enumeration {
      val Data, Nat, Nat2Nat, Nat2Data, Address, Fragment, MatrixLayout, Function = Value

      def fromString(s: String): Value = s match {
        case "data" => Data
        case "nat" => Nat
        case "nat2nat" => Nat2Nat
        case "nat2data" => Nat2Data
        case "address" => Address
        case "fragment" => Fragment
        case "matrixLayout" => MatrixLayout
      }
    }
  }

  def PrimitiveDeclarations[_: P]: P[Seq[(String, Option[(Int, Int)], TypeAST)]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  def PrimitiveDeclaration[_: P]: P[(String, Option[(Int, Int)], TypeAST)] = {
    def ScalaFunArgs: P[(Int, Int)] = {
      import scalaparse.Scala.TrailingCommaOps
      P("(" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index  ~ ")")
    }

    P("def" ~ Identifier ~ ScalaFunArgs.? ~ ":" ~ TypeSignature)
  }

  def Identifier[_: P]: P[String] = {
    def Keywords: P[Unit] =
      P(("def" | (Kind: P[Unit]) | DataType.TypeName) ~~ CharPred(_.isWhitespace))

    val LowerChar = scalaparse.syntax.Identifiers.NamedFunction(CharPredicates.isLower)
    val IdCharacter = scalaparse.syntax.Identifiers.NamedFunction(c =>
      CharPredicates.isLetter(c) || CharPredicates.isDigit(c))

    P((!Keywords ~ CharPred(LowerChar).! ~~ CharsWhile(IdCharacter).!.?).
      map(t => t._1 ++ t._2.getOrElse("")))
  }

  def TypeSignature[_: P]: P[TypeAST] = {
    def DepFunType: P[TypeAST.DepFunType] =
      P("(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature).map(TypeAST.DepFunType.tupled)

    def ImplicitDepFunType: P[TypeAST.ImplicitDepFunType] =
      P("{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature).
        map(TypeAST.ImplicitDepFunType.tupled)

    def FunType: P[TypeAST.FunType] =
      P(NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature).map(TypeAST.FunType.tupled)

    // Types that can appear at the left of an function arrow
    def LeftTypeSignature: P[TypeAST] = P(DataType.DataType | ("(" ~ TypeSignature ~ ")"))

    P(DepFunType | ImplicitDepFunType | FunType | LeftTypeSignature)
  }

  def TypeIdentifier[_: P]: P[TypeAST.Identifier] = P(Identifier).map(TypeAST.Identifier)

  def IdentifierKindPair[_: P]: P[(TypeAST.Identifier, String)] = P(TypeIdentifier ~ ":" ~ Kind)

  def Kind[_: P]: P[String] =
    P("data".! | "address".! | "nat2nat".! | "nat2data".! | "nat".! |
      "fragment".! | "matrixLayout".!)

  object DataType {
    def ScalarType[_: P]: P[TypeAST.ScalarType] =
      P("bool".! | "int".! |
        "i8".! | "i16".! | "i32".! | "i64".! |
        "u8".! | "u16".! | "u32".! | "u64".! |
        "f16".! | "f32".! | "f64".!).map(TypeAST.ScalarType)

    def NatType[_: P]: P[TypeAST.NatType.type] = P("natType").map(_ => TypeAST.NatType)

    def IndexType[_: P]: P[TypeAST.IndexType] = P("idx[" ~ Nat ~ "]").map(TypeAST.IndexType)

    def VectorType[_: P]: P[TypeAST.VectorType] =
      P("vec[" ~ DataType ~ "," ~ Nat ~ "]").map(t => TypeAST.VectorType(t._2, t._1))

    def FragmentType[_: P]: P[TypeAST.FragmentType] = {
      def FragmentKind: P[FragmentAST] =
        P(("fragment." ~~ (
          "ACC".!.map(_ => FragmentAST.ACC) |
            "A".!.map(_ => FragmentAST.A) |
            "B".!.map(_ => FragmentAST.B))
          ) | TypeIdentifier.map(FragmentAST.Identifier))

      def MatrixLayout: P[MatrixLayoutAST] =
        P(("matrixLayout." ~~ (
          "ROW_MAJOR".!.map(_ => MatrixLayoutAST.ROW_MAJOR) |
            "COL_MAJOR".!.map(_ => MatrixLayoutAST.COL_MAJOR))
          ) | TypeIdentifier.map(MatrixLayoutAST.Identifier))

      P("fragment["~ Nat ~","~ Nat ~","~ Nat ~","~ DataType ~","~ FragmentKind ~
        ","~ MatrixLayout ~"]").map(TypeAST.FragmentType.tupled)
    }

    def DepArrayType[_: P]: P[TypeAST.DepArrayType] =
      P(Nat ~ ".." ~/ NatToData).map(TypeAST.DepArrayType.tupled)

    def ArrayType[_: P]: P[TypeAST.ArrayType] =
      P(Nat ~ "." ~~ !"." ~/ DataType).map(TypeAST.ArrayType.tupled)

    def DepPairType[_: P]: P[TypeAST.DepPairType] =
      P("(" ~ IdentifierKindPair ~ "**" ~/ DataType ~ ")").map(TypeAST.DepPairType.tupled)

    def NatToDataApply[_: P]: P[TypeAST.NatToDataApply] =
      P(NatToData ~ "(" ~ Nat ~ ")").map(TypeAST.NatToDataApply.tupled)

    def PairType[_: P]: P[TypeAST.PairType] =
      P("(" ~ NoCut(DataType) ~ "," ~/ DataType ~ ")").map(TypeAST.PairType.tupled)

    def DataType[_: P]: P[TypeAST] =
      P(ScalarType | NatType | IndexType | VectorType | FragmentType | DepArrayType |
        ArrayType | DepPairType | NatToDataApply | PairType | TypeIdentifier |
        ("(" ~ DataType ~ ")"))

    def TypeName[_: P]: P[Unit] =
      P(ScalarType | NatType | "idx" | "vec" | "fragment" | "matrixLayout")
  }

  def NatToData[_: P]: P[TypeAST] = {
    def NatToDataLambda: P[TypeAST.NatToDataLambda] =
      P("(" ~ IdentifierKindPair.filter(_._2 == "nat").map(_._1) ~
          "|->" ~/ DataType.DataType ~ ")").map(TypeAST.NatToDataLambda.tupled)

    P(TypeIdentifier | NatToDataLambda)
  }

  object isWellKindedType {
    import TypeAST.Kind._

    def apply(typeAST: TypeAST): Boolean = {
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
        case TypeAST.FragmentType(n, m, k, elemType, fKind, mLayout) =>
          for {
            k1 <- kindOf(n, env)
            k2 <- kindOf(m, env)
            k3 <- kindOf(k, env)
            k4 <- kindOf(elemType, env)
            k5 <- kindOf(fKind, env)
            k6 <- kindOf(mLayout, env)
            if k1 == Nat && k2 == Nat && k3 == Nat && k4 == Data &&
              k5 == Fragment && k6 == TypeAST.Kind.MatrixLayout
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
        case NatAST.Number(_) =>
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

    def kindOf(fragmentAST: TypeAST.FragmentAST,
               env: Map[TypeAST.Identifier, TypeAST.Kind.Value]): Option[TypeAST.Kind.Value] = {
      fragmentAST match {
        case FragmentAST.Identifier(id) =>
          env.get(id)
        case FragmentAST.ACC | FragmentAST.A | FragmentAST.B => Some(Fragment)
      }
    }

    def kindOf(matrixLayout: TypeAST.MatrixLayoutAST,
               env: Map[TypeAST.Identifier, TypeAST.Kind.Value]): Option[TypeAST.Kind.Value] = {
      matrixLayout match {
        case MatrixLayoutAST.Identifier(id) =>
          env.get(id)
        case MatrixLayoutAST.ROW_MAJOR |
             MatrixLayoutAST.COL_MAJOR => Some(TypeAST.Kind.MatrixLayout)
      }
    }
  }
}
