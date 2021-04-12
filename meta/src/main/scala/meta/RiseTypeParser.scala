package meta

import fastparse.ScalaWhitespace._
import fastparse._
import meta.NatParser._
import RiseDpiaShared.{Identifier, Kind}

object RiseTypeParser {
  import meta.RiseTypeParser.RISETypeAST.{FragmentAST, MatrixLayoutAST}

  sealed trait RISETypeAST
  object RISETypeAST {
    case class Identifier(name: String) extends RISETypeAST
    case class FunType(inT: RISETypeAST, outT: RISETypeAST) extends RISETypeAST
    case class DepFunType(id: Identifier, kind: String, t: RISETypeAST) extends RISETypeAST
    case class ImplicitDepFunType(id: Identifier, kind: String, t: RISETypeAST) extends RISETypeAST

    case class ScalarType(t: String) extends RISETypeAST
    case object NatType extends RISETypeAST
    case class VectorType(size: NatAST, elemType: RISETypeAST) extends RISETypeAST
    case class IndexType(size: NatAST) extends RISETypeAST
    case class PairType(lhs: RISETypeAST, rhs: RISETypeAST) extends RISETypeAST
    case class DepPairType(id: Identifier, kind: String, t: RISETypeAST) extends RISETypeAST
    case class NatToDataApply(f: RISETypeAST, n: NatAST) extends RISETypeAST
    case class NatToDataLambda(id: Identifier, t: RISETypeAST) extends RISETypeAST
    case class ArrayType(size: NatAST, elemType: RISETypeAST) extends RISETypeAST
    case class DepArrayType(size: NatAST, fdt: RISETypeAST) extends RISETypeAST
    case class FragmentType(n: NatAST, m: NatAST, k: NatAST, elemType: RISETypeAST,
                            fKind: FragmentAST, mLayout: MatrixLayoutAST) extends RISETypeAST

    sealed trait FragmentAST
    object FragmentAST {
      case class Identifier(name: String) extends FragmentAST
      object ACC extends FragmentAST
      object A extends FragmentAST
      object B extends FragmentAST
    }

    sealed trait MatrixLayoutAST
    object MatrixLayoutAST {
      case class Identifier(name: String) extends MatrixLayoutAST
      object ROW_MAJOR extends MatrixLayoutAST
      object COL_MAJOR extends MatrixLayoutAST
    }
  }

  def PrimitiveDeclarations[_: P]: P[Seq[(String, Option[(Int, Int)], RISETypeAST)]] =
    P(Start ~ PrimitiveDeclaration.rep(1) ~ End)

  def PrimitiveDeclaration[_: P]: P[(String, Option[(Int, Int)], RISETypeAST)] = {
    def ScalaFunArgs: P[(Int, Int)] = {
      import scalaparse.Scala.TrailingCommaOps
      P("(" ~ Index ~
        (scalaparse.Scala.Id ~ scalaparse.syntax.Key.O(":") ~ scalaparse.Scala.Type).repTC(1) ~
        Index  ~ ")")
    }

    P("def" ~ Identifier ~ ScalaFunArgs.? ~ ":" ~ TypeSignature)
  }

  def TypeSignature[_: P]: P[RISETypeAST] = {
    def DepFunType: P[RISETypeAST.DepFunType] =
      P("(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature).map(RISETypeAST.DepFunType.tupled)

    def ImplicitDepFunType: P[RISETypeAST.ImplicitDepFunType] =
      P("{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature).
        map(RISETypeAST.ImplicitDepFunType.tupled)

    def FunType: P[RISETypeAST.FunType] =
      P(NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature).map(RISETypeAST.FunType.tupled)

    // Types that can appear at the left of an function arrow
    def LeftTypeSignature: P[RISETypeAST] = P(DataType.DataType | ("(" ~ TypeSignature ~ ")"))

    P(DepFunType | ImplicitDepFunType | FunType | LeftTypeSignature)
  }

  def TypeIdentifier[_: P]: P[RISETypeAST.Identifier] = P(Identifier).map(RISETypeAST.Identifier)

  def IdentifierKindPair[_: P]: P[(RISETypeAST.Identifier, String)] =
    RiseDpiaShared.IdentifierKindPair.map(p => (RISETypeAST.Identifier(p._1), p._2))

  object DataType {
    def ScalarType[_: P]: P[RISETypeAST.ScalarType] =
      P("bool".! | "int".! |
        "i8".! | "i16".! | "i32".! | "i64".! |
        "u8".! | "u16".! | "u32".! | "u64".! |
        "f16".! | "f32".! | "f64".!).map(RISETypeAST.ScalarType)

    def NatType[_: P]: P[RISETypeAST.NatType.type] = P("natType").map(_ => RISETypeAST.NatType)

    def IndexType[_: P]: P[RISETypeAST.IndexType] = P("idx[" ~ Nat ~ "]").map(RISETypeAST.IndexType)

    def VectorType[_: P]: P[RISETypeAST.VectorType] =
      P("vec[" ~ DataType ~ "," ~ Nat ~ "]").map(t => RISETypeAST.VectorType(t._2, t._1))

    def FragmentType[_: P]: P[RISETypeAST.FragmentType] = {
      def FragmentKind: P[FragmentAST] =
        P(("fragment." ~~ (
          "ACC".!.map(_ => FragmentAST.ACC) |
            "A".!.map(_ => FragmentAST.A) |
            "B".!.map(_ => FragmentAST.B))
          ) | TypeIdentifier.map(i => FragmentAST.Identifier(i.name)))

      def MatrixLayout: P[MatrixLayoutAST] =
        P(("matrixLayout." ~~ (
          "ROW_MAJOR".!.map(_ => MatrixLayoutAST.ROW_MAJOR) |
            "COL_MAJOR".!.map(_ => MatrixLayoutAST.COL_MAJOR))
          ) | TypeIdentifier.map(i => MatrixLayoutAST.Identifier(i.name)))

      P("fragment["~ Nat ~","~ Nat ~","~ Nat ~","~ DataType ~","~ FragmentKind ~
        ","~ MatrixLayout ~"]").map(RISETypeAST.FragmentType.tupled)
    }

    def DepArrayType[_: P]: P[RISETypeAST.DepArrayType] =
      P(Nat ~ ".." ~/ NatToData).map(RISETypeAST.DepArrayType.tupled)

    def ArrayType[_: P]: P[RISETypeAST.ArrayType] =
      P(Nat ~ "." ~~ !"." ~/ DataType).map(RISETypeAST.ArrayType.tupled)

    def DepPairType[_: P]: P[RISETypeAST.DepPairType] =
      P("(" ~ IdentifierKindPair ~ "**" ~/ DataType ~ ")").map(RISETypeAST.DepPairType.tupled)

    def NatToDataApply[_: P]: P[RISETypeAST.NatToDataApply] =
      P(NatToData ~ "(" ~ Nat ~ ")").map(RISETypeAST.NatToDataApply.tupled)

    def PairType[_: P]: P[RISETypeAST.PairType] =
      P("(" ~ NoCut(DataType) ~ "," ~/ DataType ~ ")").map(RISETypeAST.PairType.tupled)

    def DataType[_: P]: P[RISETypeAST] =
      P(ScalarType | NatType | IndexType | VectorType | FragmentType | DepArrayType |
        ArrayType | DepPairType | NatToDataApply | PairType | TypeIdentifier |
        ("(" ~ DataType ~ ")"))

    def TypeName[_: P]: P[Unit] =
      P(ScalarType | NatType | "idx" | "vec" | "fragment" | "matrixLayout")
  }

  def NatToData[_: P]: P[RISETypeAST] = {
    def NatToDataLambda: P[RISETypeAST.NatToDataLambda] =
      P("(" ~ IdentifierKindPair.filter(_._2 == "nat").map(_._1) ~
          "|->" ~/ DataType.DataType ~ ")").map(RISETypeAST.NatToDataLambda.tupled)

    P(TypeIdentifier | NatToDataLambda)
  }

  object isWellKindedType {
    import RiseDpiaShared.TypeAST.Kind._

    def apply(typeAST: RISETypeAST): Boolean = {
      kindOf(typeAST, Map.empty).isDefined
    }

    def kindOf(typeAST: RISETypeAST,
               env: Map[String, RiseDpiaShared.TypeAST.Kind.Value]): Option[RiseDpiaShared.TypeAST.Kind.Value] = {
      typeAST match {
        case RISETypeAST.Identifier(name) =>
          env.get(name)
        case RISETypeAST.FunType(inT, outT) =>
          for {
            _ <- kindOf(inT, env)
            _ <- kindOf(outT, env)
          } yield Function
        case RISETypeAST.DepFunType(id, kind, t) =>
          if (env.isDefinedAt(id.name)) {
            None // we forbid shadowing
          } else {
            kindOf(t, env.updated(id.name, RiseDpiaShared.TypeAST.Kind.fromString(kind)))
          }
        case RISETypeAST.ImplicitDepFunType(id, kind, t) =>
          if (env.isDefinedAt(id.name)) {
            None // we forbid shadowing
          } else {
            kindOf(t, env.updated(id.name, RiseDpiaShared.TypeAST.Kind.fromString(kind)))
          }
        case RISETypeAST.VectorType(size, elemType) =>
          for {
            k1 <- kindOf(size, env)
            k2 <- kindOf(elemType, env)
            if k1 == Nat && k2 == Data
          } yield Data
        case RISETypeAST.IndexType(size) =>
          for {
            k <- kindOf(size, env)
            if k == Nat
          } yield Data
        case RISETypeAST.PairType(lhs, rhs) =>
          for {
            k1 <- kindOf(lhs, env)
            k2 <- kindOf(rhs, env)
            if k1 == Data && k2 == Data
          } yield Data
        case RISETypeAST.DepPairType(id, kind, t) =>
          if (env.isDefinedAt(id.name)) {
            None // we forbid shadowing
          } else {
            kindOf(t, env.updated(id.name, RiseDpiaShared.TypeAST.Kind.fromString(kind)))
          }
        case RISETypeAST.NatToDataApply(f, n) =>
          for {
            k1 <- kindOf(f, env)
            k2 <- kindOf(n, env)
            if k1 == Nat2Data && k2 == Nat
          } yield Data
        case RISETypeAST.NatToDataLambda(id, t) =>
          if (env.isDefinedAt(id.name)) {
            None // we forbid shadowing
          } else {
            for {
              k <- kindOf(t, env.updated(id.name, Nat))
              if k == Data
            } yield Nat2Data
          }
        case RISETypeAST.ArrayType(size, elemType) =>
          for {
            k1 <- kindOf(size, env)
            k2 <- kindOf(elemType, env)
            if k1 == Nat && k2 == Data
          } yield Data
        case RISETypeAST.DepArrayType(size, fdt) =>
          for {
            k1 <- kindOf(size, env)
            k2 <- kindOf(fdt, env)
            if k1 == Nat && k2 == Nat2Data
          } yield Data
        case RISETypeAST.FragmentType(n, m, k, elemType, fKind, mLayout) =>
          for {
            k1 <- kindOf(n, env)
            k2 <- kindOf(m, env)
            k3 <- kindOf(k, env)
            k4 <- kindOf(elemType, env)
            k5 <- kindOf(fKind, env)
            k6 <- kindOf(mLayout, env)
            if k1 == Nat && k2 == Nat && k3 == Nat && k4 == Data &&
              k5 == Fragment && k6 == RiseDpiaShared.TypeAST.Kind.MatrixLayout
          } yield Data
        case _: RISETypeAST.ScalarType | RISETypeAST.NatType =>
          Some(Data)
      }
    }

    def kindOf(natAST: NatAST,
               env: Map[String, RiseDpiaShared.TypeAST.Kind.Value]
              ): Option[RiseDpiaShared.TypeAST.Kind.Value] = {
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
          val nEnv = env.updated(id.name, Nat)
          for {
            k1 <- kindOf(from, nEnv)
            k2 <- kindOf(upTo, nEnv)
            k3 <- kindOf(body, nEnv)
            if k1 == Nat && k2 == Nat && k3 == Nat
          } yield Nat
      }
    }

    def kindOf(fragmentAST: RISETypeAST.FragmentAST,
               env: Map[String, RiseDpiaShared.TypeAST.Kind.Value]
              ): Option[RiseDpiaShared.TypeAST.Kind.Value] = {
      fragmentAST match {
        case FragmentAST.Identifier(id) =>
          env.get(id)
        case FragmentAST.ACC | FragmentAST.A | FragmentAST.B => Some(Fragment)
      }
    }

    def kindOf(matrixLayout: RISETypeAST.MatrixLayoutAST,
               env: Map[String, RiseDpiaShared.TypeAST.Kind.Value]
              ): Option[RiseDpiaShared.TypeAST.Kind.Value] = {
      matrixLayout match {
        case MatrixLayoutAST.Identifier(id) =>
          env.get(id)
        case MatrixLayoutAST.ROW_MAJOR |
             MatrixLayoutAST.COL_MAJOR => Some(RiseDpiaShared.TypeAST.Kind.MatrixLayout)
      }
    }
  }
}
