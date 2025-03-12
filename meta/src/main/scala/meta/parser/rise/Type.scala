package meta.parser.rise

import fastparse.ScalaWhitespace._
import fastparse._
import meta.parser.shared.Identifier
import meta.parser._

object Type {
  sealed trait AST
  object AST {
    case class Identifier(name: String) extends AST
    case class UnrolledIdentifier(name: String) extends AST

    case class FunType(inT: AST, outT: AST) extends AST
    case class DepFunType(id: Identifier, kind: Kind.AST, t: AST) extends AST
    case class ImplicitDepFunType(id: Identifier, kind: Kind.AST, t: AST) extends AST
    case class VariadicFunType(n: Identifier, inTs: AST, outT: AST) extends AST
    case class VariadicDepFunType(n: Identifier, id: Identifier, kind: Kind.AST, t: AST) extends AST

    case class ScalarType(t: String) extends AST
    case object NatType extends AST
    case class OpaqueType(name: String) extends AST
    case class VectorType(size: Nat.AST, elemType: AST) extends AST
    case class IndexType(size: Nat.AST) extends AST
    case class PairType(lhs: AST, rhs: AST) extends AST
    case class DepPairType(id: Identifier, kind: Kind.AST, t: AST) extends AST
    case class NatToDataApply(f: AST, n: Nat.AST) extends AST
    case class NatToDataLambda(id: Identifier, t: AST) extends AST
    case class ArrayType(size: Nat.AST, elemType: AST) extends AST
    case class DepArrayType(size: Nat.AST, fdt: AST) extends AST
    case class FragmentType(n: Nat.AST, m: Nat.AST, k: Nat.AST, elemType: AST,
                            fKind: Fragment.AST, mLayoutKind: MatrixLayout.AST) extends AST
    case class ManagedBufferType(t: AST) extends AST
  }

  object Fragment {
    sealed trait AST
    object AST {
      case class Identifier(name: String) extends AST
      object ACC extends AST
      object A extends AST
      object B extends AST
    }
  }

  object MatrixLayout {
    sealed trait AST
    object AST {
      case class Identifier(name: String) extends AST
      object ROW_MAJOR extends AST
      object COL_MAJOR extends AST
      object NONE extends AST
    }
  }

  def TypeSignature[$: P]: P[AST] = {
    def DepFunType: P[AST.DepFunType] =
      P("(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature).map(AST.DepFunType.tupled)

    def VariadicDepFunType: P[AST.VariadicDepFunType] =
      P(Identifier.map(AST.Identifier) ~ "*" ~ "(" ~ "(" ~ IdentifierKindPair ~ ")" ~ "->" ~ ")" ~/ TypeSignature).map {
        case (n, (id, kind), t) => AST.VariadicDepFunType(n, id, kind, t)
      }

    def ImplicitDepFunType: P[AST.ImplicitDepFunType] =
      P("{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature).
        map(AST.ImplicitDepFunType.tupled)

    def FunType: P[AST.FunType] =
      P(NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature).map(AST.FunType.tupled)

    def VariadicFunType: P[AST.VariadicFunType] =
      P(Identifier.map(AST.Identifier) ~ "*" ~ "(" ~ NoCut(LeftTypeSignature) ~ "->" ~ ")" ~/ TypeSignature).map(
        AST.VariadicFunType.tupled)

    // Types that can appear at the left of an function arrow
    def LeftTypeSignature: P[AST] = P(DataType.DataType | ("(" ~ TypeSignature ~ ")"))

    P(DepFunType | VariadicDepFunType | ImplicitDepFunType | FunType | VariadicFunType | LeftTypeSignature)
  }

  def TypeIdentifier[$: P]: P[AST.Identifier] = P(Identifier).map(AST.Identifier)

  def IdentifierKindPair[$: P]: P[(AST.Identifier, Kind.AST)] =
    P(Identifier.map(AST.Identifier) ~ ":" ~ Kind.Kind)

  def UnrolledTypeIdentifier[$: P]: P[AST.UnrolledIdentifier] =
    P("*" ~ Identifier).map(AST.UnrolledIdentifier)

  object DataType {
    def ScalarType[$: P]: P[AST.ScalarType] =
      P("bool".! | "int".! |
        "i8".! | "i16".! | "i32".! | "i64".! |
        "u8".! | "u16".! | "u32".! | "u64".! |
        "f16".! | "f32".! | "f64".!).map(AST.ScalarType)

    def NatType[$: P]: P[AST.NatType.type] = P("natType").map(_ => AST.NatType)

    def OpaqueType[$: P]: P[AST.OpaqueType] = P( "\"" ~~ Identifier ~~ "\"" ).map(AST.OpaqueType)

    def IndexType[$: P]: P[AST.IndexType] = P("idx[" ~ Nat.Nat ~ "]").map(AST.IndexType)

    def VectorType[$: P]: P[AST.VectorType] =
      P("vec[" ~ DataType ~ "," ~ Nat.Nat ~ "]").map(t => AST.VectorType(t._2, t._1))

    def FragmentType[$: P]: P[AST.FragmentType] = {
      def FragmentKind: P[Fragment.AST] =
        P(("fragment." ~~ (
          "ACC".!.map(_ => Fragment.AST.ACC) |
            "A".!.map(_ => Fragment.AST.A) |
            "B".!.map(_ => Fragment.AST.B))
          ) | TypeIdentifier.map(i => Fragment.AST.Identifier(i.name)))

      def MatrixLayoutKind: P[MatrixLayout.AST] =
        P(("matrixLayout." ~~ (
          "ROW_MAJOR".!.map(_ => MatrixLayout.AST.ROW_MAJOR) |
            "COL_MAJOR".!.map(_ => MatrixLayout.AST.COL_MAJOR) |
            "NONE".!.map(_ => MatrixLayout.AST.NONE))
          ) | TypeIdentifier.map(i => MatrixLayout.AST.Identifier(i.name)))

      P("fragment[" ~ Nat.Nat ~ "," ~ Nat.Nat ~ "," ~ Nat.Nat ~ "," ~ DataType ~ "," ~ FragmentKind ~
        "," ~ MatrixLayoutKind ~ "]").map(AST.FragmentType.tupled)
    }

    def ManagedBufferType[$: P]: P[AST.ManagedBufferType] =
      P("managed[" ~ DataType ~ "]").map(AST.ManagedBufferType)

    def DepArrayType[$: P]: P[AST.DepArrayType] =
      P(Nat.Nat ~ ".." ~/ NatToData).map(AST.DepArrayType.tupled)

    def ArrayType[$: P]: P[AST.ArrayType] =
      P(Nat.Nat ~ "." ~~ !"." ~/ DataType).map(AST.ArrayType.tupled)

    def DepPairType[$: P]: P[AST.DepPairType] =
      P("(" ~ IdentifierKindPair ~ "**" ~/ DataType ~ ")").map(AST.DepPairType.tupled)

    def NatToDataApply[$: P]: P[AST.NatToDataApply] =
      P(NatToData ~ "(" ~ Nat.Nat ~ ")").map(AST.NatToDataApply.tupled)

    def PairType[$: P]: P[AST.PairType] =
      P("(" ~ NoCut(DataType) ~ "," ~/ DataType ~ ")").map(AST.PairType.tupled)

    def DataType[$: P]: P[AST] =
      P(ScalarType | NatType | OpaqueType | IndexType | VectorType | FragmentType |
        ManagedBufferType | DepArrayType | ArrayType | DepPairType | NatToDataApply |
        PairType | UnrolledTypeIdentifier | TypeIdentifier | ("(" ~ DataType ~ ")"))

    def TypeName[$: P]: P[Unit] =
      P(ScalarType | NatType | "idx" | "vec" | "fragment" | "matrixLayout")
  }

  def NatToData[$: P]: P[AST] = {
    def NatToDataLambda: P[AST.NatToDataLambda] =
      P("(" ~ IdentifierKindPair.filter(_._2 == Kind.AST.Nat).map(_._1) ~
        "|->" ~/ DataType.DataType ~ ")").map(AST.NatToDataLambda.tupled)

    P(TypeIdentifier | NatToDataLambda)
  }
}
