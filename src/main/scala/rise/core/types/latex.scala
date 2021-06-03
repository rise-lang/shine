package rise.core.types

import scala.language.{implicitConversions, postfixOps}
import rise.core.DSL.Type._
import rise.core._

object latex {
  import builderDSL._
  // LaTeX printer
  def toLaTeX[T, I <: Kind.Identifier](kind: Kind[T, I]): String = kind match {
    case TypeKind => "\\mathsf{type}"
    case DataKind => "\\mathsf{data}"
    case NatKind => "\\mathsf{nat}"
    case NatCollectionKind => "\\mathsf{nats}"
    case AddressSpaceKind => "\\mathsf{addrSpace}"
//    case MatrixLayoutKind => "\\mathsf{matrixLayout}"
//    case FragmentKind => "\\mathsf{fragment}"
    case NatToNatKind =>
      s"${toLaTeX(natkind)}\\hspace{-.25em}\\rightarrow\\hspace{-.25em}${toLaTeX(natkind)}"
    case NatToDataKind =>
      s"${toLaTeX(natkind)}\\hspace{-.25em}\\rightarrow\\hspace{-.25em}${toLaTeX(datakind)}"
  }
  def kindToLaTeX[T, I <: Kind.Identifier](kind: Kind[T, I]): String = toLaTeX(kind)

  def toLaTeX(t: Type): String = t match {
    case TypePlaceholder => s"?"
    case TypeIdentifier(name) => name
    case FunType(inT, outT) =>
      s"${toLaTeX(inT)} \\rightarrow ${toLaTeX(outT)}"
    case d@DepFunType(kind, x, t) =>
      s"(${kindIdentifier(kind, x)}) \\rightarrow ${toLaTeX(t)}"
    case dt: DataType => toLaTeX(dt)
  }

  def toLaTeX(dt: DataType): String = dt match {
    case DataTypeIdentifier(name) => name
    case _: ScalarType => s"$dt"
    case NatType => s"\\mathsf{natType}"
    case IndexType(size) =>
      s"\\mathsf{idx}[${toLaTeX(size)}]"
    case VectorType(size, elemType) =>
      s"\\mathsf{vec}[${toLaTeX(elemType)}, ${toLaTeX(size)}]"
    case FragmentType(rows, columns, d3, dataType, fragmentKind, layout) =>
      s"\\mathsf{fragment}[${toLaTeX(rows)}, ${toLaTeX(columns)}, ${toLaTeX(d3)}, " +
        s"${toLaTeX(dataType)}, ${toLaTeX(fragmentKind)}, ${toLaTeX(layout)}]"

    case PairType(dt1, dt2) =>
      s"(${toLaTeX(dt1)},\\ ${toLaTeX(dt2)})"
    case ArrayType(size, elemType) =>
      s"${toLaTeX(size)}_\\bullet ${toLaTeX(elemType)}"

    case DepPairType(kind, x, t) =>
      s"(${kindIdentifier(kind, x)}\\ \\ast\\ast\\ ${toLaTeX(t)})"
    case DepArrayType(size, fdt) =>
      s"${toLaTeX(size)}_{\\bullet\\bullet} ${toLaTeX(fdt)}"

    case apply: NatToDataApply => s"${apply.f}\\ ${apply.n}"
    case ManagedBufferType(_) | OpaqueType(_) => ???
  }

  def toLaTeX(nat: Nat): String = nat match {
    case apply: NatToNatApply => s"${toLaTeX(apply.f)}\\ ${toLaTeX(apply.n)}"
    case _ => s"$nat"
  }

  def toLaTeX(addressSpace: AddressSpace): String = addressSpace match {
    case AddressSpaceIdentifier(name) => name
    case AddressSpace.Global => "\\mathsf{global}"
    case AddressSpace.Local => "\\mathsf{local}"
    case AddressSpace.Private => "\\mathsf{private}"
    case AddressSpace.Constant => "\\mathsf{constant}"
  }

  def toLaTeX(ntn: NatToNat): String = ntn match {
    case NatToNatIdentifier(name) => name
    case NatToNatLambda(x, body) => s"${toLaTeX(x)} \\mapsto ${toLaTeX(body)}"
  }

  def toLaTeX(ntd: NatToData): String = ntd match {
    case NatToDataIdentifier(name) => name
    case NatToDataLambda(x, body) => s"${toLaTeX(x)} \\mapsto ${toLaTeX(body)}"
  }

  def toLaTeX(kind: FragmentKind): String = kind match {
    case FragmentKind.AMatrix => "\\mathsf{AMatrix}"
    case FragmentKind.BMatrix => "\\mathsf{BMatrix}"
    case FragmentKind.Accumulator => "\\mathsf{Accumulator}"
    case FragmentKindIdentifier(name) => name
  }

  def toLaTeX(layout: MatrixLayout): String = layout match {
    case MatrixLayout.Row_Major => "\\mathsf{Row\\_Major}"
    case MatrixLayout.Col_Major => "\\mathsf{Col\\_Major}"
    case MatrixLayout.None => ???
    case MatrixLayoutIdentifier(name) => name
  }

  def toLaTeX(expr: Expr): String = expr match {
    case Identifier(name) => name
    case Lambda(x, e) => s"\\lambda ${toLaTeX(x)}. ${toLaTeX(e)}"
    case App(f, e) => s"${toLaTeX(f)}\\ ${toLaTeX(e)}"
    case DepLambda(kind, x, e) => s"\\Lambda ${kindIdentifier(kind, x)}. ${toLaTeX(e)}"
    case DepApp(_, f, x) => s"${toLaTeX(f)}\\ ${x match {
      case d: DataType => toLaTeX(d)
      case n: Nat => toLaTeX(n)
      case a: AddressSpace => toLaTeX(a)
      case ntd: NatToData => toLaTeX(ntd)
      case ntn: NatToNat => toLaTeX(ntn)
      case m: MatrixLayout => toLaTeX(m)
      case f: FragmentKind => toLaTeX(f)
    }}"
    case Literal(d) => d.toString
    case Opaque(e, t) => ???
    case TypeAnnotation(e, annotation) => ???
    case TypeAssertion(e, assertion) => ???
    case primitive: Primitive => primitive.name
  }

  private def kindIdentifier[T, I <: Kind.Identifier](kind: Kind[T, I], x: I): String =
    kind.name match {
      case "nat" => s"${x.name}: ${toLaTeX(NatKind)}"
    }

  object wellFormedTypes {
    val kinds: String =
      kappa ::=  Seq(
        typekind, datakind, natkind, //natskind,
        ntdkind, ntnkind, addrkind,
        // matrixKind, fragmentKind
      ) .map(kindToLaTeX)
        .mkString(" \\mid ")

    val structuralRules: String = (
      ("x" `:` kappa) in Delta
      ) -------------------- (
      Delta |- ("x" `:` kappa)
      )

    val typeEquality: Row = Row(
      |=(forall(sigma)
      (s"\\mathit{dom}($Delta)" to "\\mathbb{N}")
      (s"$sigma($N) = $sigma($M)")
      ) -------------------- (
        Delta |- (s"$N \\equiv $M" `:` natkind)
        )
    )

//    val fragments: Row = Row(
//      (
//        Frag in FiniteSet(aMatrix, bMatrix, accumulator)
//        ) -------------------- (
//        Delta |- (Frag `:` fragmentKind)
//        )
//    )
//
//    val matrixLayout: Row = Row(
//      (
//        Mat in FiniteSet(rowMajor, colMajor)
//        ) -------------------- (
//        Delta |- (Mat `:` matrixKind)
//        )
//    )

    val addressSpaces: Row = Row((
      A in FiniteSet(global, local, `private`, constant)
      ) -------------------- (
      Delta |- (A `:` addrkind)
      )
    )

    val natToNatTypeLevelFunctions: Row = Row((
      (Delta `,` (n `:` natkind)) |- (M `:` natkind)
      ) -------------------- (
      Delta |- ((n |->: M) `:` ntnkind)
      )
    )

    val natToDataTypeLevelFunctions: Row = Row((
      (Delta `,` (n `:` natkind)) |- (T `:` datakind)
      ) -------------------- (
      Delta |- ((n |->: T) `:` ntdkind)
      )
    )

    val natCollections: Rows = Rows()

    val naturalNumbers: Rows = Rows(
      Row(
        ""
          -------------------- (
          Delta |- ("\\underline{\\ell}" `:` natkind)
          ),
        (
          Delta |- (N `:` natkind) quad
            Delta |- (M `:` natkind) quad
            "\\oplus" in FiniteSet("+", "*", ldots)
          ) -------------------- (
          Delta |- (s"$N \\oplus $M" `:` natkind)
          )),
      Row(
        (
          Delta |- (FN `:` ntnkind) quad Delta |- (M `:` natkind)
          ) -------------------- (
          Delta |- (FN(M) `:` natkind)
          ))
    )

    val dataTypes: Rows = Rows(
      Row(
        ""
          -------------------- (
          Delta |- ("\\mathsf{scalar}" `:` datakind)
          ),
        ""
          -------------------- (
          Delta |- (NatType `:` datakind)
          ),
        (
          Delta |- (N `:` natkind)
          ) -------------------- (
          Delta |- (IndexType(N) `:` datakind)
          )),
      Row(
        (
          Delta |- (N `:` natkind) qquad
            Delta |- (T `:` datakind)
          ) -------------------- (
          Delta |- (VectorType(N, T) `:` datakind)
          )),
//      Row(
//        ( Delta |- (N `:` natkind) quad
//          Delta |- (M `:` natkind) quad
//          Delta |- (K `:` natkind) quad
//          Delta |- (T `:` datakind) quad
//          Delta |- (Frag `:` fragmentKind) quad
//          Delta |- (Mat `:` matrixKind)
//          ) -------------------- (
//          Delta |- (FragmentType(N, M, K, T, Frag, Mat) `:` datakind)
//          )),
      Row(
        (
          Delta |- (S `:` datakind) quad Delta |- (T `:` datakind)
          ) -------------------- (
          Delta |- ((S x T) `:` datakind)
          )),
      Row(
        (
          Delta |- (N `:` natkind) quad Delta |- (T `:` datakind)
          ) -------------------- (
          Delta |- (N `.` T `:` datakind)
          )),
      Row(
        (
          (Delta `,` (n `:` natkind)) |- (T `:` datakind)
          ) -------------------- (
          Delta |- (DepPairType(NatKind, n, T) `:` datakind)
          )),
      Row(
        (
          Delta |- (N `:` natkind) quad Delta |- (FT `:` ntdkind)
          ) -------------------- (
          Delta |- (N `*.` FT `:` datakind)
          )),
      Row(
        (
          Delta |- (FT `:` ntdkind) quad Delta |- (N `:` natkind)
          ) -------------------- (
          Delta |- (FT(N) `:` datakind)
          )
      )
    )

    val types: Rows = Rows(
      Row(
        (
          Delta |- (T `:` datakind)
          ) -------------------- (
          Delta |- (T `:` typekind)
          ),
        (
          Delta |- (Theta1 `:` typekind) quad
            Delta |- (Theta2 `:` typekind)
          ) -------------------- (
          Delta |- (Theta1 ->: Theta2 `:` typekind)
          )),
      Row(
        (
          (Delta `,` ("x" `:` kappa)) |- (Theta `:` typekind) qquad
            kappa in FiniteSet(addrkind, ntnkind, ntdkind, natkind, datakind)
          ) -------------------- (
          // DepFunType
          Delta |- (s"(x: $kappa) \\rightarrow ${toLaTeX(Theta)}" `:` typekind)
          ))
    )
  }

  object typingRules {
    val structural: Row = Row(
      (
        (x `:` Theta) in Gamma
        ) -------------------- (
        Delta | Gamma |- (x `:` Theta)
        ),
      (
        Delta | Gamma |- (E `:` Theta1) quad
          Delta |- (s"${toLaTeX(Theta1)} \\equiv ${toLaTeX(Theta2)}" `:` typekind)
        ) -------------------- (
        Delta | Gamma |- (E `:` Theta2)
        ),
      (
        (prim `:` Theta) in NamedSet("\\mathsf{Primitives}")
        ) -------------------- (
        Delta | Gamma |- (prim `:` Theta)
        )
    )

    val introAndElim: Rows = Rows(
      Row(
        (
          Delta | (Gamma `,` (x `:` Theta1)) |- (E `:` Theta2)
          ) -------------------- (
          Delta | Gamma |- (Lambda(x, E)(TypePlaceholder) `:` Theta1 ->: Theta2)
          ),
        (
          Delta | Gamma1 |- (E1 `:` Theta1 ->: Theta2) quad
            Delta | Gamma2 |- (E2 `:` Theta1)
          ) -------------------- (
          Delta | (Gamma1 `,` Gamma2) |- (App(E1, E2)(TypePlaceholder) `:` Theta2)
          )
      ),
      Row(
        (
          (Delta `,` ("x" `:` kappa)) | Gamma |- (E `:` Theta) quad
            "x" notin NamedSet(s"ftv($Gamma)")
          ) -------------------- (
          Delta | Gamma |-
            ValueTypePair(s"\\Lambda x. $E", s"(x: $kappa) \\rightarrow ${toLaTeX(Theta)}")
          ),
        (
          Delta | Gamma |-
            ValueTypePair(E, s"(x: $kappa) \\rightarrow ${toLaTeX(Theta)}") quad
            Delta |- (tau `:` kappa)
          ) -------------------- (
          Delta | Gamma |-
            ValueTypePair(s"$E\\ $tau", s"${toLaTeX(Theta)}[$tau/x]")
          )
      )
    )
  }

  //noinspection ScalaStyle
  object builderDSL {
    val Delta: KindingEnvVar = KindingEnvVar("\\Delta")
    val Gamma: TypingEnvVar = TypingEnvVar("\\Gamma")
    val Gamma1: TypingEnvVar = TypingEnvVar("\\Gamma_1")
    val Gamma2: TypingEnvVar = TypingEnvVar("\\Gamma_2")

    val typekind: TypeKind.type = TypeKind
    val datakind: DataKind.type = DataKind
    val natkind: NatKind.type = NatKind
    val natskind: NatCollectionKind.type = NatCollectionKind
    val ntdkind: NatToDataKind.type = NatToDataKind
    val ntnkind: NatToNatKind.type = NatToNatKind
    val addrkind: AddressSpaceKind.type = AddressSpaceKind
//    val matrixKind: MatrixLayout = MatrixLayout
//    val fragmentKind: FragmentKind = FragmentKind

    val global: String = toLaTeX(AddressSpace.Global)
    val local: String = toLaTeX(AddressSpace.Local)
    val `private`: String = toLaTeX(AddressSpace.Private)
    val constant: String = toLaTeX(AddressSpace.Constant)

//    val aMatrix: String = toLaTeX(Fragment.AMatrix)
//    val bMatrix: String = toLaTeX(Fragment.BMatrix)
//    val accumulator: String = toLaTeX(Fragment.Accumulator)

    val rowMajor: String = toLaTeX(MatrixLayout.Row_Major)
    val colMajor: String = toLaTeX(MatrixLayout.Col_Major)

    val kappa: String = "\\kappa"
    val tau: String = "\\tau"
    val sigma: String = "\\sigma"
    val prim: String = "\\mathsf{prim}"

    val ldots: String = "\\ldots"

    val A: AddressSpaceIdentifier = aid("A")
    val N: NatIdentifier = nid("N")
    val M: NatIdentifier = nid("M")
    val K: NatIdentifier = nid("K")
    val n: NatIdentifier = nid("n")
    val S: DataTypeIdentifier = did("S")
    val T: DataTypeIdentifier = did("T")
    val Theta: TypeIdentifier = tid("\\theta")
    val Theta1: TypeIdentifier = tid("\\theta_1")
    val Theta2: TypeIdentifier = tid("\\theta_2")
    val FN: NatToNatIdentifier = ntnid("F_N")
    val FT: NatToDataIdentifier = ntdid("F_T")
    val Frag: FragmentKindIdentifier = fid("F")
    val Mat: MatrixLayoutIdentifier = mid("Mat")

    val E: Identifier = Identifier("E")(TypePlaceholder)
    val E1: Identifier = Identifier("E_1")(TypePlaceholder)
    val E2: Identifier = Identifier("E_2")(TypePlaceholder)
    val x: Identifier = Identifier("x")(TypePlaceholder)

    def forall(x: String)(dom: String)(body: String): String =
      s"\\forall $x : $dom . $body"

    def |= (rhs: String) = s"\\models $rhs"

    implicit class GenericKindPairBuilder(kind: String) {
      def `:` (x: String): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class TypeKindPairBuilder(kind: TypeKind.type) {
      def `:` (x: Type): TypeKindPair = TypeKindPair(x, kind)
      def `:` (str: String): TypeKindPair = TypeKindPair(str, kind)
    }

    implicit class DataKindPairBuilder(kind: DataKind.type) {
      def `:` (x: DataType): TypeKindPair = TypeKindPair(x, kind)
      def `:` (str: String): TypeKindPair = TypeKindPair(str, kind)
    }

    implicit class NatKindPairBuilder(kind: NatKind.type) {
      def `:` (x: Nat): TypeKindPair = TypeKindPair(x, kind)
      def `:` (str: String): TypeKindPair = TypeKindPair(str, kind)
    }

    implicit class AddressSpaceKindPairBuilder(kind: AddressSpaceKind.type) {
      def `:` (x: AddressSpace): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class FragmentKindPairBuilder(kind: FragmentKind) {
      def `:` (x: FragmentKind): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class MatrixLayoutKindPairBuilder(kind: MatrixLayout) {
      def `:` (x: MatrixLayout): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class NatToNatKindPairBuilder(kind: NatToNatKind.type) {
      def `:` (x: NatToNat): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class NatToDataKindPairBuilder(kind: NatToDataKind.type) {
      def `:` (x: NatToData): TypeKindPair = TypeKindPair(x, kind)
    }

    implicit class ValueTypePairBuilder(ty: Type) {
      def `:` (x: Expr): ValueTypePair = ValueTypePair(x, ty)
      def `:` (x: String): ValueTypePair = ValueTypePair(x, ty)
    }

    implicit class KindingEnvOps(kindingEnv: KindingEnv) {
      def |-(typeKindPair: TypeKindPair): KindingJudgement = KindingJudgement(kindingEnv, typeKindPair)
      def `,` (typeKindPair: TypeKindPair): ExtendedKindingEnv = ExtendedKindingEnv(kindingEnv, typeKindPair)

      def | (typingEnv: TypingEnv): Env = Env(kindingEnv, typingEnv)
    }

    implicit class TypingEnvOps(typingEnv: TypingEnv) {
      def `,` (valueTypePair: ValueTypePair): ExtendedTypingEnv = ExtendedTypingEnv(typingEnv, valueTypePair)
      def `,` (rhsTypingEnv: TypingEnv): UnionTypingEnv = UnionTypingEnv(typingEnv, rhsTypingEnv)
    }

    implicit class EnvOps(env: Env) {
      def |-(valueTypePair: ValueTypePair): TypeJudgement = TypeJudgement(env, valueTypePair)
    }

    case class TypeKindPair(ty: Any/* Type | DataType | ... | String */, kind: Any/* Kind | String */) {
      override def toString: String = {
        val kindString = kind match {
          case k: Kind[_, _] => toLaTeX(k)
          case _ => s"$kind"
        }
        val tyString = ty match {
          case t: Type => toLaTeX(t)
          case n: Nat => toLaTeX(n)
          case a: AddressSpace => toLaTeX(a)
          case ntn: NatToNat => toLaTeX(ntn)
          case ntd: NatToData => toLaTeX(ntd)
          case _ => s"$ty"
        }
        s"$tyString : $kindString"
      }
    }

    case class ValueTypePair(value: Any, ty: Any) {
      override def toString: String = {
        val valueString = value match {
          case e: Expr => toLaTeX(e)
          case _ => s"$value"
        }
        val tyString = ty match {
          case t: Type => toLaTeX(t)
          case n: Nat => toLaTeX(n)
          case a: AddressSpace => toLaTeX(a)
          case ntn: NatToNat => toLaTeX(ntn)
          case ntd: NatToData => toLaTeX(ntd)
          case _ => s"$ty"
        }
        s"$valueString : $tyString"
      }
    }

    case class KindingJudgement(env: KindingEnv, tk: TypeKindPair) {
      override def toString: String = s"$env \\vdash $tk"
    }

    case class TypeJudgement(env: Env, vt: ValueTypePair) {
      override def toString: String = s"$env \\vdash $vt"
    }

    implicit class SetOp(lhs: Any) {
      def in (rhs: Set): String = s"$lhs \\in $rhs"
      def notin (rhs: Set): String = s"$lhs \\not\\in $rhs"
    }

    implicit class To(lhs: String) {
      def to (rhs: String): String = s"$lhs \\to $rhs"
    }

    implicit class Spacing(lhs: Any) {
      def quad (rhs: Any): String = s"$lhs \\quad $rhs"
      def qquad (rhs: Any): String = s"$lhs \\qquad $rhs"
      def \\ (rhs: Any): String = s"$lhs \\\\ $rhs"
    }

    implicit class NatToNatBuilder(rhs: Nat) {
      def |->: (lhs: NatIdentifier): NatToNat = NatToNatLambda(lhs, rhs)
    }

    implicit class NatToDataBuilder(rhs: DataType) {
      def |->: (lhs: NatIdentifier): NatToData = NatToDataLambda(lhs, rhs)
    }

    implicit class Define(lhs: Any) {
      def ::= (rhs: Any): String = s"$lhs ::= $rhs"
    }

    implicit class Rule(lhs: Any) {
      def -------------------- (rhs: Any): String =  s"\\cfrac{$lhs}{$rhs}"
    }

    case class Row(strings: String*) {
      override def toString: String = strings.mkString("\\qquad\n")
    }

    case class Rows(rows: Row*) {
      override def toString: String = rows.mkString("\\\\[1em]\n")
    }

    sealed trait Set
    case class FiniteSet(members: Any*) extends Set {
      override def toString: String = members.map({
        case kind: Kind[_, _] => toLaTeX(kind)
        case m => m.toString
      }).mkString("\\{", ", ", "\\}")
    }
    case class NamedSet(name: String) extends Set {
      override def toString: String = name
    }

    sealed trait KindingEnv extends Set
    case class KindingEnvVar(name: String) extends KindingEnv {
      override def toString: String = name
    }
    case class ExtendedKindingEnv(env: KindingEnv, typeKindPair: TypeKindPair) extends KindingEnv {
      override def toString: String = s"$env, $typeKindPair"
    }

    sealed trait TypingEnv extends Set
    case class TypingEnvVar(name: String) extends TypingEnv {
      override def toString: String = name
    }
    case class ExtendedTypingEnv(env: TypingEnv, valueTypePair: ValueTypePair) extends TypingEnv {
      override def toString: String = s"$env, $valueTypePair"
    }
    case class UnionTypingEnv(lhs: TypingEnv, rhs: TypingEnv) extends TypingEnv {
      override def toString: String = s"$lhs, $rhs"
    }

    case class Env(delta: KindingEnv, gamma: TypingEnv) extends Set {
      override def toString: String = s"$delta \\mid $gamma"
    }

    def tid(name: String): TypeIdentifier = TypeIdentifier(name)
    def aid(name: String): AddressSpaceIdentifier = AddressSpaceIdentifier(name)
    def nid(name: String): NatIdentifier = NatIdentifier(name)
    def did(name: String): DataTypeIdentifier = DataTypeIdentifier(name)
    def ntnid(name: String): NatToNatIdentifier = NatToNatIdentifier(name)
    def ntdid(name: String): NatToDataIdentifier = NatToDataIdentifier(name)
    def fid(name: String): FragmentKindIdentifier = FragmentKindIdentifier(name)
    def mid(name: String): MatrixLayoutIdentifier = MatrixLayoutIdentifier(name)
  }
}
