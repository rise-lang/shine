package meta.parser.rise

import meta.parser._
import meta.parser.rise.Kind.{AST => KindAST}

object isWellKindedType {

  def apply(scalaParams: Option[List[scala.meta.Term.Param]],
            typeAST: Type.AST): Boolean = {
    import scala.meta._
    val env = scalaParams.getOrElse(List.empty).foldLeft[Map[String, Kind]](Map.empty){
      case (env, param) => param.decltpe match {
        case Some(t"Int") => env.updated(param.name.value, DataTypeKind(KindAST.Nat))
        case _ => env
      }
    }
    kindOf(typeAST, env, None).isDefined
  }

  sealed trait Kind
  case class  DataTypeKind(kind: KindAST)             extends Kind
  case object FunctionKind                            extends Kind
  case class  VariadicKind(n: String, kind: KindAST)  extends Kind

  def kindOf(typeAST: Type.AST,
             env: Map[String, Kind],
             variadicN: Option[String]): Option[Kind] = {
    import Type._
    typeAST match {
      case AST.Identifier(name) =>
        env.get(name)
      case AST.UnrolledIdentifier(name) =>
        env.get(name).flatMap {
          case VariadicKind(n, kind) =>
            variadicN.flatMap(m =>
              if (n == m) {
                Some(VariadicKind(n, kind))
              } else {
                None
              })
          case DataTypeKind(_) => None
          case FunctionKind => None
        }
      case AST.FunType(inT, outT) =>
        for {
          _ <- kindOf(inT, env, variadicN)
          _ <- kindOf(outT, env, variadicN)
        } yield FunctionKind
      case AST.DepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          for {
            _ <- kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
          } yield FunctionKind
        }
      case AST.ImplicitDepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          for {
            _ <- kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
          } yield FunctionKind
        }
      case AST.VariadicFunType(n, inTs, outT) =>
        for {
          k1 <- kindOf(n, env, None)
          _ <- kindOf(inTs, env, Some(n.name))
          _ <- kindOf(outT, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat)
        } yield FunctionKind
      case AST.VariadicDepFunType(n, id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          for {
            k1 <- kindOf(n, env, None)
            _ <- kindOf(t, env.updated(id.name, VariadicKind(n.name, kind)), variadicN)
            if k1 == DataTypeKind(KindAST.Nat)
          } yield FunctionKind
        }
      case AST.VectorType(size, elemType) =>
        for {
          k1 <- kindOf(size, env, variadicN)
          k2 <- kindOf(elemType, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Data)
        } yield DataTypeKind(KindAST.Data)
      case AST.IndexType(size) =>
        for {
          k <- kindOf(size, env, variadicN)
          if k == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Data)
      case AST.PairType(lhs, rhs) =>
        for {
          k1 <- kindOf(lhs, env, variadicN)
          k2 <- kindOf(rhs, env, variadicN)
          if k1 == DataTypeKind(KindAST.Data) && k2 == DataTypeKind(KindAST.Data)
        } yield DataTypeKind(KindAST.Data)
      case AST.DepPairType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
        }
      case AST.NatToDataApply(f, n) =>
        for {
          k1 <- kindOf(f, env, variadicN)
          k2 <- kindOf(n, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat2Data) && k2 == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Data)
      case AST.NatToDataLambda(id, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          for {
            k <- kindOf(t, env.updated(id.name, DataTypeKind(KindAST.Nat)), variadicN)
            if k == DataTypeKind(KindAST.Data)
          } yield DataTypeKind(KindAST.Nat2Data)
        }
      case AST.ArrayType(size, elemType) =>
        for {
          k1 <- kindOf(size, env, variadicN)
          k2 <- kindOf(elemType, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Data)
        } yield DataTypeKind(KindAST.Data)
      case AST.DepArrayType(size, fdt) =>
        for {
          k1 <- kindOf(size, env, variadicN)
          k2 <- kindOf(fdt, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat2Data)
        } yield DataTypeKind(KindAST.Data)
      case AST.FragmentType(n, m, k, elemType, fKind, mLayout) =>
        for {
          k1 <- kindOf(n, env, variadicN)
          k2 <- kindOf(m, env, variadicN)
          k3 <- kindOf(k, env, variadicN)
          k4 <- kindOf(elemType, env, variadicN)
          k5 <- kindOf(fKind, env, variadicN)
          k6 <- kindOf(mLayout, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat) &&
            k3 == DataTypeKind(KindAST.Nat) && k4 == DataTypeKind(KindAST.Data) &&
            k5 == DataTypeKind(KindAST.Fragment) && k6 == DataTypeKind(KindAST.MatrixLayout)
        } yield DataTypeKind(KindAST.Data)
      case AST.ManagedBufferType(dt) =>
        for {
          k1 <- kindOf(dt, env, variadicN)
          if k1 == DataTypeKind(KindAST.Data)
        } yield DataTypeKind(KindAST.Data)
      case _: AST.ScalarType | AST.NatType | _: AST.OpaqueType =>
        Some(DataTypeKind(KindAST.Data))
    }
  }

  def kindOf(natAST: Nat.AST,
             env: Map[String, Kind],
             variadicN: Option[String]
            ): Option[Kind] = {
    natAST match {
      case Nat.AST.Identifier(id) =>
        env.get(id)
      case Nat.AST.Number(_) =>
        Some(DataTypeKind(KindAST.Nat))
      case Nat.AST.BinaryOp(lhs, _, rhs) =>
        for {
          k1 <- kindOf(lhs, env, variadicN)
          k2 <- kindOf(rhs, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Nat)

      case Nat.AST.TernaryOp(_, thenN, elseN) =>
        for {
          k1 <- kindOf(thenN, env, variadicN)
          k2 <- kindOf(elseN, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Nat)

      case Nat.AST.Nat2NatApply(f, n) =>
        for {
          k1 <- kindOf(f, env, variadicN)
          k2 <- kindOf(n, env, variadicN)
          if k1 == DataTypeKind(KindAST.Nat2Nat) && k2 == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Nat)

      case Nat.AST.Sum(id, from, upTo, body) =>
        val nEnv = env.updated(id.name, DataTypeKind(KindAST.Nat))
        for {
          k1 <- kindOf(from, nEnv, variadicN)
          k2 <- kindOf(upTo, nEnv, variadicN)
          k3 <- kindOf(body, nEnv, variadicN)
          if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat) && k3 == DataTypeKind(KindAST.Nat)
        } yield DataTypeKind(KindAST.Nat)
    }
  }

  def kindOf(fragmentAST: Type.Fragment.AST,
             env: Map[String, Kind],
             variadicN: Option[String]
            ): Option[Kind] = {
    import Type._
    fragmentAST match {
      case Fragment.AST.Identifier(id) =>
        env.get(id)
      case Fragment.AST.ACC | Fragment.AST.A | Fragment.AST.B => Some(DataTypeKind(KindAST.Fragment))
    }
  }

  def kindOf(matrixLayout: Type.MatrixLayout.AST,
             env: Map[String, Kind],
             variadicN: Option[String]
            ): Option[Kind] = {
    import Type._
    matrixLayout match {
      case MatrixLayout.AST.Identifier(id) =>
        env.get(id)
      case MatrixLayout.AST.ROW_MAJOR |
           MatrixLayout.AST.COL_MAJOR |
           MatrixLayout.AST.NONE => Some(DataTypeKind(KindAST.MatrixLayout))
    }
  }
}
