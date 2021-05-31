package meta.parser.rise

import meta.parser._
import meta.parser.rise.Kind.{AST => KindAST}

object isWellKindedType {
  // check if given type AST is well kinded, i.e.
  // that type-level parameters are declared with the correct kind before they are used
  def apply(scalaParams: Option[List[scala.meta.Term.Param]],
            typeAST: Type.AST): Boolean = {
    import scala.meta._
    // prepopulate the environment with all integers in the scala parameters
    val env = scalaParams.getOrElse(List.empty).foldLeft[Map[String, Kind]](Map.empty){
      case (env, param) => param.decltpe match {
        case Some(t"Int") => env.updated(param.name.value, DataTypeKind(KindAST.Nat))
        case _ => env
      }
    }
    kindOf(typeAST, env, None).isDefined
  }

  // possible kinds used in well kindness check
  sealed trait Kind
  case class  DataTypeKind(kind: KindAST)             extends Kind
  case object FunctionKind                            extends Kind
  case class  VariadicKind(n: String, kind: KindAST)  extends Kind

  def kindOf(typeAST: Type.AST,           // the AST of the type that is checked
             env: Map[String, Kind],      // ... the kinding environment
             variadicN: Option[String]    // ... an optional string for the number of types in a variadic type
            ): Option[Kind] = typeAST match {
    case Type.AST.Identifier(name) =>
      env.get(name)
    case Type.AST.UnrolledIdentifier(name) =>
      env.get(name).flatMap {         // the kind of the identifier must be a ...
        case VariadicKind(n, kind) => // ... variadic kind
          variadicN.flatMap(m =>      // Then check that a variadic number is given ...
            if (n == m) {             // ... and that it is matching
              Some(VariadicKind(n, kind))
            } else {
              None
            })
        case DataTypeKind(_) => None
        case FunctionKind => None
      }
    case Type.AST.FunType(inT, outT) =>
      for {
        _ <- kindOf(inT, env, variadicN)
        _ <- kindOf(outT, env, variadicN)
      } yield FunctionKind
    case Type.AST.DepFunType(id, kind, t) =>
      if (env.isDefinedAt(id.name)) {
        None // we forbid shadowing
      } else {
        for {
          _ <- kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
        } yield FunctionKind
      }
    case Type.AST.ImplicitDepFunType(id, kind, t) =>
      if (env.isDefinedAt(id.name)) {
        None // we forbid shadowing
      } else {
        for {
          _ <- kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
        } yield FunctionKind
      }
    case Type.AST.VariadicFunType(n, inTs, outT) =>
      for {
        k1 <- kindOf(n, env, None)
        _ <- kindOf(inTs, env, Some(n.name)) // pass the variadic number down when checking the kind of inTs
        _ <- kindOf(outT, env, variadicN)
        if k1 == DataTypeKind(KindAST.Nat)
      } yield FunctionKind
    case Type.AST.VariadicDepFunType(n, id, kind, t) =>
      if (env.isDefinedAt(id.name)) {
        None // we forbid shadowing
      } else {
        for {
          k1 <- kindOf(n, env, None)
          _ <- kindOf(t, env.updated(id.name, VariadicKind(n.name, kind)), variadicN)
          if k1 == DataTypeKind(KindAST.Nat)
        } yield FunctionKind
      }
    case Type.AST.VectorType(size, elemType) =>
      for {
        k1 <- kindOf(size, env, variadicN)
        k2 <- kindOf(elemType, env, variadicN)
        if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Data)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.IndexType(size) =>
      for {
        k <- kindOf(size, env, variadicN)
        if k == DataTypeKind(KindAST.Nat)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.PairType(lhs, rhs) =>
      for {
        k1 <- kindOf(lhs, env, variadicN)
        k2 <- kindOf(rhs, env, variadicN)
        if k1 == DataTypeKind(KindAST.Data) && k2 == DataTypeKind(KindAST.Data)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.DepPairType(id, kind, t) =>
      if (env.isDefinedAt(id.name)) {
        None // we forbid shadowing
      } else {
        kindOf(t, env.updated(id.name, DataTypeKind(kind)), variadicN)
      }
    case Type.AST.NatToDataApply(f, n) =>
      for {
        k1 <- kindOf(f, env, variadicN)
        k2 <- kindOf(n, env, variadicN)
        if k1 == DataTypeKind(KindAST.Nat2Data) && k2 == DataTypeKind(KindAST.Nat)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.NatToDataLambda(id, t) =>
      if (env.isDefinedAt(id.name)) {
        None // we forbid shadowing
      } else {
        for {
          k <- kindOf(t, env.updated(id.name, DataTypeKind(KindAST.Nat)), variadicN)
          if k == DataTypeKind(KindAST.Data)
        } yield DataTypeKind(KindAST.Nat2Data)
      }
    case Type.AST.ArrayType(size, elemType) =>
      for {
        k1 <- kindOf(size, env, variadicN)
        k2 <- kindOf(elemType, env, variadicN)
        if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Data)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.DepArrayType(size, fdt) =>
      for {
        k1 <- kindOf(size, env, variadicN)
        k2 <- kindOf(fdt, env, variadicN)
        if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat2Data)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.FragmentType(n, m, k, elemType, fKind, mLayout) =>
      for {
        k1 <- kindOf(n, env, variadicN)
        k2 <- kindOf(m, env, variadicN)
        k3 <- kindOf(k, env, variadicN)
        k4 <- kindOf(elemType, env, variadicN)
        k5 <- kindOf(fKind, env)
        k6 <- kindOf(mLayout, env)
        if k1 == DataTypeKind(KindAST.Nat) && k2 == DataTypeKind(KindAST.Nat) &&
          k3 == DataTypeKind(KindAST.Nat) && k4 == DataTypeKind(KindAST.Data) &&
          k5 == DataTypeKind(KindAST.Fragment) && k6 == DataTypeKind(KindAST.MatrixLayout)
      } yield DataTypeKind(KindAST.Data)
    case Type.AST.ManagedBufferType(dt) =>
      for {
        k1 <- kindOf(dt, env, variadicN)
        if k1 == DataTypeKind(KindAST.Data)
      } yield DataTypeKind(KindAST.Data)
    case _: Type.AST.ScalarType | Type.AST.NatType | _: Type.AST.OpaqueType =>
      Some(DataTypeKind(KindAST.Data))
  }

  def kindOf(natAST: Nat.AST,
             env: Map[String, Kind],
             variadicN: Option[String]
            ): Option[Kind] = natAST match {
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

  def kindOf(fragmentAST: Type.Fragment.AST, env: Map[String, Kind]): Option[Kind] = fragmentAST match {
    case Type.Fragment.AST.Identifier(id) => env.get(id)
    case Type.Fragment.AST.ACC | Type.Fragment.AST.A | Type.Fragment.AST.B => Some(DataTypeKind(KindAST.Fragment))
  }

  def kindOf(matrixLayout: Type.MatrixLayout.AST, env: Map[String, Kind]): Option[Kind] = matrixLayout match {
    case Type.MatrixLayout.AST.Identifier(id) => env.get(id)
    case Type.MatrixLayout.AST.ROW_MAJOR |
         Type.MatrixLayout.AST.COL_MAJOR |
         Type.MatrixLayout.AST.NONE => Some(DataTypeKind(KindAST.MatrixLayout))
  }
}
