package meta.parser.rise

import meta.parser._

object isWellKindedType {

  def apply(typeAST: Type.AST): Boolean = {
    kindOf(typeAST, Map.empty).isDefined
  }

  def kindOf(typeAST: Type.AST,
             env: Map[String, Kind.AST]): Option[Kind.AST] = {
    import Type._
    typeAST match {
      case AST.Identifier(name) =>
        env.get(name)
      case AST.FunType(inT, outT) =>
        for {
          _ <- kindOf(inT, env)
          _ <- kindOf(outT, env)
        } yield Kind.AST.Function
      case AST.DepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id.name, kind))
        }
      case AST.ImplicitDepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id.name, kind))
        }
      case AST.VectorType(size, elemType) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(elemType, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Data
        } yield Kind.AST.Data
      case AST.IndexType(size) =>
        for {
          k <- kindOf(size, env)
          if k == Kind.AST.Nat
        } yield Kind.AST.Data
      case AST.PairType(lhs, rhs) =>
        for {
          k1 <- kindOf(lhs, env)
          k2 <- kindOf(rhs, env)
          if k1 == Kind.AST.Data && k2 == Kind.AST.Data
        } yield Kind.AST.Data
      case AST.DepPairType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          kindOf(t, env.updated(id.name, kind))
        }
      case AST.NatToDataApply(f, n) =>
        for {
          k1 <- kindOf(f, env)
          k2 <- kindOf(n, env)
          if k1 == Kind.AST.Nat2Data && k2 == Kind.AST.Nat
        } yield Kind.AST.Data
      case AST.NatToDataLambda(id, t) =>
        if (env.isDefinedAt(id.name)) {
          None // we forbid shadowing
        } else {
          for {
            k <- kindOf(t, env.updated(id.name, Kind.AST.Nat))
            if k == Kind.AST.Data
          } yield Kind.AST.Nat2Data
        }
      case AST.ArrayType(size, elemType) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(elemType, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Data
        } yield Kind.AST.Data
      case AST.DepArrayType(size, fdt) =>
        for {
          k1 <- kindOf(size, env)
          k2 <- kindOf(fdt, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Nat2Data
        } yield Kind.AST.Data
      case AST.FragmentType(n, m, k, elemType, fKind, mLayout) =>
        for {
          k1 <- kindOf(n, env)
          k2 <- kindOf(m, env)
          k3 <- kindOf(k, env)
          k4 <- kindOf(elemType, env)
          k5 <- kindOf(fKind, env)
          k6 <- kindOf(mLayout, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Nat && k3 == Kind.AST.Nat && k4 == Kind.AST.Data &&
            k5 == Kind.AST.Fragment && k6 == Kind.AST.MatrixLayout
        } yield Kind.AST.Data
      case _: AST.ScalarType | AST.NatType =>
        Some(Kind.AST.Data)
    }
  }

  def kindOf(natAST: Nat.AST,
             env: Map[String, Kind.AST]
            ): Option[Kind.AST] = {
    natAST match {
      case Nat.AST.Identifier(id) =>
        env.get(id)
      case Nat.AST.Number(_) =>
        Some(Kind.AST.Nat)
      case Nat.AST.BinaryOp(lhs, _, rhs) =>
        for {
          k1 <- kindOf(lhs, env)
          k2 <- kindOf(rhs, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Nat
        } yield Kind.AST.Nat

      case Nat.AST.TernaryOp(_, thenN, elseN) =>
        for {
          k1 <- kindOf(thenN, env)
          k2 <- kindOf(elseN, env)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Nat
        } yield Kind.AST.Nat

      case Nat.AST.Nat2NatApply(f, n) =>
        for {
          k1 <- kindOf(f, env)
          k2 <- kindOf(n, env)
          if k1 == Kind.AST.Nat2Nat && k2 == Kind.AST.Nat
        } yield Kind.AST.Nat

      case Nat.AST.Sum(id, from, upTo, body) =>
        val nEnv = env.updated(id.name, Kind.AST.Nat)
        for {
          k1 <- kindOf(from, nEnv)
          k2 <- kindOf(upTo, nEnv)
          k3 <- kindOf(body, nEnv)
          if k1 == Kind.AST.Nat && k2 == Kind.AST.Nat && k3 == Kind.AST.Nat
        } yield Kind.AST.Nat
    }
  }

  def kindOf(fragmentAST: Type.Fragment.AST,
             env: Map[String, Kind.AST]
            ): Option[Kind.AST] = {
    import Type._
    fragmentAST match {
      case Fragment.AST.Identifier(id) =>
        env.get(id)
      case Fragment.AST.ACC | Fragment.AST.A | Fragment.AST.B => Some(Kind.AST.Fragment)
    }
  }

  def kindOf(matrixLayout: Type.MatrixLayout.AST,
             env: Map[String, Kind.AST]
            ): Option[Kind.AST] = {
    import Type._
    matrixLayout match {
      case MatrixLayout.AST.Identifier(id) =>
        env.get(id)
      case MatrixLayout.AST.ROW_MAJOR |
           MatrixLayout.AST.COL_MAJOR |
           MatrixLayout.AST.NONE => Some(Kind.AST.MatrixLayout)
    }
  }
}
