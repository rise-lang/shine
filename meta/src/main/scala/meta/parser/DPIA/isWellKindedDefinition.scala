package meta.parser.DPIA

import meta.parser._

object isWellKindedDefinition {

  def apply(params: Seq[Phrase.AST.Param], returnType: Type.AST): Boolean = {
    import Phrase.AST._
    var isWellKindedFlag = true
    val env = params.foldLeft(Map.empty[String, Kind.AST]) {
      case (env, Param(Identifier(name), Left(kind))) =>
        env.updated(name, kind)
      case (env, Param(Identifier(_), Right(typeAST))) =>
        if (!isWellKinded(typeAST, env)) isWellKindedFlag = false
        env
    }
    isWellKindedFlag && isWellKinded(returnType, env)
  }

  def isWellKinded(typeAST: Type.AST, env: Map[String, Kind.AST]): Boolean = {
    import Type._
    import rise.isWellKindedType._
    typeAST match {
      case AST.ExpType(dataType, access) =>
        val nenv = env.flatMap {
          case (string, DPIA.Kind.AST.RiseKind(riseKind)) =>
            Some((string, riseKind))
          case _ => None
        }
        kindOf(dataType, nenv).isDefined && isWellKinded(access, env)
      case AST.AccType(dataType) =>
        val nenv = env.flatMap {
          case (string, DPIA.Kind.AST.RiseKind(riseKind)) =>
            Some((string, riseKind))
          case _ => None
        }
        kindOf(dataType, nenv).isDefined
      case AST.CommType => true
      case AST.PairType(lhs, rhs) =>
        isWellKinded(lhs, env) && isWellKinded(rhs, env)
      case AST.FunType(inT, outT) =>
        isWellKinded(inT, env) && isWellKinded(outT, env)
      case AST.DepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          // we forbid shadowing
          false
        } else {
          isWellKinded(t, env.updated(id.name, kind))
        }
      case AST.Identifier(name) =>
        env.contains(name)
    }
  }

  def isWellKinded(accessAST: Type.Access.AST, env: Map[String, Kind.AST]): Boolean = {
    import Type.Access._
    accessAST match {
      case AST.Identifier(name) => env.isDefinedAt(name)
      case AST.Read => true
      case AST.Write => true
    }
  }
}
