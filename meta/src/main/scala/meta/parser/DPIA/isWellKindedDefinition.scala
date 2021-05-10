package meta.parser.DPIA

import meta.parser.DPIA.Decl.KindOrType
import meta.parser._

object isWellKindedDefinition {

  def apply(scalaParams: Option[List[scala.meta.Term.Param]],
            params: Seq[Decl.AST.Param],
            returnType: Type.AST): Boolean = {
    import Decl.AST._
    var isWellKindedFlag = true
    val incompleteEnv = scalaParams.map(_.foldLeft(Map.empty[String, Kind.AST]) {
      case (env, param) => param.decltpe match {
        case Some(scala.meta.Type.Name("Int")) =>
          env.updated(param.name.value,Kind.AST.RiseKind(rise.Kind.AST.Nat))
        case _ => env
      }
    }).getOrElse(Map.empty[String, Kind.AST])
    val env = params.foldLeft(incompleteEnv) {
      case (env, Param(Identifier(name), KindOrType.Kind(kind))) =>
        env.updated(name, kind)
      case (env, Param(Identifier(_), KindOrType.Type(typeAST))) =>
        if (!isWellKinded(typeAST, env, None))
          isWellKindedFlag = false
        env
    }
    isWellKindedFlag && isWellKinded(returnType, env, None)
  }

  def isWellKinded(typeAST: Type.AST,
                   env: Map[String, Kind.AST],
                   variadicN: Option[String]): Boolean = {
    import Type._
    import rise.isWellKindedType._
    typeAST match {
      case AST.ExpType(dataType, access) =>
        val nenv = env.flatMap {
          case (string, DPIA.Kind.AST.RiseKind(riseKind)) =>
            Some((string, rise.isWellKindedType.DataTypeKind(riseKind)))
          case (string, DPIA.Kind.AST.VariadicKind(n, DPIA.Kind.AST.RiseKind(riseKind))) =>
            Some((string, rise.isWellKindedType.VariadicKind(n, riseKind)))
          case _ => None
        }
        kindOf(dataType, nenv, variadicN).isDefined && isWellKinded(access, env)
      case AST.AccType(dataType) =>
        val nenv = env.flatMap {
          case (string, DPIA.Kind.AST.RiseKind(riseKind)) =>
            Some((string, rise.isWellKindedType.DataTypeKind(riseKind)))
          case _ => None
        }
        kindOf(dataType, nenv, variadicN).isDefined
      case AST.CommType => true
      case AST.PairType(lhs, rhs) =>
        isWellKinded(lhs, env, variadicN) && isWellKinded(rhs, env, variadicN)
      case AST.FunType(inT, outT) =>
        isWellKinded(inT, env, variadicN) && isWellKinded(outT, env, variadicN)
      case AST.DepFunType(id, kind, t) =>
        if (env.isDefinedAt(id.name)) {
          // we forbid shadowing
          false
        } else {
          isWellKinded(t, env.updated(id.name, kind), variadicN)
        }
      case AST.Identifier(name) =>
        env.contains(name)
      case AST.VariadicType(n, ty) =>
        isWellKinded(ty, env, Some(n.name))
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
