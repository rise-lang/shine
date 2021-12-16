package meta.parser.DPIA

import meta.parser.DPIA.Decl.KindOrType
import meta.parser._
import meta.parser.DPIA.Kind.{AST => KindAST}
import meta.parser.rise.isWellKindedType

object isWellKindedDefinition {
  // check if given parameters and return type are well kinded, i.e.
  // that type-level parameters are declared with the correct kind before they are used
  def apply(scalaParams: Option[List[scala.meta.Term.Param]],
            params: Seq[Decl.AST.Param],
            returnType: Type.AST): Boolean = {
    import Decl.AST._
    var isWellKindedFlag = true
    // prepopulate the environment with all integers in the scala parameters
    val prepopulatedEnv = scalaParams.map(_.foldLeft(Map.empty[String, KindAST]) {
      case (env, param) => param.decltpe match {
        case Some(scala.meta.Type.Name("Int")) =>
          env.updated(param.name.value, KindAST.RiseKind(rise.Kind.AST.Nat))
        case _ => env
      }
    }).getOrElse(Map.empty[String, KindAST])
    // go left-to-right over the parameters ...
    val env = params.foldLeft(prepopulatedEnv) {
      // ... and add identifiers to the environment
      case (env, Param(Identifier(name), KindOrType.Kind(kind))) =>
        env.updated(name, kind)
      // ... or check if a parameter is well-kinded
      case (env, Param(Identifier(_), KindOrType.Type(typeAST))) =>
        if (!isWellKinded(typeAST, env, None))
          isWellKindedFlag = false // ... set flag if param is not well-kinded
        env
    }
    // ... finally check return type
    isWellKindedFlag && isWellKinded(returnType, env, None)
  }

  def isWellKinded(typeAST: Type.AST,             // the AST of the type that is checked
                   env: Map[String, KindAST],    // ... the kinding environment
                   variadicN: Option[String]      // ... an optional string for the number of types in a variadic type
                  ): Boolean = typeAST match {
    case Type.AST.ExpType(dataType, access) =>
      rise.isWellKindedType.kindOf(dataType, convertEnv(env), variadicN).isDefined && isWellKinded(access, env)
    case Type.AST.AccType(dataType) =>
      rise.isWellKindedType.kindOf(dataType, convertEnv(env), variadicN).isDefined
    case Type.AST.CommType => true
    case Type.AST.PairType(lhs, rhs) =>
      isWellKinded(lhs, env, variadicN) && isWellKinded(rhs, env, variadicN)
    case Type.AST.FunType(inT, outT) =>
      isWellKinded(inT, env, variadicN) && isWellKinded(outT, env, variadicN)
    case Type.AST.DepFunType(id, kind, t) =>
      if (env.isDefinedAt(id.name)) {
        // we forbid shadowing
        false
      } else {
        isWellKinded(t, env.updated(id.name, kind), variadicN)
      }
    case Type.AST.Identifier(name) =>
      env.contains(name)
    case Type.AST.VariadicType(n, ty) =>
      isWellKinded(ty, env, Some(n.name))
  }

  def isWellKinded(accessAST: Type.Access.AST, env: Map[String, KindAST]): Boolean = accessAST match {
    case Type.Access.AST.Identifier(name) => env.isDefinedAt(name)
    case Type.Access.AST.Read => true
    case Type.Access.AST.Write => true
  }

  def convertEnv(env: Map[String, KindAST]): Map[String, isWellKindedType.Kind] = env.flatMap {
    case (string, KindAST.RiseKind(riseKind)) =>
      Some((string, rise.isWellKindedType.DataTypeKind(riseKind)))
    case (string, KindAST.VariadicKind(n, KindAST.RiseKind(riseKind))) =>
      Some((string, rise.isWellKindedType.VariadicKind(n, riseKind)))
    case _ => None
  }
}
