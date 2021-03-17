package meta

import fastparse.MultiLineWhitespace._
import fastparse._

object TypeParser {

  sealed trait TypeAST
  object TypeAST {
    case class TypeIdentifier(name: String) extends TypeAST
    case class FunType(inT: TypeAST, outT: TypeAST) extends TypeAST
    case class DepFunType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST
    case class ImplicitDepFunType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST

    case class ScalarType(t: String) extends TypeAST
    case object NatType extends TypeAST
    case class VectorType(size: Nat, elemType: TypeAST) extends TypeAST
    case class IndexType(size: Nat) extends TypeAST
    case class PairType(lhs: TypeAST, rhs: TypeAST) extends TypeAST
    case class DepPairType(id: TypeIdentifier, kind: String, t: TypeAST) extends TypeAST
    case class NatToDataApply(f: TypeAST, n: Nat) extends TypeAST
    case class NatToDataLambda(id: TypeIdentifier, t: TypeAST) extends TypeAST
    case class ArrayType(size: Nat, elemType: TypeAST) extends TypeAST
    case class DepArrayType(size: Nat, fdt: TypeAST) extends TypeAST

    sealed trait Nat
    object Nat {
      case class Identifier(id: TypeIdentifier) extends Nat
      case class Number(n: String) extends Nat
      case class BinOp(lhs: Nat, op: String, rhs: Nat) extends Nat
      case class Nat2NatApply(f: TypeIdentifier, n: Nat) extends Nat
    }
  }

  def Identifier[_: P]: P[String] =
    P( CharPred(_.isLower).! ~~ (CharIn("0-9") | CharIn("a-z") | CharIn("A-Z")).rep.! ).
      map(t => t._1 ++ t._2).filter(!_.contains(' '))

  def TypeIdentifier[_: P]: P[TypeAST.TypeIdentifier] = P( Identifier ).map(TypeAST.TypeIdentifier)

  def FunType[_: P]: P[TypeAST.FunType] = P( NoCut(LeftTypeSignature) ~ "->" ~/ TypeSignature ).map(TypeAST.FunType.tupled)

  def Kind[_: P]: P[String] = P( "data".! | "address".! | "nat2nat".! | "nat2data".! | "nat".! )

  def IdentifierKindPair[_: P]: P[(TypeAST.TypeIdentifier, String)] = P( TypeIdentifier ~ ":" ~ Kind )

  def DepFunType[_: P]: P[TypeAST.DepFunType] =
    P( "(" ~ IdentifierKindPair ~ ")" ~ "->" ~/ TypeSignature ).map(TypeAST.DepFunType.tupled)

  def ImplicitDepFunType[_: P]: P[TypeAST.ImplicitDepFunType] =
    P( "{" ~ IdentifierKindPair ~ "}" ~ "->" ~/ TypeSignature ).map(TypeAST.ImplicitDepFunType.tupled)

  def ScalarType[_: P]: P[TypeAST.ScalarType] =
    P(  "bool".! | "int".! |
        "i8".! | "i16".! | "i32".! | "i64".! |
        "u8".! | "u16".! | "u32".! | "u64".! |
        "f16".! | "f32".! | "f64".! ).map(TypeAST.ScalarType)

  def NatType[_: P]: P[TypeAST.NatType.type] = P( "nat" ).map(_ => TypeAST.NatType)

  def VectorType[_: P]: P[TypeAST.VectorType] = P( "<" ~ Nat ~ ">" ~/ DataType  ).map(TypeAST.VectorType.tupled)

  def IndexType[_: P]: P[TypeAST.IndexType] = P( "idx[" ~ Nat ~ "]" ).map(TypeAST.IndexType)

  def PairType[_: P]: P[TypeAST.PairType] = P( "(" ~ NoCut(LeftDataType) ~ "," ~/ DataType ~ ")" ).map(TypeAST.PairType.tupled)

  def DepPairType[_: P]: P[TypeAST.DepPairType] =
    P( "(" ~ IdentifierKindPair ~ "**" ~/ DataType ~ ")" ).map(TypeAST.DepPairType.tupled)

  def NatToDataApply[_: P]: P[TypeAST.NatToDataApply] =
    P( NatToData ~ "(" ~ Nat ~ ")" ).map(TypeAST.NatToDataApply.tupled)

  def ArrayType[_: P]: P[TypeAST.ArrayType] =
    P( Nat ~ "." ~~ !"." ~/ DataType ).map(TypeAST.ArrayType.tupled)

  def DepArrayType[_: P]: P[TypeAST.DepArrayType] =
    P( Nat ~ ".." ~/ NatToData ).map(TypeAST.DepArrayType.tupled)

  def NatToDataLambda[_: P]: P[TypeAST.NatToDataLambda] =
    P( "(" ~ IdentifierKindPair.filter(_._2 == "nat").map(_._1) ~ "|->" ~/ DataType ~ ")" ).
      map(TypeAST.NatToDataLambda.tupled)

  def NatToData[_: P]: P[TypeAST] = P( TypeIdentifier | NatToDataLambda )

  def Nat[_: P]: P[TypeAST.Nat] = P( NatOp | LeftNat )
  def LeftNat[_: P]: P[TypeAST.Nat] = P( Number | NatIdentifier | Nat2NatApply | ("(" ~ Nat ~ ")") )
  def Number[_: P]: P[TypeAST.Nat.Number] = P( CharIn("0-9").rep(1).! ).map(TypeAST.Nat.Number)
  def NatIdentifier[_: P]: P[TypeAST.Nat.Identifier] = P( TypeIdentifier ).map(TypeAST.Nat.Identifier)
  def NatOp[_: P]: P[TypeAST.Nat.BinOp] = P( LeftNat ~ BinOp ~ Nat ).map(TypeAST.Nat.BinOp.tupled)
  def BinOp[_:P]: P[String] = P( "+".! | "-".! | "*".! | "^".! )
  def Nat2NatApply[_: P]: P[TypeAST.Nat.Nat2NatApply] =
    P( TypeIdentifier ~ "(" ~ Nat ~ ")" ).map(TypeAST.Nat.Nat2NatApply.tupled)

  def LeftDataType[_: P]: P[TypeAST] =
    P(  ScalarType | NatType | IndexType | VectorType | DepArrayType |
        ArrayType | DepPairType | NatToDataApply | TypeIdentifier | ("(" ~ DataType ~ ")") )

  def DataType[_: P]: P[TypeAST] =
    P( PairType | LeftDataType )

  // Types that can appear at the left of an function arrow
  def LeftTypeSignature[_: P]: P[TypeAST] = P( DataType | ("(" ~ TypeSignature ~ ")") )

  def TypeSignature[_: P]: P[TypeAST] = P( FunType | DepFunType | ImplicitDepFunType | LeftTypeSignature )

  def PrimitiveDeclaration[_: P]: P[(String, TypeAST)] =
    P( Identifier ~ ":" ~ TypeSignature ~ End )

  // def PrimitiveDeclarations[_: P]: P[Seq[(String, TypeAST)]] = P( PrimitiveDeclaration.rep(1) )

  sealed trait Explicitness
  case object Explicit extends Explicitness
  case object Implicit extends Explicitness

//  def isWellFormedType(typeAST: TypeAST, env: Map[TypeAST.Identifier, (String, Explicitness)]): Boolean = {
//    typeAST match {
//      case TypeAST.Identifier(name) =>
//      case TypeAST.FunType(inT, outT) => isWellFormedType(inT, env) && isWellFormedType(outT, env)
//      case TypeAST.DepFunType(id, kind, t) =>
//
//      case TypeAST.ImplicitDepFunType(id, kind, t) =>
//      case TypeAST.ScalarType(t) =>
//      case TypeAST.NatType =>
//      case TypeAST.VectorType(size, elemType) =>
//      case TypeAST.IndexType(size) =>
//      case TypeAST.PairType(lhs, rhs) =>
//      case TypeAST.DepPairType(id, kind, t) =>
//      case TypeAST.NatToDataApply(f, n) =>
//      case TypeAST.NatToDataLambda(id, t) =>
//      case TypeAST.ArrayType(size, elemType) =>
//      case TypeAST.DepArrayType(size, fdt) =>
//    }
//  }

  def kindName(kind: String): String = kind match {
    case "nat" => "Nat"
    case "data" => "DataType"
    case "nat2nat" => "NatToNat"
    case "nat2data" => "NatToData"
  }

  def generateTypeScheme(typeAST: TypeAST, env: Map[TypeAST.TypeIdentifier, String]): scala.meta.Term = {
    import scala.meta._
    typeAST match {
      case id@TypeAST.TypeIdentifier(name) =>
        assert(env.contains(id), s"$id is not in $env")
        Term.Name(name)
      case TypeAST.FunType(inT, outT) =>
        q"(${generateTypeScheme(inT, env)}) ->: (${generateTypeScheme(outT, env)})"
      case TypeAST.DepFunType(id, kind, t) =>
        q"expl((${Term.Name(id.name)}: ${Type.Name(kindName(kind))}) => ${generateTypeScheme(t, env.updated(id, kind))})"
      case TypeAST.ImplicitDepFunType(id, kind, t) =>
        q"impl((${Term.Name(id.name)}: ${Type.Name(kindName(kind))}) => ${generateTypeScheme(t, env.updated(id, kind))})"
      case TypeAST.ScalarType(t) =>
        t.parse[Term].get
      case TypeAST.NatType =>
        q"rise.core.types.NatType"
      case TypeAST.VectorType(size, elemType) =>
        q"rise.core.types.VectorType(${generateNat(size, env)}, ${generateTypeScheme(elemType, env)})"
      case TypeAST.IndexType(size) =>
        q"rise.core.types.IndexType(${generateNat(size, env)})"
      case TypeAST.PairType(lhs, rhs) =>
        q"rise.core.types.PairType(${generateTypeScheme(lhs, env)}, ${generateTypeScheme(rhs, env)})"
      case TypeAST.DepPairType(id, kind, t) => kind match {
        case "nat" =>
          q"Nat `**` ((${Term.Name(id.name)}: Nat) => ${generateTypeScheme(t, env.updated(id, kind))})"
        case _ => ???
      }
      case TypeAST.NatToDataApply(f, n) =>
        q"rise.core.types.NatToDataApply(${generateTypeScheme(f, env)}, ${generateNat(n, env)})"
      case TypeAST.NatToDataLambda(id, t) =>
        q"n2dtFun((${Term.Name(id.name)}: NatIdentifier) => ${generateTypeScheme(t, env.updated(id, "nat"))})"
      case TypeAST.ArrayType(size, elemType) =>
        q"rise.core.types.ArrayType(${generateNat(size, env)}, ${generateTypeScheme(elemType, env)})"
      case TypeAST.DepArrayType(size, fdt) =>
        q"rise.core.types.DepArrayType(${generateNat(size, env)}, ${generateTypeScheme(fdt, env)})"
    }
  }

  def generateNat(n: TypeAST.Nat, env: Map[TypeAST.TypeIdentifier, String]): scala.meta.Term = {
    import scala.meta._
    n match {
      case TypeAST.Nat.Identifier(id) =>
        assert(env.contains(id), s"$id is not in $env")
        Term.Name(id.name)
      case TypeAST.Nat.Number(n) =>
        n.parse[Term].get
      case TypeAST.Nat.BinOp(lhs, op, rhs) =>
        q"${generateNat(lhs, env)} ${Term.Name(op)} ${generateNat(rhs, env)}"
      case TypeAST.Nat.Nat2NatApply(f, n) =>
        q"${generateTypeScheme(f, env)}(${generateNat(n, env)})"
    }
  }

  def generateCaseClassAndObject(name: String, typeSignature: TypeAST): scala.meta.Defn.Object = {
    import scala.meta._
    val className = name + "_class"
    val makeInstance = q"${Term.Name(className)}()"
    val generated = q"""
      object ${Term.Name{name}}  extends Builder {
        final case class ${Type.Name(className)}()
        (override val t: rise.core.types.Type = rise.core.types.TypePlaceholder)
          extends rise.core.Primitive {
        override val name: String = ${Lit.String(name)}
        override def setType(ty: rise.core.types.Type): ${Type.Name(className)} =
          $makeInstance(ty)
        override def typeScheme: rise.core.types.Type =
          ${TypeParser.generateTypeScheme(typeSignature, Map.empty)}

        override def equals(obj: Any): Boolean = obj match {
          case p: ${Type.Name(className)} => p.t =~= t
          case _ => false
        }
      }

        override def primitive: ${Type.Name(className)} = $makeInstance()
        override def apply: rise.core.DSL.ToBeTyped[${Type.Name(className)}] =
          rise.core.DSL.toBeTyped($makeInstance())
        override def unapply(arg: rise.core.Expr): Boolean = arg match {
          case _: ${Type.Name(className)} => true
          case _ => false
        }
      }"""
   generated
  }
}

object main extends App {
  val rise = os.pwd/"src"/"main"/"scala"/"rise"
  os.walk.stream(rise).filter(_.ext == "rise").foreach(path => {
    var generated: scala.meta.Term.Block = {
      import scala.meta._
      q"{}"
    }

    os.read(path).split("def").tail.foreach(definition => {
      parse(definition.trim, TypeParser.PrimitiveDeclaration(_)) match {
        case failure: Parsed.Failure =>
          println(s"Failed to parse `${failure.extra.input}'")
          println(s"  $failure")
        case Parsed.Success((name, typeSignature), _) =>
          val generatedClass = TypeParser.generateCaseClassAndObject(name, typeSignature)
          import scala.meta._
          generated =
            q"""{
            ..${generated.stats} ;
            $generatedClass
            }
           """
      }
    })

    println(generated)
  })
}
