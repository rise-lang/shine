package rise.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

// scalastyle:off indentation
object Primitive {
  val verbose = false

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("primitive macro")
  class primitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro PrimitiveImpl.prim
  }

  class PrimitiveImpl(val c: blackbox.Context) {
    import c.universe._

    def prim(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (mdef: ModuleDef) :: Nil =>
          c.Expr(fromModuleDef(mdef))
        case (cdef: ClassDef) :: Nil =>
          c.Expr(fromClassDef(cdef))
        case _ => c.abort(c.enclosingPosition,
          "expected an object or class definition")
      }
    }

    def fromModuleDef: ModuleDef => Tree = {
      case q"object ${name: TermName} extends Primitive with Builder { $typeScheme }" =>
        makePrimitiveClassAndObject(name.toString, typeScheme)
      case _ =>
        c.abort(c.enclosingPosition, "expected a matching definition:\n" +
          "object primitive extends Primitive with Builder { typeScheme }\n")
    }

    def fromClassDef: ClassDef => Tree = {
      case q"case class ${name: TypeName}(..$params) extends Primitive with Builder { $typeScheme }" =>
        makePrimitiveClass(name.toString, params, typeScheme)
      case _ =>
        c.abort(c.enclosingPosition, "expected a matching definition:\n" +
          "case class primitive(params) extends Primitive with Builder { typeScheme }\n")
    }

    def makePrimitiveClassAndObject(name: String, typeScheme: Tree): Tree = {
      val className = name + "_class"
      val makeInstance = q"${TermName(className)}()"

      val generated = q"""
        final case class ${TypeName(className)}()
          (override val t: rise.core.types.Type =
              rise.core.types.TypePlaceholder) extends Primitive
        {
          override val name: String = $name
          override def setType(ty: rise.core.types.Type): ${TypeName(className)} =
            $makeInstance(ty)
          override def typeScheme: rise.core.types.Type = $typeScheme

          override def equals(obj: Any) = obj match {
            case p: ${TypeName(className)} => ${TermName("p")}.t =~= t
            case _ => false
          }
        }

        object ${TermName{name}}  extends Builder {
          override def primitive: ${TypeName(className)} = $makeInstance()
          override def apply: rise.core.DSL.ToBeTyped[${TypeName(className)}] =
            rise.core.DSL.toBeTyped($makeInstance())
          override def unapply(arg: rise.core.Expr): Boolean = arg match {
            case _: ${TypeName(className)} => true
            case _ => false
          }
          override def toString: String = $name
        }
        """
      if (verbose) {
        c.info(c.enclosingPosition,
          s"generated `${name.toString}'\n$generated", force = false)
      }
      generated
    }

    def makePrimitiveClass(name: String,
                           params: List[ValDef],
                           typeScheme: Tree): Tree = {
      val className = name + "_class"
      val makeInstance = q"${TermName(className)}(..${getArgs(params)})"

      val generated = q"""
        final case class ${TypeName(className)}(..$params)
            (override val t: rise.core.types.Type =
                rise.core.types.TypePlaceholder) extends Primitive
        {
          override val name: String = $name
          override def setType(ty: rise.core.types.Type): ${TypeName(className)} =
            $makeInstance(ty)
          override def typeScheme: rise.core.types.Type = $typeScheme

          override def equals(obj: Any) = obj match {
            case other: ${TypeName(className)} =>
              ${makeComparisonChain(TermName("other"), getArgs(params))} &&
              (${TermName("other")}.t =~= t)
            case _ => false
          }
        }

        final case class ${TypeName(name)}(..$params) extends Builder {
          override def primitive: ${TypeName(className)} = $makeInstance()
          override def apply: rise.core.DSL.ToBeTyped[${TypeName(className)}] =
            rise.core.DSL.toBeTyped($makeInstance())
          override def toString: String = $name
        }

        object ${TermName(name)} {
          def unapply(arg: rise.core.Expr): Option[..${getTypes(params)}] = arg match {
            case p: ${TypeName(className)} =>
              Some(..${makeMemberAccess(TermName("p"), getArgs(params))})
            case _ => None
          }
        }
        """
      if (verbose) {
        c.info(c.enclosingPosition,
          s"generated `${name.toString}'\n$generated", force = false)
      }
      generated
    }

    def getArgs(p: List[ValDef]): Seq[TermName] =
      p.map({
        case ValDef(_, tn, _, _) => tn
      })

    def getTypes(p: List[ValDef]): Seq[Tree] =
      p.map({
        case ValDef(_, _, ty, _) => ty
      })

    def makeComparisonChain(other: TermName, props: Seq[TermName]): Tree = {
      props match {
        case Seq() => q"true"
        case Seq(head, tail @ _*) =>
          q"($other.$head == $head) && ${
            makeComparisonChain(other, tail)}"
      }
    }

    def makeMemberAccess(a: TermName, props: Seq[TermName]): Seq[Tree] = {
      props match {
        case Seq()      => Seq(q"")
        case Seq(prop)  => Seq(q"$a.$prop")
        case Seq(head, tail @ _*) =>
          q"$a.$head" +: makeMemberAccess(a, tail)
      }
    }
  }
}
// scalastyle:on indentation
