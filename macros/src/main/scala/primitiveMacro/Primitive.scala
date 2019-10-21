package primitiveMacro

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox.Context

object Primitive {

  @compileTimeOnly("primitive macro")
  class primitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.primitive
  }

  class Impl(val c: blackbox.Context) {

    import c.universe._

    def primitive(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: ClassDef) :: Nil =>
          c.Expr(fromClassDef(cdef))
        case (cdef: ClassDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${fromClassDef(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a class definition")
      }
    }

    def fromClassDef: ClassDef => ClassDef = {
      case q"case class $name(..$params) extends $_ {..$body} " =>
        val r = q"""
            case class $name(..$params) extends Primitive {
              override def setType(t: Type): $name = ${name.asInstanceOf[c.TypeName].toTermName}(..${params.map({
                case q"$_ val $n: $_ " => n
                case q"$_ val $n: $_ = $_" => n
                case x => c.abort(c.enclosingPosition, s"expected a parameter, but got $x")})})
              ..$body
            }
         """
        // for debugging
        // c.error(c.enclosingPosition, s"generated $r")
        r.asInstanceOf[ClassDef]
      case _ => c.abort(c.enclosingPosition, "expected a case class extends Primitive")
    }
  }
}