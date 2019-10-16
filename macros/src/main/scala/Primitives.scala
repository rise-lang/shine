package macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

object Primitives {
  @compileTimeOnly("expr macro")
  class expr extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.expr
  }

  class Impl(val c: Context) {
    import c.universe._

    def expr(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: ClassDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${classDef(Structure.fromClassDef(cdef))}; $md}")
        case (cdef: ClassDef) :: Nil =>
          c.Expr(classDef(Structure.fromClassDef(cdef)))
        case _ => c.abort(c.enclosingPosition, "expected class definition")
      }
    }

    case class Structure(name: c.TypeName,
                         tags: Params,
                         args: Params,
                         t: c.Tree,
                         parent: c.Tree,
                         body: Seq[c.Tree])

    object Structure {
      def fromClassDef: ClassDef => Structure = {
        case q"case class $name(...$param_lists) extends $parent { ..$body }"
        =>
          val (tags, args) = param_lists match {
            case a :: b :: Nil => (a, b)
            case b :: Nil => (Nil, b)
            case _ => c.abort(c.enclosingPosition, "expected expr [tags and] arguments")
          }
          args match {
            case (largs: Seq[ValDef]) :+ q"$_ val t: Option[$pt]" =>
              Structure(name, params(tags), params(largs), pt, parent, body)
            case _ => c.abort(c.enclosingPosition, "expected 'val t: Option[?]' as last argument")
          }
        case _ => c.abort(c.enclosingPosition, "expected case class extending a parent")
      }
    }

    type Params = Seq[(c.TermName, c.Tree)]
    def params(t: Seq[ValDef]): Params = {
      t.map({ case q"$_ val $x: $t" => (x, t) })
    }

    def classDef(s: Structure): ClassDef = {
      val parts =
        s.tags.map({ case (x, t) => q"val $x: $t" }) ++
          (s.parent match {
            case tq"Expr" | tq"PrimitiveExpr" =>
              s.args.map({ case (x, t) => q"val $x: $t" })
            case _ =>
              s.args.map({ case (x, t) => q"override val $x: $t" })
          }) :+ q"override val t: Option[${s.t}]"
      q"""
      final case class ${s.name}(..$parts) extends ${s.parent} {
        ${stem(s)}
        ${children(s)}
        ${rebuild(s)}
        ..${s.body}
      }
      """
    }

    def stem(s: Structure): DefDef = {
      val parts = s.name.toTermName +: s.tags.map({ case (x, _) => x })
      q"override def stem = Some(..$parts)"
    }

    def children(s: Structure): DefDef = {
      val parts = s.args.map({ case (x, _) => q"$x" }) :+ q"t"
      q"override def children = Seq(..$parts)"
    }

    def rebuild(s: Structure): DefDef = {
      val patParts = // TODO: type check when there is type erasure
        s.args.map({ case (x, t) => pq"$x: $t" }) :+ pq"t: ${tq"Option[${s.t}]"}"
      val parts =
        (s.tags ++ s.args).map({ case (x, _) => q"$x" }) :+ q"t"
      q"""
      override def rebuild: Seq[Any] => Expr = {
        case Seq(..$patParts) => ${s.name.toTermName}(..$parts)
      }
      """
    }
  }
}