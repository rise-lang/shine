package shine.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

object Primitive {
  @compileTimeOnly("ExpPrimitive macro")
  class expPrimitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.expPrimitive
  }

  @compileTimeOnly("AccPrimitive macro")
  class accPrimitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.accPrimitive
  }

  @compileTimeOnly("CommandPrimitive macro")
  class comPrimitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.comPrimitive
  }

  class Impl(val c: blackbox.Context) {
    import c.universe._

    def expPrimitive(annottees : c.Expr[Any]*): c.Expr[Any] = primitive(c => makePrimitiveClass(primitivesFromClassDef(c)))(annottees)
    def accPrimitive(annottees : c.Expr[Any]*): c.Expr[Any] = primitive(c => makePrimitiveClass(primitivesFromClassDef(c)))(annottees)
    def comPrimitive(annottees : c.Expr[Any]*): c.Expr[Any] = primitive(c => makePrimitiveClass(primitivesFromClassDef(c)))(annottees)

    def primitive(transform : ClassDef => ClassDef)(annottees: Seq[c.Expr[Any]]): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: ClassDef) :: Nil =>
          c.Expr(transform(cdef))
        case (cdef: ClassDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${transform(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a class definition")
      }
    }

    def makeLowerCaseName(s: String): String =
      s"${Character.toLowerCase(s.charAt(0))}${s.substring(1)}"

    def makeVisitAndRebuild(name: TypeName,
                            additionalParams: List[ValDef],
                            params: List[ValDef]): Tree = {
      val v = q"v"
      q"""
        override def visitAndRebuild(
          $v: shine.DPIA.Phrases.VisitAndRebuild.Visitor): $name
        = new ${Apply( additionalParams match {
        case List() => Ident(name)
        case _ => Apply(Ident(name), additionalParams.map {
          case ValDef(_, name, _, _) => q"$name"
        })
      }, params.map {
        case ValDef(_, name, tpt, _) => tpt match {
          case Ident(TypeName("DataType")) | Ident(TypeName("ScalarType")) |
               Ident(TypeName("BasicType"))     => q"$v.data($name)"
          case Ident(TypeName("Nat"))           => q"$v.nat($name)"
          case Ident(TypeName("NatIdentifier")) => q"$v.nat($name)"
          case Ident(TypeName("NatToNat"))      => q"$v.natToNat($name)"
          case Ident(TypeName("NatToData"))     => q"$v.natToData($name)"
          case Ident(TypeName("AccessType"))    => q"$v.access($name)"
          case Ident(TypeName("AddressSpace"))  => q"$v.addressSpace($name)"
          case Ident(TypeName("LocalSize"))  => q"$name.visitAndRebuild($v)"
          case Ident(TypeName("GlobalSize"))  => q"$name.visitAndRebuild($v)"
          // Phrase[ExpType]
          case AppliedTypeTree((Ident(TypeName("Phrase")), _)) =>
            q"shine.DPIA.Phrases.VisitAndRebuild($name, $v)"
          // Vector[Phrase[ExpType]]
          case AppliedTypeTree((Ident(TypeName("Vector")),
          List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
               |   AppliedTypeTree((Ident(TypeName("Seq")),
          List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
          =>
            q"$name.map(shine.DPIA.Phrases.VisitAndRebuild(_, $v))"
          case _ =>
            q"$name"
        }
      })}
      """
    }

    case class ClassInfo(name: TypeName,
                         additionalParams: List[ValDef],
                         params: List[ValDef],
                         body: List[Tree],
                         parents: List[Tree])

    def primitivesFromClassDef: ClassDef => ClassInfo = {
      case q"case class $name(..$params) extends { ..$_ } with ..$parents {..$body} " =>
        ClassInfo(
          name.asInstanceOf[c.TypeName],
          List(),
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]],
          parents.asInstanceOf[List[Tree]])
      case q"final case class $name(..$params) extends { ..$_ } with ..$parents {..$body} " =>
        ClassInfo(
          name.asInstanceOf[c.TypeName],
          List(),
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]],
          parents.asInstanceOf[List[Tree]])
      case q"""case class $name(..$additionalParams)
                               (..$params) extends { ..$_ } with ..$parents {..$body} """ =>
        ClassInfo(
          name.asInstanceOf[c.TypeName],
          additionalParams.asInstanceOf[List[ValDef]],
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]],
          parents.asInstanceOf[List[Tree]])
      case q"""final case class $name(..$additionalParams)
                                     (..$params) extends { ..$_ } with ..$parents {..$body} """ =>
        ClassInfo(
          name.asInstanceOf[c.TypeName],
          additionalParams.asInstanceOf[List[ValDef]],
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]],
          parents.asInstanceOf[List[Tree]])
      case _ =>
        c.abort(c.enclosingPosition, "expected a case class extends Primitive")
    }

    def makePrimitiveClass : ClassInfo => ClassDef = { case ClassInfo(name, additionalParams, params, body, parents) =>
      val visitAndRebuildMissing =
        body.collectFirst({ case DefDef(_, TermName("visitAndRebuild"), _, _, _, _) => ()}).isEmpty

      val generated = q"""
          ${if (visitAndRebuildMissing)
        makeVisitAndRebuild(name, additionalParams, params)
      else q""}
         """

      val expClass = (additionalParams match {
        case List() =>
          q"""
          final case class $name(..$params) extends {} with ..$parents {
            ..$body
            ..$generated
          }
         """
        case _ =>
          val newParams = params map {
            case ValDef(_, name, tpt, rhs) if rhs.isEmpty => q"val $name : $tpt"
            case ValDef(_, name, tpt, rhs) => q"val $name : $tpt = $rhs"
          }
          q"""
          final case class $name(..$additionalParams)
                                (..$newParams) extends {} with ..$parents {
            ..$body
            ..$generated
          }
         """
      }).asInstanceOf[ClassDef]

      c.info(c.enclosingPosition,
        s"generated `${name.toString}'\n$expClass", force = false)

      expClass
    }
  }
}