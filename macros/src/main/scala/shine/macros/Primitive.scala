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

    def makeTraverseCall(v : Tree, name : TermName) : Tree => Option[Tree] = {
      case Ident(TypeName("DataType")) | Ident(TypeName("ScalarType")) |
           Ident(TypeName("BasicType"))                       => Some(fq"${name} <- $v.datatype($name)")
      case Ident(TypeName("Data"))                            => Some(fq"${name} <- $v.data($name)")
      case Ident(TypeName("Nat"))                             => Some(fq"${name} <- $v.nat($name)")
      case Ident(TypeName("NatIdentifier"))                   => Some(fq"${name} <- $v.nat($name)")
      case Ident(TypeName("NatToNat"))                        => Some(fq"${name} <- $v.natToNat($name)")
      case Ident(TypeName("NatToData"))                       => Some(fq"${name} <- $v.natToData($name)")
      case Ident(TypeName("AccessType"))                      => Some(fq"${name} <- $v.accessType($name)")
      case Ident(TypeName("AddressSpace"))                    => Some(fq"${name} <- $v.addressSpace($name)")
      // Phrase[ExpType]
      case AppliedTypeTree((Ident(TypeName("Phrase")), _))    => Some(fq"${name} <- $v.phrase($name)")
      // Vector[Phrase[ExpType]]
      case AppliedTypeTree((Ident(TypeName("Vector")),
      List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
           |   AppliedTypeTree((Ident(TypeName("Seq")),
      List(AppliedTypeTree((Ident(TypeName("Phrase")), _))))) => Some(fq"${name} <- $name.map($v.phrase(_))")
      case _ => None
    }

    def makeTraverse(name: TypeName,
                     additionalParams: List[ValDef],
                     params: List[ValDef]): Tree = {

      val v = q"v"
      val paramNames = params.map { case ValDef(_, name, _, _) => q"$name" }
      val additionalParamNames = additionalParams.map { case ValDef(_, name, _, _) => q"$name" }
      val forLoopBindings : List[Tree] = params.flatMap {
        case ValDef(_, name, tpt, _) => makeTraverseCall(v, name)(tpt)
      }
      val construct = if (additionalParamNames.isEmpty) q"new $name(..$paramNames)"
                      else q"new $name(..$additionalParamNames)(..$paramNames)"
      val forloop = if (forLoopBindings.isEmpty) q"monad.return_($construct)"
                    else q"for (..${forLoopBindings}) yield $construct"

      q"""
        override def traverse[M[_]]($v: shine.DPIA.Phrases.traverse.Traversal[M]): M[$name] = {
          import util.monad._
          implicit val monad: Monad[M] = implicitly($v.monad)
          $forloop
        }
      """
    }

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

    def makeXMLPrinter(name: TypeName,
                       additionalParams: List[ValDef],
                       params: List[ValDef]): Tree = {
      def makeAttributes(params: List[ValDef]): (List[ValDef], Tree) = {
        if (params.isEmpty) return (params, q"scala.xml.Null")
        params.head match {
          case ValDef(_, name, tpt, _) => tpt match {
            case Ident(TypeName("DataType")) | Ident(TypeName("ScalarType")) |
                 Ident(TypeName("BasicType")) | Ident(TypeName("Nat")) |
                 Ident(TypeName("NatToNat")) | Ident(TypeName("NatToData")) |
                 Ident(TypeName("AccessType")) | Ident(TypeName("AddressSpace"))
            =>
              val (list, next) = makeAttributes(params.tail)
              (list, q"""
                 scala.xml.Attribute(${name.toString},
                                     scala.xml.Text(
                                        shine.DPIA.Phrases.ToString($name)),
                                     $next)
               """)
            case _ => (params, q"scala.xml.Null")
          }
        }
      }

      def makeBody(params: List[ValDef]): List[Tree] = {
        params.map {
          case ValDef(_, name, tpt, _) =>

            val body = tpt match {
              // Phrase[ExpType]
              case AppliedTypeTree((Ident(TypeName("Phrase")), _)) =>
                q"shine.DPIA.Phrases.xmlPrinter($name)"
              // Vector[Phrase[ExpType]]
              case AppliedTypeTree((Ident(TypeName("Vector")),
              List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
                   |   AppliedTypeTree((Ident(TypeName("Seq")),
              List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
              =>
                q"$name.flatMap(shine.DPIA.Phrases.xmlPrinter(_)):_*"
              case _ =>
                q"scala.xml.Text(shine.DPIA.Phrases.ToString($name))"
            }
            q"""
               scala.xml.Elem(null, ${name.toString},
                              scala.xml.Null, scala.xml.TopScope,
                              minimizeEmpty = false, $body)
             """
        }
      }

      val lowerCaseName = makeLowerCaseName(name.toString)
      val (rest, attributes) = makeAttributes(params)
      val body = makeBody(rest)

      q"""
       override def xmlPrinter: scala.xml.Elem = {
         val attributes_ = $attributes
         val body_ = $body
         scala.xml.Elem(null, $lowerCaseName, attributes_, scala.xml.TopScope,
                        minimizeEmpty = false, (body_):_*)
       }
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
      val traverseMissing =
        body.collectFirst({ case DefDef(_, TermName("traverse"), _, _, _, _) => ()}).isEmpty
      val visitAndRebuildMissing =
        body.collectFirst({ case DefDef(_, TermName("visitAndRebuild"), _, _, _, _) => ()}).isEmpty
      val xmlPrinterMissing =
        body.collectFirst({ case DefDef(_, TermName("xmlPrinter"), _, _, _, _) => ()}).isEmpty

      val generated = q"""
          ${if (traverseMissing) makeTraverse(name, additionalParams, params) else q""}

          ${if (visitAndRebuildMissing)
        makeVisitAndRebuild(name, additionalParams, params)
      else q""}

          ${if (xmlPrinterMissing)
        makeXMLPrinter(name, additionalParams, params)
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