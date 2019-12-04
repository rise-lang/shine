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
    import c.universe.Flag._

    def primitive(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: ClassDef) :: Nil =>
          c.Expr(fromClassDef(cdef))
        case (cdef: ClassDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${fromClassDef(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a class definition")
      }
    }

    def makeStringName(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)

    def makeArgs(p: Seq[Tree]): Seq[TermName] = p.map({
      case q"$_ val $n: $_ " => n
      case q"$_ val $n: $_ = $_" => n
      case x => c.abort(c.enclosingPosition, s"expected a parameter, but got $x")})
      .asInstanceOf[Seq[TermName]]

    def makeChain(a: TermName, props: Seq[TermName]): Tree =
      if (props.isEmpty) q"true" else q"(${a}.${props.head} == ${props.head}) && ${makeChain(a, props.tail)}"

    def fromClassDef: ClassDef => ClassDef = {
      case q"case class $name(..$params)(..$_) extends $_ {..$body} " =>
        val r = q"""
            case class $name(..$params)(override val t: Type = TypePlaceholder) extends Primitive {
              override def equals(obj: Any) = obj match {
                case ${TermName("p")} : ${name.asInstanceOf[c.TypeName]} =>
                  ${makeChain(TermName("p"), makeArgs(params.asInstanceOf[Seq[Tree]]))} && (${TermName("p")}.t =~= t)
                case _ => false
              }
              override val name: String = ${Literal(Constant(makeStringName(name.toString())))}
              override def setType(t: Type): $name =
                ${name.asInstanceOf[c.TypeName].toTermName}(..${makeArgs(params.asInstanceOf[Seq[Tree]])})(t)
              ..$body
            }
         """.asInstanceOf[ClassDef]
        // the Scala macro bug: https://github.com/scala/bug/issues/10589
        // the workaround: https://stackoverflow.com/questions/52222122/workaround-for-scala-macro-annotation-bug
        val noCaseAccessorForType = r match {
          case ClassDef(mods, name, tparams, impl @ Template(parents, self, body)) =>
            val newBody = body.map {
              case ValDef(mods, name, tpt, rhs) if name.toString == "t" =>
                val newMods = if (mods.hasFlag(CASEACCESSOR)) {
                  mods.asInstanceOf[scala.reflect.internal.Trees#Modifiers]
                    .&~(CASEACCESSOR.asInstanceOf[Long]).asInstanceOf[Modifiers]
                } else {
                  mods
                }
                ValDef(newMods, name, tpt, rhs)
              case d => d
            }
            ClassDef(mods, name, tparams, Template(parents, self, newBody))
          case _ => c.abort(c.enclosingPosition, "should be impossible")
        }
        // for debugging
        // c.warning(c.enclosingPosition, s"generated\n$noCaseAccessorForType")
        noCaseAccessorForType
      case _ => c.abort(c.enclosingPosition, "expected a case class extends Primitive")
    }
  }
}