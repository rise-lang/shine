package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{MapAcc, MapRead}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Map(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     f: Phrase[ExpType ->: ExpType],
                     array: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt1: DataType) ->: (dt2: DataType) ->: (access: AccessType) ->:
      (f :: t"exp[$dt1, $access] -> exp[$dt2, $access]") ->:
      (array :: exp"[$n.$dt1, $access]") ->: exp"[$n.$dt2, $access]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Map(fun.nat(n), fun.data(dt1), fun.data(dt2), fun.access(access),
      VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt1, access))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(array)(λ(env.toList.head._2.t)(y =>
      MapAcc(n, dt2, dt1,
        Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))), C(y))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt1, write))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    acc(array)(MapAcc(n, dt2, dt1,
      Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
      A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(x =>
      C(MapRead(n, dt1, dt2,
        fun(exp"[$dt1, $read]")(a =>
          fun(exp"[$dt2, $read]" ->: (comm: CommType))(cont =>
            con(f(a))(fun(exp"[$dt2, $read]")(b => Apply(cont, b))))),
        x))))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)} access={ToString(access)}>
      <f type={ToString(ExpType(dt1, access) ->: ExpType(dt2, access))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1), access))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}
