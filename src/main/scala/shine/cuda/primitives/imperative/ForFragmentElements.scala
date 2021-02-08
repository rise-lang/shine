package shine.cuda.primitives.imperative

import shine.DPIA.->:
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ForFragmentElements(fragType: FragmentType,
                                     in: Phrase[ExpType],
                                     out: Phrase[AccType],
                                     fun: Phrase[ExpType ->: AccType ->: CommType],
                                    ) extends CommandPrimitive {
  in :: ExpType(fragType, read)
  out :: AccType(fragType)
  fun :: FunType(ExpType(fragType.dataType, read), FunType(AccType(fragType.dataType), comm))

  override def prettyPrint: String = s"ForFragmentElements(${PrettyPhrasePrinter(in)}, ${PrettyPhrasePrinter(out)}, ${PrettyPhrasePrinter(fun)})"
}
