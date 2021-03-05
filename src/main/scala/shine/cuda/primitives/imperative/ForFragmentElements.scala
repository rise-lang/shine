package shine.cuda.primitives.imperative

import shine.DPIA.->:
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

/**
  * Applies a function on every element of a fragment. <br>
  * This primitive needs to be executed by a full warp!
  * @param fragType type of the fragment
  * @param in       fragment of type `fragType` whose elements should be iterated
  * @param out      fragment-Acceptor of type `fragType` which is used to store the result
  * @param fun      function which takes an element of type `fragType.dataType` from `in` and
  *                 an element-Acceptor of type `fragType.dataType` from `out` and returns a command
  */
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
