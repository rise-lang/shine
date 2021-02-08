package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.{acc, con}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.{->:, VarType, accT, expT}
import shine.OpenCL.primitives.imperative.New
import shine.macros.Primitive.expPrimitive

@expPrimitive
case class MapFragmentElements(fragType: FragmentType,
                               fragment: Phrase[ExpType],
                               fun: Phrase[ExpType ->: ExpType],
                              ) extends ExpPrimitive with AccT with ConT {

  fragment :: ExpType(fragType, read)
  fun :: ExpType(fragType.dataType, read) ->: ExpType(fragType.dataType, write)

  override val t: ExpType = ExpType(fragType, write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    val dt = fragType.dataType

    con(fragment)(λ(expT(fragType, read))(input =>
      shine.cuda.primitives.imperative.ForFragmentElements(fragType, input, A,
        λ(expT(dt, read))(x =>
          λ(accT(dt))(o =>
            acc(fun(x))(o))))))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    val dt = fragType.dataType

    New(AddressSpace.Private, fragType,
      λ(VarType(fragType))(fragmentAcc =>
        (if (fragment.t.accessType.toString == write.toString)
          acc(fragment)(fragmentAcc.wr) `;`
            shine.cuda.primitives.imperative.ForFragmentElements(fragType, fragmentAcc.rd, fragmentAcc.wr,
              λ(expT(dt, read))(x =>
                λ(accT(dt))(o =>
                  acc(fun(x))(o))))
        else
          acceptorTranslation(fragmentAcc.wr)) `;`
          C(fragmentAcc.rd)))
  }
}
