package shine.OpenCL.Compilation

import shine.DPIA.Compilation.FunDef
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.PhraseType
import shine.OpenCL.{GlobalSize, LocalSize}

class KernelDef(override val name: String,
                override val definition: Phrase[_ <: PhraseType],
                val wgConfig: Option[(LocalSize, GlobalSize)]
               ) extends FunDef(name, definition) {
  def withWgConfig(localSize: LocalSize, globalSize: GlobalSize): KernelDef =
    KernelDef(name, definition, localSize, globalSize)

}

object KernelDef {
  def apply(name: String, definition: Phrase[_ <: PhraseType]): KernelDef =
    new KernelDef(name, definition, None)
  def apply(name: String,
            definition: Phrase[_ <: PhraseType],
            localSize: LocalSize,
            globalSize: GlobalSize): KernelDef =
    new KernelDef(name, definition, Some(localSize, globalSize))
}
