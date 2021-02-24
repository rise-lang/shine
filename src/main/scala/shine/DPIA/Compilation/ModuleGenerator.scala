package shine.DPIA.Compilation

import shine.DPIA
import shine.DPIA.Phrases._
import shine.DPIA.Types.{AccType, CommType, ExpType, PhraseType, TypeCheck}
import util.compiler.DSL.run

trait ModuleGenerator {
  type Module
  type CodeGenerator <: DPIA.Compilation.CodeGenerator

  def generateFunction(name: String,
                       gen: CodeGenerator): Phrase[_ <: PhraseType] => Module =
    (new FunDef(name, _)) andThen funDefToModule(gen)

  def funDefToModule(gen: CodeGenerator): FunDef => Module = funDef => {
    val outParam = createOutputParam(funDef.body.t)

    funDef.body |> (
      run(TypeCheck(_: Phrase[ExpType])) andThen
      rewriteToImperative(gen, funDef, outParam) andThen
      imperativePasses(gen, funDef, outParam) andThen
      generateCode(gen, funDef, outParam) andThen
      makeModule(gen, funDef, outParam) )
  }

  def createOutputParam(outT: ExpType): Identifier[AccType]

  def rewriteToImperative(gen: CodeGenerator,
                          funDef: FunDef,
                          outParam: Identifier[AccType]
                         ): Phrase[ExpType] => Phrase[CommType]

  type PhraseAfterPasses
  def imperativePasses(gen: CodeGenerator,
                       funDef: FunDef,
                       outParam: Identifier[AccType]
                      ): Phrase[CommType] => PhraseAfterPasses

  type GeneratedCode
  def generateCode(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): PhraseAfterPasses => GeneratedCode

  def makeModule(gen: CodeGenerator,
                 funDef: FunDef,
                 outParam: Identifier[AccType]
                ): GeneratedCode => Module
}
