package shine.DPIA.Compilation

import shine.DPIA
import shine.DPIA.Phrases._
import shine.DPIA.Types.{AccType, CommType, ExpType, TypeCheck}
import util.compiler.DSL.run

trait ModuleGenerator[FunDef <: DPIA.Compilation.FunDef] {
  type Module
  type CodeGenerator <: DPIA.Compilation.CodeGenerator

  def funDefToModule(gen: CodeGenerator): FunDef => Module = funDef => {
    val outParam = createOutputParam(funDef.body.t)

    funDef.body |> (
      run(TypeCheck(_: Phrase[ExpType])) andThen
        toImperative(gen, funDef, outParam) andThen
        imperativeToModule(gen, funDef, outParam))
  }

  def createOutputParam(outT: ExpType): Identifier[AccType]

  def toImperative(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[ExpType] => Phrase[CommType]

  def imperativeToModule(gen: CodeGenerator,
                         funDef: FunDef,
                         outParam: Identifier[AccType]
                        ): Phrase[CommType] => Module
}
