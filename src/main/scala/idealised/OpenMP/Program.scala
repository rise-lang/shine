package idealised.OpenMP

import idealised.DPIA.Phrases.Identifier
import idealised.DPIA.Types.{AccType, ExpType}
import opencl.generator._

case class Program(function: OpenCLAST.Function,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String = (new OpenCLPrinter)(function)

}
