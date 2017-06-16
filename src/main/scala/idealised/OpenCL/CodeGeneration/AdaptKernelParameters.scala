package idealised.OpenCL.CodeGeneration

import idealised.DPIA
import idealised.DPIA.Phrases.{Identifier, Literal, Phrase, VisitAndRebuild}
import idealised.DPIA.Types.{AccType, CommandType, ExpType, IndexType, PhraseType}
import idealised.DPIA.DSL._
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import ir.{ArrayType, ScalarType}
import lift.arithmetic.Cst
import opencl.generator.OpenCLAST.{ParamDecl, VarRef}
import opencl.ir.{GlobalMemory, LocalMemory}

import scala.collection.mutable

//
// Parameters (ParDecl) in global or local memory which have a non-array type in DPIA have to be
// represented as arrays of size 1 in OpenCL. Every reference to such a parameter is adjusted
// by indexing it with 0.
//
object AdaptKernelParameters {

  def apply(originalPhrase: Phrase[CommandType],
            params: Seq[ParamDecl],
            inputParams: Seq[Identifier[ExpType]]
           ): (Phrase[CommandType], Seq[ParamDecl]) = {
    val (newParams, scalarParamsInGlobalOrLocalMemory) = adaptParamDecls(params, inputParams)

    val rewrittenPhrase = VisitAndRebuild(originalPhrase, Visitor(scalarParamsInGlobalOrLocalMemory))

    (rewrittenPhrase, newParams)
  }

  private def adaptParamDecls(params: Seq[ParamDecl],
                      inputParams: Seq[Identifier[ExpType]]): (Seq[ParamDecl], Set[String]) = {
    val scalarParamsInGlobalOrLocalMemory = mutable.Set[String]()

    val newParams = params.map(paramDecl => {
      paramDecl.t match {
        case _: ScalarType =>
          paramDecl.addressSpace match {
            case GlobalMemory | LocalMemory =>
              // remember scalar parameters in global or local memory and change their type to an
              // array of size 1
              scalarParamsInGlobalOrLocalMemory.add(paramDecl.name)
              paramDecl.copy(t = ArrayType(paramDecl.t, 1))
            case _ => paramDecl
          }
        case _: ArrayType =>
          // make input parameters const
          if (inputParams.map(_.name).contains(paramDecl.name)) {
            paramDecl.copy(const = true)
          } else {
            paramDecl
          }
        case _ => paramDecl
      }
    })

    (newParams, scalarParamsInGlobalOrLocalMemory.toSet)
  }

  private case class Visitor(scalarParamsInGlobalOrLocalMemory: Set[String])
    extends VisitAndRebuild.Visitor
  {
    override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
      p match {
        case i: Identifier[T] if scalarParamsInGlobalOrLocalMemory.contains(i.name) =>
          val zero = Literal(IndexData(0), IndexType(1))
          Stop(i.`type` match {
            case _: ExpType =>
              val ie = i.asInstanceOf[Identifier[ExpType]]
              (ie.copy(`type` = ExpType(DPIA.Types.ArrayType(1, ie.`type`.dataType))) `@` zero).asInstanceOf[Phrase[T]]
            case _: AccType =>
              val ia = i.asInstanceOf[Identifier[AccType]]
              (ia.copy(`type` = AccType(DPIA.Types.ArrayType(1, ia.`type`.dataType))) `@` zero).asInstanceOf[Phrase[T]]
          })
        case _ => Continue(p, this)
      }
    }
  }


}
