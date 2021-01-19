package shine.C

import shine.C

class Function(val code: C.AST.FunDecl, val paramMetaData: Seq[ParamMetaData]) {
  assert(params.length == paramMetaData.length)

  def name: String = code.name
  def params: Seq[C.AST.ParamDecl] = code.params
  def returnType: C.AST.Type = code.returnType

  def inputParams: Seq[(C.AST.ParamDecl, ParamMetaData)] =
    (params zip paramMetaData).filter(_._2.kind == ParamMetaData.Kind.input)

  def outputParams: Seq[(C.AST.ParamDecl, ParamMetaData)] =
    (params zip paramMetaData).filter(_._2.kind == ParamMetaData.Kind.output)

  def temporaryParams: Seq[(C.AST.ParamDecl, ParamMetaData)] =
    (params zip paramMetaData).filter(_._2.kind == ParamMetaData.Kind.temporary)
}

object Function {
  def apply(code: C.AST.FunDecl, paramMetaData: Seq[ParamMetaData]): Function =
    new Function(code, paramMetaData)
}
