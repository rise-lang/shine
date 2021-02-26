package shine.C.AST

import shine.C

class Function(val code: C.AST.FunDecl,
               val paramKinds: Seq[ParamKind]) {
  assert(params.length == paramKinds.length)

  def name: String = code.name
  def params: Seq[C.AST.ParamDecl] = code.params
  def returnType: C.AST.Type = code.returnType

  def inputParams: Seq[(C.AST.ParamDecl, ParamKind)] =
    (params zip paramKinds).filter(_._2.kind == ParamKind.Kind.input)

  def outputParams: Seq[(C.AST.ParamDecl, ParamKind)] =
    (params zip paramKinds).filter(_._2.kind == ParamKind.Kind.output)

  def temporaryParams: Seq[(C.AST.ParamDecl, ParamKind)] =
    (params zip paramKinds).filter(_._2.kind == ParamKind.Kind.temporary)
}

object Function {
  def apply(code: C.AST.FunDecl, paramKinds: Seq[ParamKind]): Function =
    new Function(code, paramKinds)
}
