package shine.C.Compilation

import arithexpr.arithmetic.Cst
import shine.C.AST.{IncludeHeader, ParamKind}
import shine.C.{Compilation, Module}
import shine.DPIA.Compilation.Passes._
import shine.DPIA.Compilation.{FunDef, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.{AccType, CommType, ExpType, TypeCheck}
import rise.core.types.DataType._
import shine.DPIA.primitives.functional
import shine.{C, DPIA}
import util.compiler.DSL.run

import scala.collection.{immutable, mutable}

object ModuleGenerator extends DPIA.Compilation.ModuleGenerator[FunDef] {
  override type Module = C.Module
  override type CodeGenerator = Compilation.CodeGenerator

  override def createOutputParam(outT: ExpType): Identifier[AccType] =
    outT.dataType match {
      case _: ScalarType | _: FragmentType | _: IndexType | `NatType` | _: VectorType =>
        identifier("output", AccType(ArrayType(Cst(1), outT.dataType)))
      case _: ArrayType | _: DepArrayType =>
        identifier("output", AccType(outT.dataType))
      case _: PairType =>
        throw new Exception("Pairs as output parameters currently not supported")
      case _: DepPairType[_, _] =>
        identifier("output", AccType(outT.dataType))
      case _: DataTypeIdentifier | _: NatToDataApply |
           _: OpaqueType | _: ManagedBufferType =>
        throw new Exception(s"unexpected output data type: ${outT.dataType}")
    }

  def toImperative(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[ExpType] => Phrase[CommType] = p => {
    implicit val context: DPIA.Compilation.TranslationContext =
      gen.translationContext

    val output = (outParam.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => outParam
      case (rise.core.types.DataType.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        outParam `@` functional.NatAsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    output |> (
      TranslationToImperative.acc(p) _ andThen
      run(TypeCheck(_)) )
  }

  override def imperativeToModule(gen: CodeGenerator,
                                  funDef: FunDef,
                                  outParam: Identifier[AccType]
                                 ): Phrase[CommType] => Module = {
    imperativePasses andThen
      generateCode(gen, funDef, outParam) andThen
      makeCModule(gen, funDef, outParam)
  }

  def imperativePasses: Phrase[CommType] => Phrase[CommType] = {
    UnrollLoops.unroll andThen
    SimplifyNats.simplify
  }

  def generateCode(gen: CodeGenerator,
                   funDef: FunDef,
                   outParam: Identifier[AccType]
                  ): Phrase[CommType] => (Seq[gen.Decl], gen.Stmt) = {
    val env = shine.DPIA.Compilation.CodeGenerator.Environment(
      (outParam +: funDef.params).map(p => p -> C.AST.DeclRef(p.name)).toMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    gen.generate(funDef.topLevelLetNats, env)
  }

  def makeCModule(gen: CodeGenerator,
                  funDef: FunDef,
                  outParam: Identifier[AccType]
                 ): ((Seq[gen.Decl], gen.Stmt)) => Module = {
    case (declarations, code) =>
      val params = (outParam +: funDef.params).
        map(C.AST.makeParam(C.AST.makeParamTy(gen)))
      Module(
        includes = immutable.Seq(IncludeHeader("stdint.h")),
        decls = collectTypeDeclarations(code, params) ++ declarations,
        functions = immutable.Seq(
          C.AST.Function(
            code =
              C.AST.FunDecl(
                funDef.name,
                returnType = C.AST.Type.void,
                params,
                C.AST.Block(immutable.Seq(code))),
            paramKinds =
              ParamKind(
                outParam.`type`.dataType, C.AST.ParamKind.Kind.output
              ) +: funDef.params.map(p =>
                ParamKind(p.`type`.dataType, C.AST.ParamKind.Kind.input)
              )
          )
        )
      )
  }

  def collectTypeDeclarations(code: C.AST.Stmt, params: immutable.Seq[C.AST.ParamDecl]
                             ): immutable.Seq[C.AST.Decl] = {
    def visitor(decls: mutable.ArrayBuffer[C.AST.Decl]): C.AST.Nodes.VisitAndRebuild.Visitor = {
      new C.AST.Nodes.VisitAndRebuild.Visitor {
        def collect(t: C.AST.Type): Unit = t match {
          case _: C.AST.BasicType | _: C.AST.OpaqueType | _: C.AST.FragmentType =>
          case s: C.AST.StructType =>
            s.fields.foreach { case (ty, _) => collect(ty) }
            decls += C.AST.StructTypeDecl(
              s.print,
              s.fields.map { case (ty, name) => C.AST.VarDecl(name, ty) }
            )
          case at: C.AST.ArrayType => collect(at.elemType)
          case pt: C.AST.PointerType => collect(pt.valueType)
          case ut: C.AST.UnionType => ut.fields.foreach(collect)
        }

        override def apply(t: C.AST.Type): C.AST.Type = {
          collect(t); t
        }
      }
    }

    val allocTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    code.visitAndRebuild(visitor(allocTypeDecls))

    val paramTypeDecls = mutable.ArrayBuffer[C.AST.Decl]()
    params.foreach(_.visitAndRebuild(visitor(paramTypeDecls)))

    (allocTypeDecls ++ paramTypeDecls).distinct.toSeq
  }
}
