package shine.GAP8.Compilation

import arithexpr.arithmetic
import arithexpr.arithmetic.ArithExpr
import rise.core.types.DataType.ArrayType
import shine.DPIA.Compilation.{CodeGenerator, TranslationContext}
import shine.DPIA.Nat
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{CommType, ExpType}
import shine.DPIA.primitives.functional.{Join, PadClamp}
import shine.GAP8.ConvolutionFilterSize
import shine.GAP8.primitives.imperative.{Conv3x3, Conv5x5, Conv7x4, Conv7x7}
import shine.{C, OpenMP}

import scala.collection.{immutable, mutable}

// scalastyle:off
class AcceleratorCodeGenerator(override val decls: C.Compilation.CodeGenerator.Declarations,
                               override val ranges: C.Compilation.CodeGenerator.Ranges)
  extends OpenMP.CodeGenerator(decls, ranges) {
  override def name: String = "GAP8 ACC"

  override def translationContext: TranslationContext = super.translationContext

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    //case Conv3x3(w, h, bias, dt, in, PadClamp(n, l, r, _, Join(_, _, _, _, filter)), out) =>
    //  ???
    //TODO: Supoort multicycle output for 3x3
    case Conv3x3(w, h, bias, dt, in, filter: shine.DPIA.Phrases.Identifier[ExpType], out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          val oldFilterId = shine.DPIA.Phrases.Identifier(filter.name,
            ExpType(ArrayType(3, ArrayType(3, dt)), rise.core.types.read))
          val ref = env.identEnv(oldFilterId)
          val identEnv = env.identEnv - oldFilterId
          val env2 = CodeGenerator.Environment(identEnv + ((filter, ref)),
                                               env.commEnv, env.contEnv, env.letNatEnv)
          filter |> exp(env2, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._3x3, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv5x5(w, h, bias, dt, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._5x5, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x7(w, h, bias, dt, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x7, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x4(w, h, bias, dt, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x4, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case phrase => phrase |> super.cmd(env)
  }

  private def generateCalls(fs: ConvolutionFilterSize, w: Nat, h: Nat, bias: Nat,
                                       in: Expr, filter: Expr, output: Expr): Stmt = {
    C.AST.Block(Seq(
      hwceEnableCall,
      hwceGenericInitCall(fs),
      hwceSetYinModeCall(),
      generateHwceCallFunction(fs, w, h, bias, in, filter, output),
      hwceDisableCall
    ))
  }

  private def generateHwceCallFunction(fs: ConvolutionFilterSize, w: Nat, h: Nat, bias: Nat,
                                       in: Expr, filter: Expr, output: Expr): Stmt = {
    fs match {
      case shine.GAP8._3x3 =>
        C.AST.ExprStmt(C.AST.FunCall(
          C.AST.DeclRef(fs.functionName),
          Seq(in,
            output,
            C.AST.Literal("NULL"),
            C.AST.Literal("NULL"),
            filter,
            C.AST.Literal(ArithExpr.toInt(bias).toString),
            C.AST.Literal(ArithExpr.toInt(w).toString),
            C.AST.Literal(ArithExpr.toInt(h).toString),
            C.AST.Literal("0x7")
          )
        ))
      case _ =>
        C.AST.ExprStmt(C.AST.FunCall(
          C.AST.DeclRef(fs.functionName),
          Seq(in,
            output,
            filter,
            C.AST.Literal(ArithExpr.toInt(bias).toString),
            C.AST.Literal(ArithExpr.toInt(w).toString),
            C.AST.Literal(ArithExpr.toInt(h).toString),
          )
        ))
    }
  }

  private val hwceEnableCall = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("HWCE_Enable"), Seq()))
  private val hwceDisableCall = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("HWCE_Disable"), Seq()))
  private val hwceSoftResetCall = C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("HwCE_SoftReset"), Seq()))
  private def hwceSetInputBiasCall(bias: Int = 0) = C.AST.ExprStmt(C.AST.FunCall(
    C.AST.DeclRef("HwCE_SetInputBias"), Seq(C.AST.Literal(bias.toString))
  ))
  private def hwceSetYinModeCall(mode: Int = 1) = C.AST.ExprStmt(C.AST.FunCall(
    C.AST.DeclRef("HwCE_SetYinMode"),
    Seq(C.AST.Cast(C.AST.Type.u32, C.AST.Literal(mode.toString)))
  ))
  private def hwceGenericInitCall(fs: ConvolutionFilterSize, wstride: Int = 0, qnorm: Int = 0) =
    C.AST.ExprStmt(C.AST.FunCall(
      C.AST.DeclRef("HWCE_GenericInit"),
      Seq(
        C.AST.Cast(C.AST.Type.u32, C.AST.Literal(fs.toBackendConst)),
        C.AST.Cast(C.AST.Type.u32, C.AST.Literal(wstride.toString)),
        C.AST.Cast(C.AST.Type.u32, C.AST.Literal(qnorm.toString))
      )
    ))
}

object AcceleratorCodeGenerator {
  def apply() = new AcceleratorCodeGenerator(
    mutable.ListBuffer[C.AST.Decl](),
    immutable.Map[String, arithmetic.Range]()
  )
}
