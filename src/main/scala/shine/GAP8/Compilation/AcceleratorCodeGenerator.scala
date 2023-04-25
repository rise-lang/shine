package shine.GAP8.Compilation

import arithexpr.arithmetic
import arithexpr.arithmetic.ArithExpr
import arithexpr.arithmetic.ArithExpr.toInt
import rise.core.types.DataType
import rise.core.types.DataType.ArrayType
import shine.DPIA.Compilation.{CodeGenerator, TranslationContext}
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Identifier, Lambda, Phrase, PhrasePair}
import shine.DPIA.Types.{AccType, CommType, ExpType}
import shine.DPIA.primitives.functional.{Join, PadClamp}
import shine.GAP8.{ConvolutionFilterSize, MemoryType}
import shine.GAP8.primitives.imperative.{Conv3x3, Conv5x5, Conv7x4, Conv7x7, DmaCopy, MemoryAlloc}
import shine.{C, OpenMP}

import scala.collection.{immutable, mutable}

// scalastyle:off
class AcceleratorCodeGenerator(override val decls: C.Compilation.CodeGenerator.Declarations,
                               override val ranges: C.Compilation.CodeGenerator.Ranges)
  extends OpenMP.CodeGenerator(decls, ranges) {
  override def name: String = "GAP8 ACC"

  override def translationContext: TranslationContext = super.translationContext

  private def swapEnvIdentifier(
                                 identifier: Identifier[ExpType],
                                 dt: DataType,
                                 env: Environment,
                                 height: Nat,
                                 width: Nat
                               ): Environment = {
    val oldFilterId = Identifier(
      identifier.name,
      ExpType(ArrayType(height, ArrayType(width, dt)), rise.core.types.read)
    )
    val ref = env.identEnv(oldFilterId)
    val identEnv = env.identEnv - oldFilterId
    CodeGenerator.Environment(identEnv + ((identifier, ref)),
      env.commEnv, env.contEnv, env.letNatEnv)
  }

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    //TODO: Supoort multicycle output for 3x3
    case Conv3x3(w, h, bias, dt, in, filter: Identifier[ExpType], out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          val env2 = swapEnvIdentifier(filter, dt, env, 3, 3)
          filter |> exp(env2, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._3x3, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv5x5(w, h, bias, dt, in, filter: Identifier[ExpType], out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          val env2 = swapEnvIdentifier(filter, dt, env, 5, 5)
          filter |> exp(env2, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._5x5, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x7(w, h, bias, dt, in, filter: Identifier[ExpType], out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          val env2 = swapEnvIdentifier(filter, dt, env, 7, 7)
          filter |> exp(env2, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x7, w, h, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x4(w, h, bias, dt, in, filter: Identifier[ExpType], out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          val env2 = swapEnvIdentifier(filter, dt, env, 7, 4)
          filter |> exp(env2, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x4, w, h, bias, inC, filterC, outputC)
          })
        })
      })

    //rt_dma_memcpy(src, dest, size, direction, merge, struct
    // Source address        Destination address  Size Direction Merge Struct
    // Identifier[ExpType]   Identifier[ExpType]  Nat  Nat       Nat   Identifier[ExpType]
    case copy@DmaCopy(transferType) =>
      println("In DmaCopy")
      val (size, elemType) = copy.dt match {
        case ArrayType(size, elemType) =>
          (toInt(size), elemType)
        case _ => throw new RuntimeException(s"Expected ArrayType, got ${copy.dt}")
      }
      copy.src |> exp(env, Nil, (srcC: C.AST.Expr) => {
        copy.dst |> acc(env, Nil, (dstC: C.AST.Expr) => {
          C.AST.Block(Seq(
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("rt_dma_memcpy"),
              Seq(
                srcC,
                dstC,
                //C.AST.Literal(ArithExpr.toInt(copy.dt.asInstanceOf[ArrayType].size).toString),
                C.AST.Literal(ArithExpr.toInt(size).toString),
                C.AST.Literal(copy.tt.toGAP8string),
                C.AST.Literal(""),
                C.AST.Literal("")
              )
            ))
          ))
        })
      })

    //short int* ImageIn_L1_Temp_Converted =
    //  (short int*) rt_alloc(RT_ALLOC_CL_DATA, (IMG_W + 2) * ((IMG_H / NUM_STRIPES) + 2) * sizeof(short int));
    // Array Type  Memory Type          Size
    // DataType    Nat / Address Space  Nat
    case malloc@MemoryAlloc(memoryType) => {
      println("In MemoryAlloc")
      val dt = malloc.dt
      malloc.f match {
        case Lambda(v, p) =>
          val ve = Identifier(s"${v.name}_e", v.t.t1)
          val va = Identifier(s"${v.name}_a", v.t.t2)
          val vC = C.AST.DeclRef(v.name)

          C.AST.Block(immutable.Seq(
            //
            //C.AST.DeclStmt(C.AST.VarDecl(vC.name, typ(dt), Funcall (malloc))),
            C.AST.DeclStmt(C.AST.VarDecl(
              vC.name,
              typ(dt),
              Some(C.AST.FunCall(
                C.AST.DeclRef("rt_alloc"),
                Seq()
              ))
            )),
            //Generate here a call to malloc
            Phrase.substitute(PhrasePair(ve, va), `for` = v, `in` = p) |> cmd(env updatedIdentEnv (ve -> vC)
              updatedIdentEnv (va -> vC))))

          /*C.AST.Block(Seq(
            C.AST.ExprStmt(C.AST.Assignment(
              ???,
              ???
            ))
          ))*/
        case _ => throw new RuntimeException("This should not happen")
      }
    }

    case phrase => phrase |> super.cmd(env)
  }

  private def arraySizeHelper(dt: DataType): Nat = ???

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
