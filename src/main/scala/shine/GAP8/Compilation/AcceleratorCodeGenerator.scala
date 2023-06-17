package shine.GAP8.Compilation

import arithexpr.arithmetic
import arithexpr.arithmetic.ArithExpr
import arithexpr.arithmetic.ArithExpr.toInt
import rise.core.types.DataType
import shine.C.Compilation.CodeGenerator.CIntExpr
import shine.DPIA.Compilation.{CodeGenerator, TranslationContext}
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Identifier, Lambda, Phrase, PhrasePair}
import shine.DPIA.Types.{CommType, ExpType}
import shine.GAP8.{ConvolutionFilterSize, DMATransferType}
import shine.GAP8.primitives.functional.Cast
import shine.GAP8.primitives.imperative._
import shine.{C, OpenMP}

import scala.collection.{immutable, mutable}

// scalastyle:off
class AcceleratorCodeGenerator(override val decls: C.Compilation.CodeGenerator.Declarations,
                               override val ranges: C.Compilation.CodeGenerator.Ranges)
  extends OpenMP.CodeGenerator(decls, ranges) {
  override def name: String = "GAP8 ACC"

  override def translationContext: TranslationContext = super.translationContext

  override def updatedRanges(key: String, value: arithexpr.arithmetic.Range): AcceleratorCodeGenerator =
    new AcceleratorCodeGenerator(decls, ranges.updated(key, value))

  override def exp(env: Environment,
                   path: List[shine.C.Compilation.CodeGenerator.PathExpr],
                   cont: Expr => Stmt): Phrase[ExpType] => Stmt = {
    case Cast(_, _, input) =>
      super.exp(env, path, cont)(input)
    case other =>
      super.exp(env, path, cont)(other)
  }

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    //TODO: Supoort multicycle output for 3x3
    case Conv3x3(h, w, dt, bias, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._3x3, h, w, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv5x5(h, w, dt, bias, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._5x5, h, w, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x7(h, w, dt, bias, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x7, h, w, bias, inC, filterC, outputC)
          })
        })
      })
    case Conv7x4(h, w, dt, bias, in, filter, out) =>
      out |> acc(env, Nil, (outputC: C.AST.Expr) => {
        in |> exp(env, Nil, (inC: C.AST.Expr) => {
          filter |> exp(env, Nil, (filterC: C.AST.Expr) => {
            generateCalls(shine.GAP8._7x4, h, w, bias, inC, filterC, outputC)
          })
        })
      })

    /**
      * Generates a call to DMA transfer
      * rt_dma_memcpy(src, dest, size, direction, merge, struct)
      * rt_dma_wait(struct)
      * ext loc
      * TODO: Rethink this as it issues a call which will wait for the transfer to complete
      * immediately after the transfer has begun. Separating these two concerns could enable
      * parallelization of transfers and other useful calculations
      * */
    case copy@DmaCopy(transferType) =>
      val size = shine.GAP8.AST.Types.sizeInBytes(copy.dt)
      copy.dst |> acc(env, List(CIntExpr(0)), (dstC: C.AST.Expr) => {
        copy.src |> exp(env, List(CIntExpr(0)), (srcC: C.AST.Expr) => {
          val (ext, loc) = transferType match {
            case shine.GAP8.L1toL2 =>
              (dstC, srcC)
            case shine.GAP8.L2toL1 =>
              (srcC, dstC)
          }
          C.AST.Block(Seq(
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("rt_dma_memcpy"),
              Seq(
                // TODO: Reference fetch via ampersand might be needed (ext,loc)
                ext,
                loc,
                C.AST.Literal(size.toString),
                C.AST.Literal(transferType.toGAP8string),
                C.AST.Literal(0.toString),
                C.AST.Literal("&L2toL1")
              )
            )),
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("rt_dma_wait"),
              Seq(C.AST.Literal("&L2toL1"))
            ))
          ))
        })
      })

    //short int* ImageIn_L1_Temp_Converted =
    //  (short int*) rt_alloc(RT_ALLOC_CL_DATA, (IMG_W + 2) * ((IMG_H / NUM_STRIPES) + 2) * sizeof(short int));
    // Array Type  Memory Type          Size
    // DataType    Nat / Address Space  Nat
    case malloc@MemoryAlloc(memoryType) => {
      val dt = malloc.dt
      malloc.f match {
        case Lambda(v, p) =>
          val ve = Identifier(s"${v.name}_e", v.t.t1)
          val va = Identifier(s"${v.name}_a", v.t.t2)
          val vC = C.AST.DeclRef(v.name)

          C.AST.Block(Seq(
            C.AST.DeclStmt(C.AST.VarDecl(
              vC.name,
              arrayToPointerType(dt),
              Some(
                C.AST.Cast(
                  C.AST.PointerType(shine.GAP8.AST.Types.toStdint(dt), false),
                  C.AST.FunCall(
                    C.AST.DeclRef("rt_alloc"),
                    Seq(
                      C.AST.Literal(memoryType.toAllocString),
                      C.AST.Literal(shine.GAP8.AST.Types.sizeInBytes(dt).toString)
                    )
                  )
                )
              )
            )),
            Phrase.substitute(PhrasePair(ve, va), `for` = v, `in` = p) |>
              cmd(env updatedIdentEnv (ve -> vC) updatedIdentEnv (va -> vC)),
            //TODO: Generate dealloc / free?
            C.AST.ExprStmt(
              C.AST.FunCall(
                C.AST.DeclRef("rt_free"),
                Seq(
                  C.AST.Literal(memoryType.toAllocString),
                  C.AST.Literal(vC.name),
                  C.AST.Literal(shine.GAP8.AST.Types.sizeInBytes(dt).toString)
                )
              )
            )
          ))
        case _ => throw new RuntimeException("This should not happen")
      }
    }

    case MemorySet(dt, value, array) =>
      array |> acc(env, Nil, (arrayC: C.AST.Expr) => {
        value |> exp(env, Nil, (valueC: C.AST.Expr) => {
          C.AST.Block(Seq(
            C.AST.Comment("memset"),
            C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("memset"), Seq(
              arrayC,
              valueC,
              C.AST.Literal(shine.GAP8.AST.Types.sizeInBytes(dt).toString)
            )))
          ))
        })
      })

    //rt_dma_memcpy_2d(unsigned int ext,
    // unsigned int loc,
    // unsigned short size,
    // unsigned short stride,
    // unsigned short length,
    // rt_dma_dir_e dir, int merge, rt_dma_copy_t *copy);
    // stride  2D stride, which is the number of bytes which are added to the beginning of the current line to switch to the next one. Must fit in 16 bits, i.e. must be less than 65536.
    // length  2D length, which is the number of transfered bytes after which the DMA will switch to the next line. Must fit in 16 bits, i.e. must be less than 65536.
    case copy@Dma2DOffsetCopy(transferType) =>
      copy.dst |> acc(env, CIntExpr(0) :: Nil, (dstC: C.AST.Expr) => {
        copy.src |> exp(env, CIntExpr(0) :: Nil, (srcC: C.AST.Expr) => {
          val (ext, loc) = transferType match {
            case shine.GAP8.L1toL2 =>
              (dstC, srcC)
            case shine.GAP8.L2toL1 =>
              (srcC, dstC)
          }
          val stride = toInt(copy.offsetH).toString
          val length = toInt(copy.offsetW).toString
          val size = shine.GAP8.AST.Types.sizeInBytes(copy.dt) * copy.h * copy.w
          C.AST.Block(Seq(
            C.AST.Comment("dma2dOffsetCopy"),
            C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef("rt_dma_memcpy_2d"), Seq(
              ext,
              loc,
              C.AST.Literal(size.toString),
              C.AST.Literal(stride),
              C.AST.Literal(length),
              C.AST.Literal(transferType.toGAP8string),
              C.AST.Literal(0.toString),
              C.AST.Literal("&L2toL1")
            ))),
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("rt_dma_wait"),
              Seq(C.AST.Literal("&L2toL1"))
            ))
          ))
        })
      })

    case phrase => phrase |> super.cmd(env)
  }

  private def generateCalls(fs: ConvolutionFilterSize, h: Nat, w: Nat, bias: Nat,
                                       in: Expr, filter: Expr, output: Expr): Stmt = {
    C.AST.Block(Seq(
      hwceEnableCall,
      hwceGenericInitCall(fs),
      hwceSetYinModeCall(),
      generateHwceCallFunction(fs, h, w, bias, in, filter, output),
      hwceDisableCall
    ))
  }

  private def generateHwceCallFunction(fs: ConvolutionFilterSize, h: Nat, w: Nat, bias: Nat,
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

  //TODO: Lazy val
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

  private def arrayToPointerType(dt: DataType): Type =
    dt match {
      case rise.core.types.DataType.ArrayType(size, elemType) =>
        C.AST.PointerType(
          C.AST.BasicType(shine.GAP8.AST.Types.toStdint(elemType).toString, false)
        )
      case _ => typ(dt)
    }
  override def typ(dt: DataType): Type = super.typ(dt)
}

object AcceleratorCodeGenerator {
  def apply() = new AcceleratorCodeGenerator(
    mutable.ListBuffer[C.AST.Decl](),
    immutable.Map[String, arithmetic.Range]()
  )
}
