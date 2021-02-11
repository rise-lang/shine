package shine.cuda

import arithexpr.arithmetic
import arithexpr.arithmetic._
import shine.C.AST.DefaultTypeImplementations.BasicType
import shine.C.AST.{Decl, PointerType}
import shine.C.CodeGenerator.CIntExpr
import shine.C.{CodeGenerator => CCodeGenerator}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.AsVectorAligned
import shine.DPIA.primitives.imperative.{AsScalarAcc, Assign}
import shine.OpenCL.{CodeGenerator => OclCodeGenerator}
import shine._
import shine.cuda.primitives.imperative._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(): CodeGenerator =
    new CodeGenerator(
      mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends OclCodeGenerator(decls, ranges) {

  override def name: String = "CUDA"

  override def translationContext: TranslationContext = new cuda.TranslationContext()

  override def updatedRanges(key: String, value: arithexpr.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))

  def toString(layout: MatrixLayout): String = {
    layout match {
      case MatrixLayout.Row_Major => "nvcuda::wmma::mem_row_major"
      case MatrixLayout.Col_Major => "nvcuda::wmma::mem_col_major"
      case _ => throw new Exception("this should not happen")
    }
  }

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case f: ParFor =>
      val (i, o, p) = f.unwrapBody
      codeGenCudaParFor(f, f.n, f.dt, f.out, i, o, p, env)

    case WmmaLoad(m, n, k, _, fragType, layoutIdentifier, matrix, fragmentAcc) =>
      matrix |> exp(env, List(CIntExpr(0), CIntExpr(0)), matrixTile => {
        //Pointer to first element of the matrix
        val matrixPtr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, matrixTile)

        val (layout, ldm) = inferFragment(matrix, env, m, n, k, FragmentKind.Accumulator)
        if (layoutIdentifier.isInstanceOf[MatrixLayoutIdentifier])
          layoutIdentifier.asInstanceOf[MatrixLayoutIdentifier].setLayout(layout)

        fragmentAcc |> acc(env, Nil, frag =>
          C.AST.ExprStmt(C.AST.FunCall(
            C.AST.DeclRef("nvcuda::wmma::load_matrix_sync"),
              //only accumulator-fragments must specify their layout
              if (fragType != FragmentKind.Accumulator)
                immutable.Seq(
                  frag,
                  matrixPtr,
                  C.AST.ArithmeticExpr(ldm)
                )
              else
                immutable.Seq(
                  frag,
                  matrixPtr,
                  C.AST.ArithmeticExpr(ldm),
                  C.AST.DeclRef(toString(layout))))))})

    case WmmaStore(m, n, k, _, fragment, matrixAcc) =>
      fragment |> exp(env, Nil, fragment =>
        matrixAcc |> acc(env, List(CIntExpr(0), CIntExpr(0)), matrixTile => {
          val matrixPtr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, matrixTile)

          val (layout, ldm) = inferFragment(matrixAcc, env, m, n, k, FragmentKind.Accumulator)

          C.AST.ExprStmt(C.AST.FunCall(
            C.AST.DeclRef("nvcuda::wmma::store_matrix_sync"),
            immutable.Seq(
              matrixPtr,
              fragment,
              C.AST.ArithmeticExpr(ldm),
              C.AST.DeclRef(toString(layout)))))}))

    case WmmaFill(_, _, _, _, fill, _, _, fragmentAcc) =>
      fill |> exp(env, Nil, fill =>
        fragmentAcc |> acc(env, Nil, fragment =>
          C.AST.ExprStmt(C.AST.FunCall(
            C.AST.DeclRef("nvcuda::wmma::fill_fragment"),
            immutable.Seq(
              fragment,
              fill)))))

    case WmmaMMA(_, _, _, _, _, _, _, aMatrix, bMatrix, cMatrix, resultMatrixAcc) =>
      aMatrix |> exp(env, Nil, aMatrix =>
        bMatrix |> exp(env, Nil, bMatrix =>
          cMatrix |> exp(env, Nil, cMatrix =>
            resultMatrixAcc |> acc(env, Nil, resultMatrix =>
              C.AST.ExprStmt(C.AST.FunCall(
                C.AST.DeclRef("nvcuda::wmma::mma_sync"),
                immutable.Seq(
                  resultMatrix,
                  aMatrix,
                  bMatrix,
                  cMatrix)))))))

    case ForFragmentElements(fragType, inFragment, outFragmemt, Lambda(in, Lambda(out, p))) =>
      inFragment |> exp(env, Nil, fragmentIn =>
        outFragmemt |> acc(env, Nil, fragmentOut => {
          val n = C.AST.StructMemberAccess(fragmentOut, C.AST.DeclRef("num_elements"))
          val xIn = C.AST.StructMemberAccess(fragmentIn, C.AST.DeclRef("x"))
          val xOut = C.AST.StructMemberAccess(fragmentOut, C.AST.DeclRef("x"))

          val cI = C.AST.DeclRef(freshName("i_"))
          val range = RangeAdd(0, PosInf, 1)
          val updatedGen = updatedRanges(cI.name, range)

          val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
          val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, n)
          val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

          //TODO this a little bit ugly
          val xIInDecl = C.AST.UnaryExpr(C.AST.UnaryOperator.&, C.AST.ArraySubscript(xIn, cI))
          val xIOutDecl =  C.AST.UnaryExpr(C.AST.UnaryOperator.&, C.AST.ArraySubscript(xOut, cI))
          val xIInPointer = C.AST.DeclRef(freshName("xIn_"))
          val xIOutPointer = C.AST.DeclRef(freshName("xOut_"))
          //TODO update a identifiert with a dereferenced pointer?
          val xIIn = C.AST.DeclRef(f"*" + xIInPointer.name)
          val xIOut = C.AST.DeclRef(f"*" + xIOutPointer.name)

          C.AST.Stmts(C.AST.Code("#pragma unroll"),
            C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
              C.AST.Block(
                immutable.Seq(
                  C.AST.DeclStmt(C.AST.VarDecl(xIInPointer.name, PointerType(typ(fragType.dataType)), init = Some(xIInDecl))),
                  C.AST.DeclStmt(C.AST.VarDecl(xIOutPointer.name, PointerType(typ(fragType.dataType)), init = Some(xIOutDecl))),
                  p |> updatedGen.cmd(env updatedIdentEnv (in -> xIIn) updatedIdentEnv (out -> xIOut))))))}))

    case Assign(_, lhsAcc, rhs) =>
      //noinspection VariablePatternShadow
      rhs |> exp(env, Nil, e =>
        lhsAcc |> acc(env, Nil, {
          case C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(outputShared, pipe))
            if name.startsWith("nvcuda::experimental::memcpy_async") =>
              C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(outputShared, e, pipe)))
          case a => C.AST.ExprStmt(C.AST.Assignment(a, e))}))

    case SyncThreads() => cuda.ast.SynchronizeThreads()

    case SyncWarp() => cuda.ast.SynchronizeWarp()

    case SyncPipeline(pipe) =>
      pipe |> exp(env, Nil, pipe =>
        C.AST.ExprStmt(C.AST.StructMemberAccess(pipe, C.AST.DeclRef("commit_and_wait()"))))

    case phrase => phrase |> super.cmd(env)
  }

  def inferFragment[T <: BasePhraseType](matrix: Phrase[T], env: Environment, m: Nat, n: Nat, k: Nat, fragmentType: FragmentKind): (MatrixLayout, ArithExpr) = {
    val index00 = matrixIndexAsNat(matrix, env, 0, 0)
    val index01 = matrixIndexAsNat(matrix, env, 0, 1)
    val index10 = matrixIndexAsNat(matrix, env, 1, 0)

    val diffWithVars = (index10 - index01).enforceSimplification

    //Difference bewteen indices can containing variables, which must be a global matrix
    //dimensions (dimension of the matrix in memory) and therefore greater than 1
    val diff = diffWithVars.substitute(
      diffWithVars.varList.map(globalMatrixDimension => globalMatrixDimension -> Cst(2)).toMap)
      .getOrElse(diffWithVars).eval

    val matrixLayout =
      if (diff > 0)
        MatrixLayout.Row_Major
      else
        MatrixLayout.Col_Major

    val ldm =
      (if (matrixLayout == MatrixLayout.Row_Major)
        index10
      else
        index01) -
      index00

    (matrixLayout, ldm)
  }

  def matrixIndexAsNat[T <: BasePhraseType](matrix: Phrase[T], env: Environment, index1: Nat, index2: Nat): ArithExpr = {
    var result: Expr = null

    matrix.t match {
      case AccType(_) =>
        matrix.asInstanceOf[Phrase[AccType]] |> acc(env, List(CIntExpr(index1), CIntExpr(index2)), matrixAst => {
          result = matrixAst.asInstanceOf[C.AST.ArraySubscript].index
          C.AST.Comment("This should not be generated!")})
      case ExpType(_, _) =>
        matrix.asInstanceOf[Phrase[ExpType]] |> exp(env, List(CIntExpr(index1), CIntExpr(index2)), matrixAst => {
          result = matrixAst.asInstanceOf[C.AST.ArraySubscript].index
          C.AST.Comment("This should not be generated!")})
      case _ =>
        throw new Exception("This should not happen!")
    }


    result.asInstanceOf[C.AST.ArithmeticExpr].ae
  }

  override def acc(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[AccType] => Stmt = {
    case GlobalToSharedAcc(_, pipe, outputShared) => path match {
      case (i: CIntExpr) :: ps =>
        pipe |> exp(env, Nil, pipe =>
          outputShared |> acc(env, i :: ps, outputShared =>
            cont(
              C.AST.FunCall(
                C.AST.DeclRef("nvcuda::experimental::memcpy_async"),
                immutable.Seq(outputShared, pipe)))))

      case _ => error(s"Expected path to be not empty")
    }

    case AsScalarAcc(_, m, dt, a) => path match {
      case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
        a |> acc(env, CIntExpr((i * m) + j) :: ps, cont)

      case (i : CIntExpr) :: Nil =>
        a |> acc(env,CIntExpr(i * m) :: Nil, array => {
          cont(
            //acces first (vector-)element pointed by the pointer
            C.AST.ArraySubscript(
              //cast pointer to array to pointer of vectorType
              C.AST.Cast(
                C.AST.PointerType(getVectorType(dt, m)),
                C.AST.UnaryExpr(C.AST.UnaryOperator.&, array)),
              C.AST.ArithmeticExpr(0)))})

      case _ => error(s"Expected path to be not empty")
    }

    case phrase => phrase |> super.acc(env, path, cont)
  }

  override def exp(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[ExpType] => Stmt = {
    case phrase@AsVectorAligned(n, _, _, dt, e) => path match {
      case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
        e |> exp(env, CIntExpr((i * n) + j) :: ps, cont)

      case (i : CIntExpr) :: Nil =>
        e |> exp(env, CIntExpr(i * n) :: Nil, array => {
          cont(
            //acces first (vector-)element pointed by the pointer
            C.AST.ArraySubscript(
              //cast pointer to array to pointer of vectorType
              C.AST.Cast(
                C.AST.PointerType(getVectorType(dt, n)),
                C.AST.UnaryExpr(C.AST.UnaryOperator.&, array)),
              C.AST.ArithmeticExpr(0)))})

      case _ => error(s"unexpected $path")
    }

    case phrase => phrase |> super.exp(env, path, cont)
  }

  override def typ(dt: DataType): Type = dt match {
    case FragmentType(m, n, k, dataType, FragmentKind.AMatrix, layout) =>
      cuda.ast.WmmaAMatrix(m, n, k, typ(dataType), layout)
    case FragmentType(m, n, k, dataType, FragmentKind.BMatrix, layout) =>
      cuda.ast.WmmaBMatrix(m, n, k, typ(dataType), layout)
    case FragmentType(m, n, k, dataType, FragmentKind.Accumulator, _) =>
      cuda.ast.WmmaAccumulator(m, n, k, typ(dataType))
    case shine.DPIA.Types.f16 =>
      cuda.ast.Type.half
    case shine.DPIA.Types.pipeline =>
      cuda.ast.Type.pipeline

    case _ =>
      super.typ(dt)
  }

  private def getVectorType(dt: ScalarType, n: Nat): Type = {
    if (n.eval > 0 && n.eval <= 4)
      dt match {
        case shine.DPIA.Types.u8 => BasicType(s"uchar$n")
        case shine.DPIA.Types.i8 => BasicType(s"char$n")
        case shine.DPIA.Types.u16 => BasicType(s"ushort$n")
        case shine.DPIA.Types.i16 => BasicType(s"short$n")
        case shine.DPIA.Types.int | shine.DPIA.Types.i32 => BasicType(s"int$n")
        case shine.DPIA.Types.u32 => BasicType(s"uint$n")
        case shine.DPIA.Types.f32 => BasicType(s"float$n")
        case shine.DPIA.Types.f64 => BasicType(s"double$n")
        case _ => ???
      }
    else
      dt match {
        case shine.DPIA.Types.f16 if (n.eval > 0 && n.eval <= 8) => BasicType(s"float${n/2}")
        case shine.DPIA.Types.f16 if (n.eval > 0 && n.eval <= 16) => BasicType(s"double${n/4}")
        case _ => ???
      }
  }

  override def genNat(n: Nat, env: Environment, cont:Expr => Stmt): Stmt =
    n match {
      case of: BuiltInAttribute => cont(C.AST.Literal(of.toString))
      case _ => super.genNat(n, env, cont)
    }

  def codeGenCudaParFor(f: ParFor,
                        n: Nat,
                        dt: DataType,
                        a: Phrase[AccType],
                        i: Identifier[ExpType],
                        o: Phrase[AccType],
                        p: Phrase[CommType],
                        env: Environment): Stmt = {
    assert(!f.unroll)
    val cI = C.AST.DeclRef(f.name)
    val range = RangeAdd(f.init, n, f.step)
    val updatedGen = updatedRanges(cI.name, range)

    applySubstitutions(n, env.identEnv) |> (n => {

      val init =
        OpenCL.AST.VarDecl(
          cI.name, C.AST.Type.int, AddressSpace.Private,
          init = Some(C.AST.ArithmeticExpr(range.start)))
      val cond =
        C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment =
        C.AST.Assignment(cI,
          C.AST.ArithmeticExpr(NamedVar(cI.name, range) + range.step))

      Phrase.substitute(a `@` i, `for` = o, `in` = p) |> (p =>

        env.updatedIdentEnv(i -> cI) |> (env => {

          range.numVals match {
            // iteration count is 0 => skip body; no code to be emitted
            case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
            // iteration count is 1 => no loop
            case Cst(1) =>
              C.AST.Stmts(C.AST.Stmts(
                C.AST.Comment("iteration count is exactly 1, no loop emitted"),
                C.AST.DeclStmt(
                  C.AST.VarDecl(
                    cI.name, C.AST.Type.int,
                    init = Some(C.AST.ArithmeticExpr(f.init))))),
                p |> updatedGen.cmd(env))
            /* FIXME?
          case _ if (range.start.min.min == Cst(0) && range.stop == Cst(1)) ||
                    (range.numVals.min == NegInf
                    && range.numVals.max == Cst(1)) =>
            C.AST.Block(collection.Seq(
              C.AST.DeclStmt(init),
              C.AST.IfThenElse(cond, updatedGen.cmd(p, env) , None)
            ))
             */
            // default case
            case _ =>
              C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
                C.AST.Block(immutable.Seq(p |> updatedGen.cmd(env))))
          }}))})
  }

}
