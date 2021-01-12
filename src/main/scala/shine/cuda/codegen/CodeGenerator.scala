package shine.cuda.codegen

import arithexpr.arithmetic
import arithexpr.arithmetic._
import shine.C.AST.DefaultTypeImplementations.BasicType
import shine.C.AST.{Decl, PointerType}
import shine.C.CodeGeneration.CodeGenerator.{CIntExpr, PathExpr}
import shine.C.CodeGeneration.{CodeGenerator => CCodeGen}
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.AsVectorAligned
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.CodeGeneration.{CodeGenerator => OclCodeGen}
import shine._
import shine.cuda.BuiltInAttribute
import shine.cuda.primitives.imperative._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(): CodeGenerator =
    new CodeGenerator(
      mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val decls: CCodeGen.Declarations,
                    override val ranges: CCodeGen.Ranges)
  extends OclCodeGen(decls, ranges) {

  override def updatedRanges(key: String, value: arithexpr.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))

  def toString(layout: MatrixLayout): String = {
    layout match {
      case MatrixLayout.Row_Major => "nvcuda::wmma::mem_row_major"
      case MatrixLayout.Col_Major => "nvcuda::wmma::mem_col_major"
      case _ => throw new Exception("this should not happen")
    }
  }

  override def cmd(phrase: Phrase[CommType], env: Environment): Stmt = {
    phrase match {
      case f@CudaParFor(n, dt, a, Lambda(i, Lambda(o, p)), _, _, _) =>
        CudaCodeGen.codeGenCudaParFor(f, n, dt, a, i, o, p, env)

      case WmmaLoad(m, n, k, _, matrix, fragmentAcc) =>
        exp(matrix, env, List(CIntExpr(0), CIntExpr(0)), matrixTile => {
          //Pointer to first element of the matrix
          val matrixPtr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, matrixTile)

          val (layout, ldm) = inferFragment(matrix, env, m, n, k, FragmentType.Acuumulator)
          if (fragmentAcc.t.dataType.asInstanceOf[Fragment].layout.isInstanceOf[MatrixLayoutIdentifier])
            fragmentAcc.t.dataType.asInstanceOf[Fragment].layout.asInstanceOf[MatrixLayoutIdentifier].setLayout(layout)

          acc(fragmentAcc, env, Nil, frag =>
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("nvcuda::wmma::load_matrix_sync"),
                //only accumulator-fragments must specify their layout
                if (fragmentAcc.t.dataType.asInstanceOf[Fragment].fragmentType != FragmentType.Acuumulator)
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
        exp(fragment, env, Nil, fragment =>
          acc(matrixAcc, env, List(CIntExpr(0), CIntExpr(0)), matrixTile => {
            val matrixPtr = C.AST.UnaryExpr(C.AST.UnaryOperator.&, matrixTile)

            val (layout, ldm) = inferFragment(matrixAcc, env, m, n, k, FragmentType.Acuumulator)

            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("nvcuda::wmma::store_matrix_sync"),
              immutable.Seq(
                matrixPtr,
                fragment,
                C.AST.ArithmeticExpr(ldm),
                C.AST.DeclRef(toString(layout)))))}))

      case WmmaFill(_, _, _, _, fill, _, _, fragmentAcc) =>
        exp(fill, env, Nil, fill =>
          acc(fragmentAcc, env, Nil, fragment =>
            C.AST.ExprStmt(C.AST.FunCall(
              C.AST.DeclRef("nvcuda::wmma::fill_fragment"),
              immutable.Seq(
                fragment,
                fill)))))

      case WmmaMMA(_, _, _, _, _, _, _, aMatrix, bMatrix, cMatrix, resultMatrixAcc) =>
        exp(aMatrix, env, Nil, aMatrix =>
          exp(bMatrix, env, Nil, bMatrix =>
            exp(cMatrix, env, Nil, cMatrix =>
              acc(resultMatrixAcc, env, Nil, resultMatrix =>
                C.AST.ExprStmt(C.AST.FunCall(
                  C.AST.DeclRef("nvcuda::wmma::mma_sync"),
                  immutable.Seq(
                    resultMatrix,
                    aMatrix,
                    bMatrix,
                    cMatrix)))))))

      case ForFragmentElements(fragType, inFragment, outFragmemt, Lambda(in, Lambda(out, p))) =>
        exp(inFragment, env, Nil, fragmentIn =>
          acc(outFragmemt, env, Nil, fragmentOut => {
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
                    updatedGen.cmd(p, env updatedIdentEnv (in -> xIIn) updatedIdentEnv (out -> xIOut))))))}))

      case Assign(_, lhsAcc, rhs) =>
        //noinspection VariablePatternShadow
        exp(rhs, env, Nil, e =>
          acc(lhsAcc, env, Nil, {
            case C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(outputShared, pipe))
              if name.startsWith("nvcuda::experimental::memcpy_async") =>
                C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(outputShared, e, pipe)))
            case a => C.AST.ExprStmt(C.AST.Assignment(a, e))}))

      case SyncThreads() => cuda.ast.SynchronizeThreads()

      case SyncWarp() => cuda.ast.SynchronizeWarp()

      case SyncPipeline(pipe) =>
        exp(pipe, env, Nil, pipe =>
          C.AST.ExprStmt(C.AST.StructMemberAccess(pipe, C.AST.DeclRef("commit_and_wait()"))))

      case _ => super.cmd(phrase, env)
    }
  }

  def inferFragment[T <: BasePhraseType](matrix: Phrase[T], env: Environment, m: Nat, n: Nat, k: Nat, fragmentType: FragmentType): (MatrixLayout, ArithExpr) = {
    val rows = fragmentType match {
      case FragmentType.AMatrix =>
        m
      case FragmentType.BMatrix =>
        k
      case FragmentType.Acuumulator =>
        m
    }

    val index00 = matrixIndexAsNat(matrix, env, 0, 0)
    val indexNextRowBlock_RowMajor = matrixIndexAsNat(matrix, env, rows, 0)
    val indexNextRowBlock_ColumnMajor = matrixIndexAsNat(matrix, env, 0, rows)
    val diffWithVars = (indexNextRowBlock_RowMajor - indexNextRowBlock_ColumnMajor).enforceSimplification

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
        matrixIndexAsNat(matrix, env, 1, 0)
      else
        matrixIndexAsNat(matrix, env, 0, 1)) -
      index00

    (matrixLayout, ldm)
  }

  def matrixIndexAsNat[T <: BasePhraseType](matrix: Phrase[T], env: Environment, index1: Nat, index2: Nat): ArithExpr = {
    var result: Expr = null

    matrix.t match {
      case AccType(_) =>
        acc(matrix.asInstanceOf[Phrase[AccType]], env, List(CIntExpr(index1), CIntExpr(index2)), matrixAst => {
          result = matrixAst.asInstanceOf[C.AST.ArraySubscript].index
          C.AST.Comment("This should not be generated!")})
      case ExpType(_, _) =>
        exp(matrix.asInstanceOf[Phrase[ExpType]], env, List(CIntExpr(index1), CIntExpr(index2)), matrixAst => {
          result = matrixAst.asInstanceOf[C.AST.ArraySubscript].index
          C.AST.Comment("This should not be generated!")})
      case _ =>
        throw new Exception("This should not happen!")
    }


    result.asInstanceOf[C.AST.ArithmeticExpr].ae
  }

  override def acc(phrase: Phrase[AccType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case GlobalToSharedAcc(_, pipe, outputShared) => path match {
        case (i: CIntExpr) :: ps =>
          exp(pipe, env, Nil, pipe =>
            acc(outputShared, env, i :: ps, outputShared =>
              cont(
                C.AST.FunCall(
                  C.AST.DeclRef("nvcuda::experimental::memcpy_async"),
                  immutable.Seq(outputShared, pipe)))))

        case _ => error(s"Expected path to be not empty")
      }

      case AsScalarAcc(_, m, dt, a) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
          acc(a, env, CIntExpr((i * m) + j) :: ps, cont)

        case (i : CIntExpr) :: Nil =>
          acc(a, env,CIntExpr(i * m) :: Nil, array => {
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

      case _ => super.acc(phrase, env, path, cont)
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path, cont: Expr => Stmt): Stmt = phrase match {
    case AsVectorAligned(n, _, _, dt, e) => path match {
      case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
        exp(e, env, CIntExpr((i * n) + j) :: ps, cont)

      case (i : CIntExpr) :: Nil =>
        exp(e, env, CIntExpr(i * n) :: Nil, array => {
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

    case _ => super.exp(phrase, env, path, cont)
  }

  override def typ(dt: DataType): Type = dt match {
    case Fragment(m, n, k, dataType, FragmentType.AMatrix, layout) =>
      cuda.ast.WmmaAMatrix(m, n, k, typ(dataType), layout)
    case Fragment(m, n, k, dataType, FragmentType.BMatrix, layout) =>
      cuda.ast.WmmaBMatrix(m, n, k, typ(dataType), layout)
    case Fragment(m, n, k, dataType, FragmentType.Acuumulator, _) =>
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

  protected object CudaCodeGen {
    def codeGenCudaParFor(f: CudaParFor,
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
          C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) +
            range.step))

        Phrase.substitute(a `@` i, `for` = o, `in` = p) |> (p =>

          env.updatedIdentEnv(i -> cI) |> (env => {

            range.numVals match {
              // iteration count is 0 => skip body; no code to be emitted
              case Cst(0) =>
                C.AST.Comment("iteration count is 0, no loop emitted")
              // iteration count is 1 => no loop
              case Cst(1) =>
                C.AST.Stmts(C.AST.Stmts(
                  C.AST.Comment("iteration count is exactly 1, no loop emitted"),
                  C.AST.DeclStmt(C.AST.VarDecl(
                    cI.name,
                    C.AST.Type.int,
                    init = Some(C.AST.ArithmeticExpr(f.init))))),
                  updatedGen.cmd(p, env))
              /* FIXME?
            case _ if (range.start.min.min == Cst(0) && range.stop == Cst(1)) ||
                      (range.numVals.min == NegInf && range.numVals.max == Cst(1)) =>
              C.AST.Block(collection.Seq(
                C.AST.DeclStmt(init),
                C.AST.IfThenElse(cond, updatedGen.cmd(p, env) , None)
              ))
               */
              // default case
              case _ =>
                C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
                  C.AST.Block(immutable.Seq(updatedGen.cmd(p, env))))
            }
          }))
      })
    }

  }

}
