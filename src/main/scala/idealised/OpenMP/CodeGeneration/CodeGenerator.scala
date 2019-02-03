package idealised.OpenMP.CodeGeneration

import idealised.C
import idealised.C.AST.{ArraySubscript, Assignment, Decl}
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.{AsScalar, AsVector, ForeignFunction}
import idealised.DPIA.ImperativePrimitives.{AsScalarAcc, AsVectorAcc, ForVec}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{ArrayData, VectorData}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, PhraseType, ScalarType, VectorType}
import idealised.DPIA.{Nat, NatIdentifier, Phrases, error, freshName}
import idealised.OpenMP.ImperativePrimitives.{ParFor, ParForNat}
import lift.arithmetic
import lift.arithmetic._

import scala.collection.immutable.VectorBuilder
import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends CCodeGenerator(decls, ranges)
{
  override def name: String = "OpenMP"

  override def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))


  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case ParFor(n, dt, a, Lambda(i, Lambda(o, p))) => OpenMPCodeGen.codeGenParFor(n, dt, a, i, o, p, env)
      case ForVec(n, dt, a, Lambda(i, Lambda(o, p))) => OpenMPCodeGen.codeGenParForVec(n, dt, a, i, o, p, env)
      case ParForNat(n, i_dt, dt, a, NatDependentLambda(i, Lambda(o, p))) => OpenMPCodeGen.codeGenParForNat(n, i_dt, dt, a, i, o, p, env)
      case _ => super.cmd(phrase, env)
    }
  }

  override def acc(phrase: Phrase[AccType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case AsVectorAcc(n, _, _, a) => path match {
        case i :: ps =>     acc(a, env, (i / n) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalarAcc(_, m, dt, a) => path match {
        case i :: j :: ps =>
          acc(a, env, (i * m) + j :: ps, cont)

        case i :: Nil =>
          acc(a, env, (i * m) :: Nil, {
            case ArraySubscript(v, idx) =>
              // emit something like: ((struct float4 *)v)[idx]
              val ptrType = C.AST.PointerType(typ(VectorType(m, dt)))
              cont( C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx) )
          })
        case _ =>           error(s"Expected path to be not empty")
      }
      case _ =>             super.acc(phrase, env, path, cont)
    }
  }

  override def exp(phrase: Phrase[ExpType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case Phrases.Literal(n) => (path, n.dataType) match {
        case (Nil, _: VectorType)       => cont(OpenMPCodeGen.codeGenLiteral(n))
        case (i :: Nil, _: VectorType)  => cont(C.AST.ArraySubscript(OpenMPCodeGen.codeGenLiteral(n), C.AST.ArithmeticExpr(i)))
        case _ => super.exp(phrase, env, path, cont)
      }
      case UnaryOp(op, e) => phrase.t.dataType match {
        case _: VectorType => path match {
          case i :: ps => exp(e, env, i :: ps, e => cont(CCodeGen.codeGenUnaryOp(op, e)))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path, cont)
      }
      case BinOp(op, e1, e2) => phrase.t.dataType match {
        case _: VectorType => path match {
          case i :: ps =>
            exp(e1, env, i :: ps, e1 =>
              exp(e2, env, i :: ps, e2 =>
                cont(CCodeGen.codeGenBinaryOp(op, e1, e2))))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path, cont)
      }
      case ForeignFunction(f, inTs, outT, args) =>
        OpenMPCodeGen.codeGenForeignFunction(f, inTs, outT, args, env, path, cont)
      case AsVector(n, _, dt, e) => path match {
        case i :: j :: ps =>
          exp(e, env, (i * n) + j :: ps, cont)

        case i :: Nil =>
          exp(e, env, (i * n) :: Nil, {
            case ArraySubscript(v, idx) =>
              // emit something like: ((struct float4 *)v)[idx]
              val ptrType = C.AST.PointerType(typ(VectorType(n, dt)))
              cont( C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx) )
          })
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalar(_, m, _, e) => path match {
        case i :: ps =>     exp(e, env, (i / m) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      case _ =>             super.exp(phrase, env, path, cont)
    }
  }

  override def typ(dt: DataType): Type = {
    dt match {
      case v: idealised.DPIA.Types.VectorType =>
        // this sets the representation of vector types in C:
        // struct float4 {
        //    float data[4];
        // };
        C.AST.StructType(v.toString,
          immutable.Seq((C.AST.ArrayType(typ(v.elemType), Some(v.size)), "data")))
      case _ => super.typ(dt)
    }
  }

  protected object OpenMPCodeGen {
    def codeGenParFor(n: Nat,
                      dt: DataType,
                      a: Phrase[AccType],
                      i: Identifier[ExpType],
                      o: Phrase[AccType],
                      p: Phrase[CommandType],
                      env: Environment): Stmt = {
      val i_ = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(i_.name, range)

      val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

      C.AST.Stmts(
        C.AST.Code("#pragma omp parallel for"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@` i, `for` = o, `in` = p), env updatedIdentEnv (i -> i_))))))
    }

    def codeGenParForNat(n: Nat,
                         i_dt: NatIdentifier,
                         dt: DataType,
                         a: Phrase[AccType],
                         i: NatIdentifier,
                         o: Phrase[AccType],
                         p: Phrase[CommandType],
                         env: Environment): Stmt = {

      val i_ = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(i_.name, range)

      val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

      //FIRST we must substitute in the indexing of o in the phrase
      val pSub = Phrase.substitute(a `@d` i, `for` = o, `in` = p)
      //THEN and only THEN we can change the type to use the new index var
      val pSub2 = PhraseType.substitute(NamedVar(i_.name, range), `for` = i, in = pSub)

      C.AST.Stmts(
        C.AST.Code("#pragma omp parallel for"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(updatedGen.cmd(pSub2, env)))))
    }

    def codeGenParForVec(n: Nat,
                         dt: DataType,
                         a: Phrase[AccType],
                         i: Identifier[ExpType],
                         o: Phrase[AccType],
                         p: Phrase[CommandType],
                         env: Environment): Stmt = {
      val i_ = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(i_.name, range)

      val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

      C.AST.Stmts(
        C.AST.Code("#pragma omp simd"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@v` i, `for` = o, `in` = p), env updatedIdentEnv (i -> i_))))))
    }

    def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
      d match {
        case VectorData(vector) => CCodeGen.codeGenLiteral(ArrayData(vector))
        case _ => CCodeGen.codeGenLiteral(d)
      }
    }

    def codeGenForeignFunction(funDecl: ForeignFunction.Declaration,
                               inTs: collection.Seq[DataType],
                               outT: DataType,
                               args: collection.Seq[Phrase[ExpType]],
                               env: Environment,
                               ps: Path,
                               cont: Expr => Stmt): Stmt = {
      (outT, ps) match {
        case (_: ScalarType, Nil) =>
          CCodeGen.codeGenForeignFunction(funDecl, inTs, outT, args, env, ps, cont)

        // This has to be generalised at some point ...
        case (VectorType(_, elemType), i :: Nil) =>
          // this is not really generic, to treat all arguments the same ...
          val inTs_ = inTs.map { case VectorType(_, et) => et }
          addDeclaration(
            C.AST.FunDecl(funDecl.name,
              returnType = typ(elemType),
              params = (funDecl.argNames zip inTs_).map {
                case (name, dt) => C.AST.ParamDecl(name, typ(dt))
              },
              body = C.AST.Code(funDecl.body)
            )
          )

          //noinspection VariablePatternShadow
          def iter(args: collection.Seq[Phrase[ExpType]], res: VectorBuilder[Expr]): Stmt = {
            args match {
              case a +: args =>
                exp(a, env, i :: Nil, a => iter(args, res += a))
              case _ => cont(
                C.AST.FunCall(C.AST.DeclRef(funDecl.name), res.result()))
            }
          }

          iter(args, new VectorBuilder())

        case _ =>
          throw new Exception(s"Can not generate fun call to $funDecl with current path $ps")
      }
    }
  }
}
