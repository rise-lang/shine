package ExpPatterns

import CommandPatterns._
import Core._
import Core.OperationalSemantics._
import DSL._
import Compiling.RewriteToImperative
import apart.arithmetic.ArithExpr

import scala.xml.Elem

abstract class AbstractMap(n: ArithExpr,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType -> ExpType],
                           array: Phrase[ExpType],
                           makeMap: (ArithExpr, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => AbstractMap,
                           makeMapI: (ArithExpr, DataType, DataType, Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI)
  extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    f.t =?= t"exp[$dt1] -> exp[$dt2]"
    array.t =?= exp"[$n.$dt1]"
    exp"[$n.$dt2]"
  }

  override def inferTypes(): AbstractMap = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt1_)) =>
        val f_ = TypeInference.setParamType(f, exp"[$dt1_]")
        f_.t match {
          case FunctionType(ExpType(dt1__), ExpType(dt2_)) =>
            if (dt1_ == dt1__) {
              makeMap(n_, dt1_, dt2_, f_, array_)
            } else {
              error(s"$dt1__", s"$dt1_")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    makeMap(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(LiteralPhrase(x))) })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    val F = f
    val E = array

    exp(E)(λ(ExpType(ArrayType(n, dt1))) { x =>
      makeMapI(n, dt1, dt2, A,
        λ(AccType(dt2))(o =>
          λ(ExpType(dt1))(x => acc(F(x))(o))),
        x
      )
    })

  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    `new`(ArrayType(n, dt2), GlobalMemory, tmp =>
      acc(this)(tmp.wr) `;`
        C(tmp.rd)
    )
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> ExpType(dt2))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Core.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

case class Map(n: ArithExpr,
               dt1: DataType,
               dt2: DataType,
               f: Phrase[ExpType -> ExpType],
               array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array, Map, MapI)