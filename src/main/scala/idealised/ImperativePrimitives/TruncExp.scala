package idealised.ImperativePrimitives

import idealised.Compiling.RewriteToImperative
import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.MapI

import scala.xml.Elem

final case class TruncExp(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = exp"[$m.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt]") -> `type`
  }

  override def inferTypes: TruncExp = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) =>
        TruncExp(n_, m, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    TruncExp(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(array)(λ(exp"[$n.$dt]")(e =>
      MapI(m, dt, dt, λ(ExpType(dt))(e => λ(AccType(dt))(a => acc(e)(a))), e, A)))
  }

  override def rewriteToImperativeCon(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] =
    RewriteToImperative.con(array)(λ(exp"[$n.$dt]")(e => C(TruncExp(n, m, dt, e))))

  override def xmlPrinter: Elem =
    <truncExp n={n.toString} m={m.toString} dt={dt.toString}>
      {Core.xmlPrinter(array)}
    </truncExp>

  override def prettyPrint: String = s"(truncExp $array)"

}
