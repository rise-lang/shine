package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class TruncExp(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt]") -> exp"[$m.$dt]"

//  override def inferTypes: TruncExp = {
//    import TypeInference._
//    val array_ = TypeInference(array)
//    array_.t match {
//      case ExpType(ArrayType(n_, dt_)) =>
//        TruncExp(n_, m, dt_, array_)
//      case x => error(x.toString, "ArrayType")
//    }
//  }

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    TruncExp(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(array)(λ(exp"[$n.$dt]")(e => A :=|ArrayType(m, dt)| TruncExp(n, m, dt, e)) )
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(array)(λ(exp"[$n.$dt]")(x => C(TruncExp(n, m, dt, x))))
  }

  override def xmlPrinter: Elem =
    <truncExp n={n.toString} m={m.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </truncExp>

  override def prettyPrint: String = s"(truncExp $array)"

}
