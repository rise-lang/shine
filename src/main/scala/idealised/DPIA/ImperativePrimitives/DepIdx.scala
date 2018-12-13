package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class DepIdx(n: Nat,
                        i: NatIdentifier,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[ExpType])
  extends ExpPrimitive with GeneratableExp {

  private def makeDt(x:Nat):DataType = DataType.substitute(x, `for`=i, `in`=dt)

  override val `type`: ExpType =
    (n: Nat) -> (i:Nat) -> (dt: DataType) ->
      (index :: exp"[idx($n)]") ->
      (array :: exp"[${DepArrayType(n, makeDt)}]") ->
      exp"[${makeDt(Lifting.liftIndexExpr(index))}]"

  //  override def inferTypes: Idx = {
  //    import TypeInference._
  //    val index_ = TypeInference(index)
  //    val array_ = TypeInference(array)
  //    (index_.t, array_.t) match {
  //      case (ExpType(IndexType(n1)), ExpType(ArrayType(n2, dt_))) if n1 == n2 =>
  //        Idx(n1, dt_, index_, array_)
  //      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
  //    }
  //  }

  override def eval(s: Store): Data = {
//    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
//      case (ArrayData(xs), IntData(i)) => xs(i)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment, path: Path): Expr = {
    //gen.codeGenIdx(index, array, env, path, gen)
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepIdx(fun(n), fun(i).asInstanceOf[NatIdentifier], fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(array)})[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <depIdx n={ToString(n)} i={ToString(i)} dt={ToString(dt)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
      <index type={ToString(index.t)}>
        {Phrases.xmlPrinter(index)}
      </index>
    </depIdx>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
//    import RewriteToImperative._
//    con(array)(λ(exp"[$n.$dt]")(x => A :=| dt | Idx(n, dt, index, x)))
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
//    import RewriteToImperative._
//    con(array)(λ(exp"[$n.$dt]")(e => C(Idx(n, dt, index, e))))
    ???
  }
}

object DepIdx {
  def apply(index: Phrase[ExpType], array: Phrase[ExpType]): DepIdx = {
    (index.t, array.t) match {
      case (ExpType(IndexType(n1)), ExpType(DepArrayType(n2, i, dt_))) if n1 == n2 =>
        DepIdx(n1, i, dt_, index, array)
      case x => error(x.toString, "(exp[idx(n)], exp[n.(i:Nat) -> dt])")
    }
  }
}
