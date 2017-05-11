package idealised.Compiling

import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.MapI

import scala.language.reflectiveCalls

object RewriteToImperative {

  def apply(p: Phrase[ExpType]): Phrase[CommandType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def acc(E: Phrase[ExpType])(A: Phrase[AccType]): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => A :=|x.t.dataType| x

      case c: Literal => A :=|c.t.dataType| c

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          A := UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            A := BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => ep.rewriteToImperativeAcc(A)

      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun)(arg))(A)
      case NatDependentApply(fun, arg) => acc(Lifting.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApply(fun, arg) => acc(Lifting.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if` (x) `then` acc(thenP)(A) `else` acc(elseP)(A)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def con(E: Phrase[ExpType])(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => ep.rewriteToImperativeCon(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => con(Lifting.liftFunction(fun)(arg))(C)
      case NatDependentApply(fun, arg) => con(Lifting.liftNatDependentFunction(fun)(arg))(C)
      case TypeDependentApply(fun, arg) => con(Lifting.liftTypeDependentFunction(fun)(arg))(C)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if`(x) `then` con(thenP)(C) `else` con(elseP)(C)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def compoundAssign(dt: DataType, A: Phrase[AccType], E: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      case _: BasicType => A := E

      case ArrayType(n, et) =>
        MapI(n, et, et, λ(ExpType(et))(x => λ(AccType(et))(a => a :=|et| x )), E, A)

      case RecordType(dt1, dt2) =>
        (recordAcc1(dt1, dt2, A) :=|dt1| E) `;` (recordAcc2(dt1, dt2, A) :=|dt2| E)

      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }

}
