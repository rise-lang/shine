package shine.DPIA.Compilation

import rise.core.types.{DataType, read, write}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{Seq => _, _}

import scala.annotation.tailrec

object FedeTranslation {
  @tailrec
  def fedAcc(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
            (E: Phrase[ExpType])
            (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    E match {
      case ep: ExpPrimitive => primitive(env)(ep)(C)

      case x: Identifier[ExpType] =>
        env.get(x) match {
          case Some(o) => C(o)
          case None => ???
        }

      // on the fly beta-reduction
      case Apply(fun, arg) => fedAcc(env)(
        Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(kind, fun, arg) => arg match {
        case a: Nat => fedAcc(env)(
          Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(nat)->:`[ExpType]]])(a))(C)
        case a: DataType => fedAcc(env)(
          Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(dt)->:`[ExpType]]])(a))(C)
      }

      case IfThenElse(cond, thenP, elseP) => ???

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")

      case LetNat(_, _, _) => throw new Exception("This should never happen")
      case _ => ???
    }
  }

  def primitive(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
               (E: ExpPrimitive)
               (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = E match {
    case AsScalar(n, m, dt, _, array) =>
      fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
        AsScalarAcc(n, m, dt, C(o))))

    case Join(n, m, _, dt, array) =>
      fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
        JoinAcc(n, m, dt, C(o))))

    case Map(n, dt1, dt2, access, f, array) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt1, access))

      val otype = AccType(dt2)
      val o = Identifier(freshName("fede_o"), otype)

      fedAcc(env)(array)(fun(env.toList.head._2.t)(y =>
        MapAcc(n, dt2, dt1,
          Lambda(o,
            fedAcc(Predef.Map((x, o)))(f(x))(fun(otype)(x => x))), C(y))))

    case MapFst(w, dt1, dt2, dt3, f, record) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt1, write))

      val otype = AccType(dt3)
      val o = Identifier(freshName("fede_o"), otype)

      fedAcc(env)(record)(fun(env.toList.head._2.t)(y =>
        MapFstAcc(dt1, dt2, dt3,
          Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
          C(y))))

    case MapSnd(_, dt1, dt2, dt3, f, record) =>
      val x = Identifier(freshName("fede_x"), ExpType(dt2, read))

      val otype = AccType(dt3)
      val o = Identifier(freshName("fede_o"), otype)

      fedAcc(env)(record)(fun(env.toList.head._2.t)(y =>
        MapSndAcc(dt1, dt2, dt3,
          Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
          C(y))))

    case PadEmpty(n, r, dt, array) =>
      fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
        TakeAcc(n, r, dt, C(o))))

    case Reorder(n, dt, _, _, idxFinv, input) =>
      fedAcc(env)(input)(fun(accT(C.t.inT.dataType))(o =>
        ReorderAcc(n, dt, idxFinv, C(o))))

    case Split(n, m, _, dt, array) =>
      fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
        SplitAcc(n, m, dt, C(o))))

    case Transpose(n, m, dt, _, array) =>
      fedAcc(env)(array)(fun(AccType(C.t.inT.dataType))(o =>
        TransposeAcc(n, m, dt, C(o))))

    case Unzip(n, dt1, dt2, _, e) =>
      fedAcc(env)(e)(fun(accT(C.t.inT.dataType))(o =>
        UnzipAcc(n, dt1, dt2, C(o))))
  }
}
