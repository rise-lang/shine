package idealised.DPIA.Phrases

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage
import lift.arithmetic.{NamedVar, RangeAdd}

sealed trait Phrase[T <: PhraseType] {
  final lazy val t: T = TypeOf(this)
}

final case class Identifier[T <: PhraseType](name: String, `type`: T)
  extends Phrase[T]

final case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param: Identifier[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2]

final case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

final case class NatDependentLambda[T <: PhraseType](x: NatIdentifier, body: Phrase[T])
  extends Phrase[`(nat)->`[T]]

final case class TypeDependentLambda[T <: PhraseType](x: DataTypeIdentifier, body: Phrase[T])
  extends Phrase[`(dt)->`[T]]

final case class NatDependentApply[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: Nat)
  extends Phrase[T]

final case class TypeDependentApply[T <: PhraseType](fun: Phrase[`(dt)->`[T]], arg: DataType)
  extends Phrase[T]

final case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2]

final case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1]

final case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2]

final case class IfThenElse[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class UnaryOp(op: SurfaceLanguage.Operators.Unary.Value, p: Phrase[ExpType])
  extends Phrase[ExpType]

final case class BinOp(op: SurfaceLanguage.Operators.Binary.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

final case class Literal(d: OperationalSemantics.Data)
  extends Phrase[ExpType]

object Phrase {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (`for` == p) {
          Stop(phrase.asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }

//      override def apply(ae: Nat):Nat = (phrase, `for`) match {
//        case (
//          Identifier(pName, ExpType(IndexType(pN))),
//          Identifier(forName, ExpType(IndexType(forN)))
//          ) =>
//          val pVar = NamedVar(pName, RangeAdd(0, pN, 1))
//          val forVar = NamedVar(forName, RangeAdd(0, forN, 1))
//          Nat.substitute(pVar, `for`=forVar, `in` = ae)
//        case _ => ae
//      }
//
//      override def apply[T <: DataType](dt: T):T = (phrase, `for`) match {
//        case (
//          Identifier(pName, ExpType(IndexType(pN))),
//          Identifier(forName, ExpType(IndexType(forN)))
//          ) =>
//          val pVar = NamedVar(pName, RangeAdd(0, pN, 1))
//          val forVar = NamedVar(forName, RangeAdd(0, forN, 1))
//          DataType.substitute(pVar, `for`=forVar, `in`=dt)
//        case _ => dt
//      }
    }

    VisitAndRebuild(in, Visitor)
  }

  def substitute[T2 <: PhraseType](substitutionMap: Map[Phrase[_], Phrase[_]],
                                   in: Phrase[T2]): Phrase[T2] = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (substitutionMap.isDefinedAt(p)) {
          Stop(substitutionMap(p).asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }
}

sealed trait Primitive[T <: PhraseType] extends Phrase[T] {
  def `type`: T

  def prettyPrint: String

  def xmlPrinter: xml.Elem

  def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[T]
}

trait ExpPrimitive extends Primitive[ExpType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType]

  def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType]
}

trait GeneratableExp {
  def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment, path: Path): Expr
}

trait AccPrimitive extends Primitive[AccType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier
}

trait GeneratableAcc {
  def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment, path: Path): Expr
}

trait CommandPrimitive extends Primitive[CommandType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store
}

trait Intermediate[T <: PhraseType] {
  def substituteImpl(env: SubstituteImplementations.Environment): Phrase[T]
}

trait GeneratableCommand {
  def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment): Stmt
}
