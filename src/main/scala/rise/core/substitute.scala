package rise.core

import rise.core.semantics.NatData
import rise.core.traverse._
import rise.core.types.DataType._
import rise.core.types._
import util.monads._

object substitute {

  // substitute in Expr

  def kindInExpr[T, I](kind: Kind[T, I], x: T, `for`: I, in: Expr): Expr =
    (kind, x, `for`) match {
      case (DataKind, dt: DataType, forDt: DataTypeIdentifier) =>
        dataTypeInExpr(dt, forDt, in)
      case (NatKind, n: Nat, forN: NatIdentifier) => natInExpr(n, forN, in)
      case (AddressSpaceKind, a: AddressSpace, forA: AddressSpaceIdentifier) =>
        addressSpaceInExpr(a, forA, in)
      case (NatToNatKind, n2n: NatToNat, forN2N: NatToNatIdentifier) =>
        n2nInExpr(n2n, forN2N, in)
      case (NatToDataKind, n2d: NatToData, forN2D: NatToDataIdentifier) =>
        n2dInExpr(n2d, forN2D, in)
      case (_, _, _) => ???
    }

  def exprInExpr(expression : Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends PureExprTraversal {
      override def expr: Expr => Pure[Expr] = e => {
        if (`for` =~= e) {
          return_(expression)
        } else {
          e match {
            case Lambda(x, b) =>
              // See https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/subst_lambda.html
              if (x =~= `for`) return_(e)
              // FIXME: should be !(FV(expression) contains x)
              if (!(FV(b) contains x))
                super.expr(e)
              else {
                val newX = Identifier(freshName(x.name.substring(0, 1)))(x.t)
                val newB = replace.exprInExpr(newX, `for`=x, in=b)
                super.expr(Lambda(newX, newB)(e.t))
              }
            case _ => super.expr(e)
          }
        }
      }
    }
    Visitor.expr(in).unwrap
  }

  def FV(expr: Expr): Set[Identifier] = expr match {
    case i: Identifier => Set(i)
    case Lambda(x, e) => FV(e) - x
    case App(f, e) => FV(f) ++ FV(e)
    case DepLambda(_, _, e) => FV(e)
    case DepApp(_, f, _) => FV(f)
    case Literal(_) => Set()
    case TypeAnnotation(e, _) => FV(e)
    case TypeAssertion(e, _) => FV(e)
    case Opaque(e, _) => FV(e)
    case _: Primitive => Set()
  }

  def dataTypeInExpr(dt: DataType, `for`: DataTypeIdentifier, in: Expr ): Expr = {
    object Visitor extends PureTraversal {
      override def datatype : DataType => Pure[DataType] = in1 =>
        return_(substitute.typeInType(dt, `for`, in1))

      override def `type`[T <: ExprType] : T => Pure[T] = in1 =>
        return_(substitute.typeInType(dt, `for`, in1))
    }
    Visitor.expr(in).unwrap
  }

  def natsInExpr(subs: Map[Nat, Nat], in: Expr): Expr = {
    object Visitor extends PureTraversal {
      override def expr: Expr => Pure[Expr] = {
        case Identifier(name) if subs.contains(NatIdentifier(name)) =>
          return_(Literal(NatData(subs(NatIdentifier(name)))): Expr)
        case e => super.expr(e)
      }

      override def nat: Nat => Pure[Nat] = e =>
        return_(substitute.natsInNat(subs, e))

      override def `type`[T <: ExprType]: T => Pure[T] = t =>
        return_(substitute.natsInType(subs, t))
    }
    Visitor.expr(in).unwrap
  }

  def natInExpr(ae: Nat, `for`: NatIdentifier, in: Expr): Expr =
    natsInExpr(Map(`for` -> ae), in)

  def addressSpaceInExpr(a: AddressSpace,
                         `for`: AddressSpaceIdentifier,
                         in: Expr): Expr = ???

  def n2nInExpr(n2n: NatToNat, `for`: NatToNatIdentifier, in: Expr): Expr = ???

  def n2dInExpr(n2d: NatToData, `for`: NatToDataIdentifier, in: Expr): Expr =
    ???

  // substitute in Type

  def kindInType[T, I, U <: ExprType](kind: Kind[T, I], x: T, `for`: I, in: U): U = {
    (kind, x, `for`) match {
      case (DataKind, dt: DataType, forDt: DataTypeIdentifier) => typeInType(dt, forDt, in)
      case (NatKind, n: Nat, forN: NatIdentifier) => natInType(n, forN, in)
      case (AddressSpaceKind, a: AddressSpace, forA: AddressSpaceIdentifier) => addressSpaceInType(a, forA, in)
      case (NatToNatKind, n2n: NatToNat, forN2N: NatToNatIdentifier) => n2nInType(n2n, forN2N, in)
      case (NatToDataKind, n2d: NatToData, forN2D: NatToDataIdentifier) => n2dInType(n2d, forN2D, in)
      case (_, _, _) => ???
    }
  }

  def typeInType[B <: ExprType](ty: ExprType, `for`: ExprType, in: B): B = {
    object Visitor extends PureTraversal {
      override def datatype: DataType => Pure[DataType] = t => {
        if (`for` =~= t) { return_(ty.asInstanceOf[DataType]) } else super.datatype(t)
      }
      override def `type`[T <: ExprType]: T => Pure[T] = t => {
        if (`for` =~= t) { return_(ty.asInstanceOf[T]) } else super.`type`(t)
      }
    }
    rise.core.traverse.traverse(in, Visitor)
  }

  def natsInType[T <: ExprType](subs: Map[Nat, Nat], in: T): T = {
    object Visitor extends PureTraversal {
      override def nat: Nat => Pure[Nat] = in1 =>
        return_(substitute.natsInNat(subs, in1))
    }
    rise.core.traverse.traverse(in, Visitor)
  }

  def natInType[T <: ExprType](n: Nat, `for`: Nat, in: T): T =
    natsInType(Map(`for` -> n), in)

  def natInDataType(n: Nat, `for`: NatIdentifier, in: DataType): DataType = {
    object Visitor extends PureTraversal {
      override def nat: Nat => Pure[Nat] = in1 =>
        return_(substitute.natInNat(n, `for`, in1))
    }
    Visitor.datatype(in).unwrap
  }

  def addressSpaceInType[T <: ExprType](a: AddressSpace, `for`: AddressSpaceIdentifier, in: T): T = {
    object Visitor extends PureTraversal {
      override def addressSpace: AddressSpace => Pure[AddressSpace] = b =>
        if (`for` == b) return_(a) else super.addressSpace(b)
    }
    rise.core.traverse.traverse(in, Visitor)
  }

  def n2nInType[T <: ExprType](n2n: NatToNat, `for`: NatToNatIdentifier, in: T ): T = {
    object Visitor extends PureTraversal {
      override def natToNat: NatToNat => Pure[NatToNat] = n =>
        if (`for` == n) return_(n2n) else super.natToNat(n)
    }
    rise.core.traverse.traverse(in, Visitor)
  }

  def n2dInType[T <: ExprType](n2d: NatToData, `for`: NatToDataIdentifier, in: T): T = {
    object Visitor extends PureTraversal {
      override def natToData: NatToData => Pure[NatToData] = n =>
        if (`for` == n) return_(n2d) else super.natToData(n)
    }
    rise.core.traverse.traverse(in, Visitor)
  }

  // substitute in Nat

  def natsInNat(subs: Map[Nat, Nat], in: Nat): Nat = {
    import arithexpr.arithmetic.ArithExpr
    ArithExpr.substitute(in, subs)
  }

  def natInNat(ae: Nat, `for`: Nat, in: Nat): Nat =
    natsInNat(Map(`for` -> ae), in)

  // substitute in AddressSpace

  def addressSpacesInAddressSpace(
      subs: Map[AddressSpaceIdentifier, AddressSpace],
      in: AddressSpace
  ): AddressSpace = {
    in match {
      case i: AddressSpaceIdentifier =>
        subs.get(i) match {
          case Some(a) => a
          case None    => i
        }
      case a => a
    }
  }

  def addressSpaceInAddressSpace(
      a: AddressSpace,
      `for`: AddressSpaceIdentifier,
      in: AddressSpace
  ): AddressSpace = {
    if (in == `for`) {
      a
    } else {
      in
    }
  }

  //substitue in MatrixLayout

  def matrixLayoutsInMatrixLayout(
      subs: Map[MatrixLayoutIdentifier, MatrixLayout],
      in: MatrixLayout
   ): MatrixLayout = {
    in match {
      case i: MatrixLayoutIdentifier =>
        subs.get(i) match {
          case Some(a) => a
          case None    => i
        }
      case a => a
    }
  }

  def matrixLayoutInMatrixLayout(
      a: MatrixLayout,
      `for`: MatrixLayoutIdentifier,
      in: MatrixLayout
  ): MatrixLayout = {
    if (in == `for`) {
      a
    } else {
      in
    }
  }

  //substitue in FragmentType

  def fragmentTypesInFragmentType(
                                   subs: Map[FragmentIdentifier, Fragment],
                                   in: Fragment
  ): Fragment = {
    in match {
      case i: FragmentIdentifier =>
        subs.get(i) match {
          case Some(a) => a
          case None    => i
        }
      case a => a
    }
  }

  def fragmentTypeInFragmentType(
                                  a: Fragment,
                                  `for`: FragmentIdentifier,
                                  in: Fragment
  ): Fragment = {
    if (in == `for`) {
      a
    } else {
      in
    }
  }

  // substitute in NatToData
  def n2dsInN2d(
      subs: Map[NatToDataIdentifier, NatToData],
      in: NatToData
  ): NatToData = {
    in match {
      case i: NatToDataIdentifier =>
        subs.get(i) match {
          case Some(n2d) => n2d
          case None      => i
        }
      case n2d => n2d
    }
  }

  def n2dInN2d(
      n2d: NatToData,
      `for`: NatToDataIdentifier,
      in: NatToData
  ): NatToData = {
    if (in == `for`) n2d else in
  }

  def n2nsInN2n(
                 subs: Map[NatToNatIdentifier, NatToNat],
                 in: NatToNat
               ): NatToNat = {
    in match {
      case i: NatToNatIdentifier =>
        subs.get(i) match {
          case Some(n2d) => n2d
          case None      => i
        }
      case n2d => n2d
    }
  }

  def n2nsInNat(subs: Map[NatToNatIdentifier, NatToNat], in: Nat): Nat =
    in.visitAndRebuild({
      case n2n @ NatToNatApply(f: NatToNatIdentifier, n) =>
        subs.get(f).map({
          case NatToNatLambda(x, body) => substitute.natInNat(n, x, body)
          case id: NatToNatIdentifier => NatToNatApply(id, n)
        }).getOrElse(n2n)
      case x => x
    })

  def natCollectionInNatCollection(
      subs: Map[NatCollectionIdentifier, NatCollection],
      in: NatCollection): NatCollection = {
    in match {
      case ident: NatCollectionIdentifier=>
        subs(ident)
      case _ => throw new Exception("No idea how to substitute")
    }
  }
}
