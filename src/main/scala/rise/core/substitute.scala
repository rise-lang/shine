package rise.core

import rise.core.types._
import rise.core.semantics.NatData
import traversal.{Continue, Result, Stop}

object substitute {

  // substitute in Expr

  def kindInExpr[K <: Kind](x: K#T, `for`: K#I, in: Expr): Expr =
    (x, `for`) match {
      case (dt: DataType, forDt: DataTypeIdentifier) =>
        dataTypeInExpr(dt, forDt, in)
      case (n: Nat, forN: NatIdentifier) => natInExpr(n, forN, in)
      case (a: AddressSpace, forA: AddressSpaceIdentifier) =>
        addressSpaceInExpr(a, forA, in)
      case (n2n: NatToNat, forN2N: NatToNatIdentifier) =>
        n2nInExpr(n2n, forN2N, in)
      case (n2d: NatToData, forN2D: NatToDataIdentifier) =>
        n2dInExpr(n2d, forN2D, in)
      case (_, _) => ???
    }

  def exprInExpr(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        if (`for` == e) {
          Stop(expr)
        } else {
          e match {
            case Lambda(x, b) =>
              // See https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/subst_lambda.html
              if (x == `for`) Stop(e)
              if (!(FV(b) contains x))
                Continue(e, this)
              else {
                val newX = Identifier(freshName(x.name.substring(0, 1)))(x.t)
                val newB = replace.exprInExpr(newX, `for`=x, in=b)
                Continue(Lambda(newX, newB)(e.t), this)
              }
            case _ => Continue(e, this)
          }
        }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def FV(expr: Expr): Set[Identifier] = expr match {
    case i: Identifier => Set(i)
    case Lambda(x, e) => FV(e) - x
    case App(f, e) => FV(f) ++ FV(e)
    case DepLambda(_, e) => FV(e)
    case DepApp(f, _) => FV(f)
    case Literal(_) => Set()
    case TypeAnnotation(e, _) => FV(e)
    case TypeAssertion(e, _) => FV(e)
    case _: Primitive => Set()
  }

  def dataTypeInExpr(
      dt: DataType,
      `for`: DataTypeIdentifier,
      in: Expr
  ): Expr = {

    object Visitor extends traversal.Visitor {
      override def visitType[T <: Type](in: T): Result[T] =
        Continue(substitute.typeInType(dt, `for`, in), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def natInExpr(ae: Nat, `for`: NatIdentifier, in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        e match {
          case Identifier(name) =>
            if (`for`.name == name) {
              Stop(Literal(NatData(ae)))
            } else {
              Continue(e, this)
            }
          case _ => Continue(e, this)
        }
      }

      override def visitNat(e: Nat): Result[Nat] =
        Continue(substitute.natInNat(ae, `for`, e), this)

      override def visitType[T <: Type](t: T): Result[T] =
        Continue(substitute.natInType(ae, `for`, t), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def addressSpaceInExpr(a: AddressSpace,
                         `for`: AddressSpaceIdentifier,
                         in: Expr): Expr = ???

  def n2nInExpr(n2n: NatToNat, `for`: NatToNatIdentifier, in: Expr): Expr = ???

  def n2dInExpr(n2d: NatToData, `for`: NatToDataIdentifier, in: Expr): Expr =
    ???

  // substitute in Type

  def kindInType[K <: Kind, T <: Type](x: K#T, `for`: K#I, in: T): T =
    (x, `for`) match {
      case (dt: DataType, forDt: DataTypeIdentifier) =>
        typeInType(dt, forDt, in)
      case (n: Nat, forN: NatIdentifier) =>
        natInType(n, forN, in)
      case (a: AddressSpace, forA: AddressSpaceIdentifier) =>
        addressSpaceInType(a, forA, in)
      case (n2n: NatToNat, forN2N: NatToNatIdentifier) =>
        n2nInType(n2n, forN2N, in)
      case (n2d: NatToData, forN2D: NatToDataIdentifier) =>
        n2dInType(n2d, forN2D, in)
      case (_, _) => ???
    }

  def typeInType[B <: Type](ty: Type, `for`: Type, in: B): B = {
    case class Visitor() extends traversal.Visitor {
      override def visitType[T <: Type](t: T): traversal.Result[T] = {
        if (`for` == t) {
          Stop(ty.asInstanceOf[T])
        } else {
          Continue(t, this)
        }
      }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def natInType[T <: Type](n: Nat, `for`: NatIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitNat(in: Nat): Result[Nat] =
        Continue(substitute.natInNat(n, `for`, in), this)
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def natInDataType(n: Nat, `for`: NatIdentifier, in: DataType): DataType = {
    case class Visitor() extends traversal.Visitor {
      override def visitNat(in: Nat): Result[Nat] =
        Continue(substitute.natInNat(n, `for`, in), this)
    }

    traversal.types.DepthFirstLocalResult.data(in, Visitor())
  }

  def addressSpaceInType[T <: Type](
      a: AddressSpace,
      `for`: AddressSpaceIdentifier,
      in: T
  ): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitAddressSpace(b: AddressSpace): Result[AddressSpace] =
        if (`for` == b) {
          Stop(a)
        } else {
          Continue(b, this)
        }
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def n2nInType[T <: Type](
      n2n: NatToNat,
      `for`: NatToNatIdentifier,
      in: T
  ): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitN2N(n: NatToNat): Result[NatToNat] =
        if (`for` == n) {
          Stop(n2n)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def n2dInType[T <: Type](
      n2d: NatToData,
      `for`: NatToDataIdentifier,
      in: T
  ): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitN2D(n: NatToData): Result[NatToData] =
        if (`for` == n) {
          Stop(n2d)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  // substitute in Nat

  def natsInNat(subs: Map[NatIdentifier, Nat], in: Nat): Nat = {
    import arithexpr.arithmetic.ArithExpr
    ArithExpr.substitute(in, subs.asInstanceOf[Map[ArithExpr, ArithExpr]])
  }

  def natInNat(ae: Nat, `for`: NatIdentifier, in: Nat): Nat =
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
      subs: Map[FragmentTypeIdentifier, FragmentType],
      in: FragmentType
  ): FragmentType = {
    in match {
      case i: FragmentTypeIdentifier =>
        subs.get(i) match {
          case Some(a) => a
          case None    => i
        }
      case a => a
    }
  }

  def fragmentTypeInFragmentType(
      a: FragmentType,
     `for`: FragmentTypeIdentifier,
      in: FragmentType
  ): FragmentType = {
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
