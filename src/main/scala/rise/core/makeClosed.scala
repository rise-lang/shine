package rise.core

import rise.core.types._

object makeClosed {
  def apply(e: Expr): Expr = withCount(e)._1
  def withCount(e: Expr): (Expr, Int) = {
    val (_, ftvs) = IsClosedForm.varsToClose(e)
    val expr = ftvs.foldLeft(e)((acc, ftv) => acc match {
      case expr => ftv match {
        case i: TypeIdentifier =>
          DepLambda[TypeKind](i, expr)(DepFunType[TypeKind, Type](i, expr.t))
        case i: DataTypeIdentifier =>
          DepLambda[DataKind](i, expr)(DepFunType[DataKind, Type](i, expr.t))
        case i: NatIdentifier =>
          DepLambda[NatKind](i, expr)(DepFunType[NatKind, Type](i, expr.t))
        case i: AddressSpaceIdentifier =>
          DepLambda[AddressSpaceKind](i, expr)(DepFunType[AddressSpaceKind, Type](i, expr.t))
        case i: NatToDataIdentifier =>
          DepLambda[NatToDataKind](i, expr)(DepFunType[NatToDataKind, Type](i, expr.t))
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    (expr, ftvs.length)
  }
}
