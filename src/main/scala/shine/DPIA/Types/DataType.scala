package shine.DPIA.Types

import arithexpr.arithmetic.{ArithExpr, BigSum}
import rise.core.types.NatCollectionIndexing
import shine.DPIA
import shine.DPIA.{Nat, Types}

sealed trait DataType

sealed trait ComposedType extends DataType

sealed trait BasicType extends DataType

sealed trait ScalarType extends BasicType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object i8 extends ScalarType { override def toString: String = "i8" }
object i16 extends ScalarType { override def toString: String = "i16" }
object i32 extends ScalarType { override def toString: String = "i32" }
object i64 extends ScalarType { override def toString: String = "i64" }

object u8 extends ScalarType { override def toString: String = "u8" }
object u16 extends ScalarType { override def toString: String = "u16" }
object u32 extends ScalarType { override def toString: String = "u32" }
object u64 extends ScalarType { override def toString: String = "u64" }

object f16 extends ScalarType { override def toString: String = "f16" }
object f32 extends ScalarType { override def toString: String = "f32" }
object f64 extends ScalarType { override def toString: String = "f64" }

object NatType extends ScalarType { override def toString: String = "nat" }

final case class IndexType(size: Nat) extends BasicType {
  override def toString: String = s"idx($size)"
}

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType private (size: Nat, elemFType: NatToData)
  extends ComposedType
{
  override def toString: String = s"$size.$elemFType"

  override def equals(obj: Any): Boolean = {
    obj match {
      case DepArrayType(s2, elemT2) =>
        // TODO: FIX EQUALITY
        // size == s2 && elemT2 == elemFType
        size == s2
      case _ => false
    }
  }
}

final case class DepPairType[K <: Kind:KindName:KindReified](x:K#I, elemT:DataType)
  extends ComposedType {

  override def toString: String = s"($x:${implicitly[KindName[K]].get} ** $elemT)"

  override def equals(other: Any): Boolean = other match {
    case DepPairType(x2, elemT2) =>
      /*
      TODO: FIX
      implicitly[KindReified[K]].tryFrom(x2).exists(x2 => {
        val elemSubbed = implicitly[Types.KindReified[K]].substitute(this.x, x2, elemT2)
        this.elemT == elemSubbed
      }
      )*/
      true
    case _ => false
  }

  def visitNat(f: Nat => Nat): DepPairType[K] = {
    val visited = implicitly[KindReified[K]].visitNat(this.x, f)
    implicitly[KindReified[K]].tryIdentifier(visited) match {
      case Some(x) =>
        val newSnd = DataType.visitNat(f, this.elemT)
        DepPairType(x, newSnd)
      case None => throw new Exception(s"Failed to build identifier in dependent type first member: $visited found")
    }
  }

  def substituteNat(ae: Nat, `for`: Nat): DepPairType[K] = {
    val visited = implicitly[KindReified[K]].substituteNat(ae, `for`, this.x)
    implicitly[KindReified[K]].tryIdentifier(visited) match {
        case Some(x) =>
          val newSnd = DataType.substitute(ae, `for`, this.elemT)
          DepPairType(x, newSnd)
        case None =>
          throw new Exception(s"Failed to build identifier in dependent type first member: $visited found")
      }
  }
}


final case class PairType(fst: DataType, snd: DataType) extends ComposedType {
  override def toString: String = s"($fst x $snd)"
}

sealed case class VectorType(size: Nat, elemType: ScalarType)
  extends BasicType
{
  override def toString: String = s"<$size>$elemType"
}

object vec {
  @inline
  def apply(size: Nat, elemType: ScalarType): VectorType =
    VectorType(size, elemType)
}

final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] =
    Some((arg.f, arg.n))
}

final case class DataTypeIdentifier(name: String)
  extends DataType with Kind.Identifier {
  override def toString: String = name
}

object DataType {

  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType | _: DataTypeIdentifier => in
        case a: ArrayType =>
          ArrayType(a.size, substitute(dt, `for`, a.elemType))
        case dp: DepArrayType => ???
        case r: PairType =>
          PairType(substitute(dt, `for`, r.fst), substitute(dt, `for`, r.snd))
      }).asInstanceOf[T]
    }
  }

  def visitNat[T <: DataType](f: Nat => Nat, in: T): T = {
    (in match {
      case s: ScalarType => s
      case i: IndexType =>
        IndexType(f(i.size))
      case a: ArrayType =>
        ArrayType(f(a.size),
          visitNat(f, a.elemType))
      case a: DepArrayType =>
        val newSize = f(a.size)
        val newElemFType = a.elemFType match {
          case NatToDataLambda(x, body) =>  NatToDataLambda(x, visitNat(f, body))
          case id:NatToDataIdentifier => id
        }
        DepArrayType(newSize, newElemFType)
      case v: VectorType =>
        VectorType(f(v.size), v.elemType)
      case r: PairType =>
        PairType(visitNat(f, r.fst), visitNat(f, r.snd))
      case r: DepPairType[_] =>
        /*
        val newFst = f(r.x).asInstanceOf[NatIdentifier]
        val newSnd = visitNat(f, r.elemT)
        DepPairType(newFst, newSnd)*/
        r.visitNat(f)
    }).asInstanceOf[T]
  }

  // TODO: Re-express in terms of visitNat?
  def substitute[T <: DataType](ae: Nat, `for`: Nat, in: T): T = {
    (in match {
      case s: ScalarType => s
      case i: IndexType =>
        IndexType(ArithExpr.substitute(i.size, Map((`for`, ae))))
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
          substitute(ae, `for`, a.elemType))
      case a: DepArrayType =>
        val subMap = Map((`for`, ae))
        val newSize = ArithExpr.substitute(a.size, subMap)
        val newElemFType = substitute(ae, `for`, a.elemFType)
        DepArrayType(newSize, newElemFType)
      case v: VectorType =>
        VectorType(ArithExpr.substitute(v.size, Map((`for`, ae))), v.elemType)
      case r: PairType =>
        PairType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
      case r: DepPairType[_] =>
        r.substituteNat(ae, `for`)
    }).asInstanceOf[T]
  }

  def substitute(ae: DPIA.Nat, `for`: DPIA.Nat, in: NatToData): NatToData = {
    in match {
      case i: NatToDataIdentifier => i
      case NatToDataLambda(x, body) =>
        NatToDataLambda(x, substitute(ae, `for`, body))
    }
  }

  def substitute(ns: NatCollection, `for`: NatCollectionIdentifier, in: DataType): DataType = {
    DataType.visitNat({
      case idx: NatCollectionIndexing =>
        idx.collection match {
          case coll: rise.core.types.NatCollectionIdentifier =>
            ns match {
              case ns: NatCollectionIdentifier =>
                if (coll.name == `for`.name) {
                  new NatCollectionIndexing(coll.copy(name = ns.name), idx.idxs)
                } else {
                  idx
                }
            }
        }
      case x => x
    }, in)
  }

  def getTotalNumberOfElements(dt: DataType): Nat = dt match {
    case _: BasicType => 1
    case _: PairType => 1
    case _: DepPairType[_] => 1
    case a: ArrayType => getTotalNumberOfElements(a.elemType) * a.size
    case a: DepArrayType =>
      a.elemFType match {
        case NatToDataLambda(x, body) =>
          BigSum(from = 0, upTo = a.size - 1,
            `for` = x, `in` = getTotalNumberOfElements(body))
        case NatToDataIdentifier(_) =>
          throw new Exception("This should not happen")
      }
    case _: DataTypeIdentifier | _: NatToDataApply =>
      throw new Exception("This should not happen")
  }


  @scala.annotation.tailrec
  def getBaseDataType(dt: DataType): DataType = dt match {
    case _: BasicType => dt
    case _: PairType => dt
    case _: DepPairType[_] => dt
    case _: DataTypeIdentifier => dt
    case ArrayType(_, elemType) => getBaseDataType(elemType)
    case DepArrayType(_, NatToDataLambda(_, elemType)) =>
      getBaseDataType(elemType)
    case DepArrayType(_, _) | _: NatToDataApply =>
      throw new Exception("This should not happen")
  }

  implicit class PairTypeConstructor(dt1: DataType) {
    def x(dt2: DataType) = PairType(dt1, dt2)
  }

  implicit final class ArrayTypeConstructor(s: Nat) {
    @inline def `.`(dt: DataType): ArrayType = ArrayType(s, dt)
    @inline def `.d`(ft: NatToData): DepArrayType =
      DepArrayType(s, ft)
    @inline def `.d`(f: DPIA.NatIdentifier => DataType): DepArrayType =
      DepArrayType(s, NatToDataLambda(s, f))
  }

  @inline
  def idx(n: Nat): IndexType = IndexType(n)
}
