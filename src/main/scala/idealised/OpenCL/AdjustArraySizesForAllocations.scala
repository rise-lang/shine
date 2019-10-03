package idealised.OpenCL

import idealised.DPIA.DSL.identifier
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.FunctionalPrimitives._
import idealised.OpenCL.ImperativePrimitives._

object AdjustArraySizesForAllocations {
  case class DataTypeAdjustment(accF: Phrase[AccType] => Phrase[AccType],
                                exprF: Phrase[ExpType] => Phrase[ExpType],
                                dt: DataType)

  trait ParallelismInfo
  case class BasicInfo(parallLevel: ParallelismLevel, dim: Int) extends ParallelismInfo
  case class RecordInfo(fst: List[ParallelismInfo], snd: List[ParallelismInfo]) extends ParallelismInfo

  def apply[T <: PhraseType](p: Phrase[T], dt: DataType, addrSpace: AddressSpace): DataTypeAdjustment = {

    val parallInfo = visitAndGatherInformation(p, List.empty).reverse
    val adjDataType = adjustedSizeDataType(dt, parallInfo, addrSpace)
    val adjAcc = adjustedAcceptor(parallInfo, adjDataType, dt, addrSpace) _
    val adjExpr = adjustedExpr(parallInfo, adjDataType, dt, addrSpace) _

    DataTypeAdjustment(adjAcc, adjExpr, adjDataType)
  }

  private def visitAndGatherInformation[T <: PhraseType](p: Phrase[T],
                                                 parallInfo: List[ParallelismInfo]): List[ParallelismInfo] = {
    p match {
      case mG@MapGlobal(dim) => visitAndGatherInformation(mG.f, BasicInfo(Global, dim) :: parallInfo)
      case mWG@MapWorkGroup(dim) => visitAndGatherInformation(mWG.f, BasicInfo(WorkGroup, dim) :: parallInfo)
      case mL@MapLocal(dim) => visitAndGatherInformation(mL.f, BasicInfo(Local, dim) :: parallInfo)
      case mS: MapSeq => visitAndGatherInformation(mS.f, BasicInfo(Sequential, -1) :: parallInfo)
      case mS: MapSeqUnroll => visitAndGatherInformation(mS.f, BasicInfo(Sequential, -1) :: parallInfo)

      // FIXME: works for scalars
      case _: OpenCLReduceSeq | _: OpenCLIterate => parallInfo

      case t: Record => {
        val fstInfo = visitAndGatherInformation(t.fst, List.empty)
        val sndInfo = visitAndGatherInformation(t.snd, List.empty)
        RecordInfo(fstInfo, sndInfo) :: parallInfo
      }

      case Lambda(_, p) => visitAndGatherInformation(p, parallInfo)
      case Fst(_, _, p) => visitAndGatherInformation(p, parallInfo) match {
        case Nil => Nil
        case RecordInfo(fst, _) :: Nil => fst
        case pi => error(s"did not expect $pi")
      }
      case Snd(_, _, p) => visitAndGatherInformation(p, parallInfo) match {
        case Nil => Nil
        case RecordInfo(_, snd) :: Nil => snd
        case pi => error(s"did not expect $pi")
      }

      case _: Identifier[_] | _: Literal | _: Natural |
           _: VectorFromScalar | _: Cast | _: ForeignFunction |
           _: BinOp | _: UnaryOp => parallInfo

      case pattern => throw new Exception(s"this should not happen for now: $pattern")
    }
  }

  private def adjustedAcceptor(parallInfo: List[ParallelismInfo],
                       adjDt: DataType,
                       oldDt: DataType,
                       addrSpace: AddressSpace)
                      (A: Phrase[AccType]): Phrase[AccType] = {
    if (parallInfo.isEmpty) A
    else {

      (adjDt, oldDt) match {
        case (ArrayType(adjSize, adjElemT), ArrayType(oldSize, oldElemT)) =>
          val (parallLevel, dim) = parallInfo match {
            case (bi: BasicInfo) :: _ => (bi.parallLevel, bi.dim)
            case _ => throw new Exception("This should never happen.")
          }
          val stride = determineStride(parallLevel, dim, addrSpace)

          val outerDimension = IdxDistributeAcc(adjSize, oldSize, stride, parallLevel, adjElemT, A)

          val arr = identifier(freshName("x"), acc"[$adjElemT]")
          val mapFunBody = adjustedAcceptor(parallInfo.tail, adjElemT, oldElemT, addrSpace)(arr)

          MapAcc(oldSize, adjElemT, mapFunBody.t.dataType, Lambda(arr, mapFunBody), outerDimension)

        case (RecordType(adjDt1, adjDt2), RecordType(oldDt1, oldDt2)) =>
          parallInfo match {
            case (ri: RecordInfo) :: _ => RecordAcc(oldDt1, oldDt2,
              adjustedAcceptor(ri.fst, adjDt1, oldDt1, addrSpace)(RecordAcc1(adjDt1, adjDt2, A)),
              adjustedAcceptor(ri.snd, adjDt2, oldDt2, addrSpace)(RecordAcc2(adjDt1, adjDt2, A)))
            case _ => throw new Exception("This should never happen.")
          }

        case _ => throw new Exception(s"Expected array types but found ajdDt: $adjDt, oldDt: $oldDt.")
      }
    }
  }

  private def adjustedExpr(parallInfo: List[ParallelismInfo],
                   adjDt: DataType,
                   oldDt: DataType,
                   addrSpace: AddressSpace)
                  (E: Phrase[ExpType]): Phrase[ExpType] = {
    if (parallInfo.isEmpty) E
    else {

      (adjDt, oldDt) match {
        case (ArrayType(adjSize, adjElemT), ArrayType(oldSize, oldElemT)) =>
          val (parallLevel, dim) = parallInfo match {
            case (bi: BasicInfo) :: _ => (bi.parallLevel, bi.dim)
            case _ => throw new Exception("This should never happen.")
          }
          val stride = determineStride(parallLevel, dim, addrSpace)

          val outerDimension = IdxDistribute(adjSize, oldSize, stride, parallLevel, adjElemT, E)

          val arr = identifier(freshName("arr"), exp"[$adjElemT, $read]")
          val mapFunBody = adjustedExpr(parallInfo.tail, adjElemT, oldElemT, addrSpace)(arr)

          Map(oldSize, adjElemT, mapFunBody.t.dataType, Lambda(arr, mapFunBody), outerDimension)

        case (RecordType(adjDt1, adjDt2), RecordType(oldDt1, oldDt2)) =>
          parallInfo match {
            case (ri: RecordInfo) :: _ => Record(oldDt1, oldDt2,
              adjustedExpr(ri.fst, adjDt1, oldDt1, addrSpace)(Fst(adjDt1, adjDt2, E)),
              adjustedExpr(ri.snd, adjDt2, oldDt2, addrSpace)(Snd(adjDt1, adjDt2, E)))
            case _ => throw new Exception("This should never happen.")
          }

        case _ => throw new Exception(s"Found unexpected ajdDt: $adjDt, oldDt: $oldDt.")
      }
    }
  }

  private def determineStride(parallLevel: ParallelismLevel,
                              dim: Int,
                              addrSpace: AddressSpace): Nat = {
    //TODO think about this more thoroughly
    (parallLevel, addrSpace) match {
      case (Global, AddressSpace.Global) => 1
      case (Global, AddressSpace.Local) => ???
      case (Global, AddressSpace.Private) => get_global_size(dim)
      case (WorkGroup, AddressSpace.Global) => 1
      case (WorkGroup, _) => ???
      case (Local, AddressSpace.Private) => get_local_size(dim)
      case (Local, _) => 1
      case (Sequential, _) => 1
      case _ => throw new Exception("This should never happen.")
    }
  }

  private def adjustedSizeDataType(oldDt: DataType,
                                   info: List[ParallelismInfo],
                                   addrSpace: AddressSpace): DataType = {
    def ceilingDiv(a: Nat, b: Nat): Nat = (a + b - 1) / b

    (oldDt, info) match {
      case (ArrayType(n, elemType), (i: BasicInfo) :: is) => {
        (i, addrSpace) match {
          case (_, AddressSpace.Global) => oldDt

          //TODO what to do here?
          case (BasicInfo(Global, _), AddressSpace.Local) => ???

          case (BasicInfo(WorkGroup, dim), AddressSpace.Local) =>
            ArrayType(ceilingDiv(n, get_num_groups(dim)), adjustedSizeDataType(elemType, is, addrSpace))

          case (BasicInfo(Local, _), AddressSpace.Local) => ArrayType(n, adjustedSizeDataType(elemType, is, addrSpace))

          case (BasicInfo(Sequential, _), AddressSpace.Local) => ArrayType(n, adjustedSizeDataType(elemType, is, addrSpace))


          case (BasicInfo(Global, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_global_size(dim)), adjustedSizeDataType(elemType, is, addrSpace))

          case (BasicInfo(WorkGroup, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_num_groups(dim)), adjustedSizeDataType(elemType, is, addrSpace))

          case (BasicInfo(Local, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_local_size(dim)), adjustedSizeDataType(elemType, is, addrSpace))

          case (BasicInfo(Sequential, _), AddressSpace.Private) => ArrayType(n, adjustedSizeDataType(elemType, is, addrSpace))

          case _ => throw new Exception("This should never happen.")
        }
      }
      //TODO think about this again
      case (RecordType(dt1, dt2), (i: RecordInfo) :: is) =>
        RecordType(adjustedSizeDataType(dt1, i.fst, addrSpace), adjustedSizeDataType(dt2, i.snd, addrSpace))

      case _ => oldDt
    }
  }
}
