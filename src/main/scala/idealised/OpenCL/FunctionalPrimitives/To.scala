package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.FunctionalPrimitives.{Fst, Map, MapSeq, Record, Snd}
import idealised.DPIA.ImperativePrimitives.{MapAcc, RecordAcc, RecordAcc1}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.AddressSpace.Private
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.OpenCL.DSL.`new`
import idealised.OpenCL.ImperativePrimitives.{IdxDistribute, IdxDistributeAcc}
import idealised.OpenCL.{Global, Local, ParallelismLevel, Sequential, WorkGroup, get_global_size, get_local_size, get_num_groups}

import scala.annotation.tailrec
import scala.xml.Elem

final case class To(addrSpace: AddressSpace,
                    dt: DataType,
                    input: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (addrSpace : AddressSpace) ->: (dt: DataType) ->:
      (input :: exp"[$dt, $write]") ->: exp"[$dt, $read]"

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    To(fun.addressSpace(addrSpace), fun.data(dt), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addrSpace ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to addrSpace={ToString(addrSpace)} dt={ToString(dt)}>
      <input type={ToString(ExpType(dt, write))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </to>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    val parallInfo = visitAndGatherInformation(input, List.empty).reverse
    val adjDataType = adjustedSizeDataType(dt, parallInfo)
    val adjAcc = adjustedAcceptor(parallInfo, adjDataType, dt) _
    val adjExpr = adjustedExpr(parallInfo, adjDataType, dt) _

    println(s"The old data type: $dt : $addrSpace : $input")
    println(s"The new data type: $adjDataType")

    `new`(addrSpace)(adjDataType, tmp => acc(input)(adjAcc(tmp.wr)) `;` C(adjExpr(tmp.rd)) )
  }

  private def adjustedAcceptor(parallInfo: List[(ParallelismLevel, Int)], adjDt: DataType, oldDt: DataType)
                              (A: Phrase[AccType]): Phrase[AccType] = {
    if (parallInfo.isEmpty) A
    else {
      val (parallLevel, dim) = parallInfo.head
      val stride = determineStride(parallLevel, dim)

      (adjDt, oldDt) match {
        case (ArrayType(adjSize, adjElemT), ArrayType(oldSize, oldElemT)) =>
          val outerDimension = IdxDistributeAcc(adjSize, oldSize, stride, parallLevel, adjElemT, A)

          val arr = identifier(freshName("x"), acc"[$adjElemT]")
          val mappedBody = adjustedAcceptor(parallInfo.tail, adjElemT, oldElemT)(arr)

          MapAcc(oldSize, adjElemT, mappedBody.t.dataType, Lambda(arr, mappedBody), outerDimension)

        case (RecordType(adjDt1, adjDt2), RecordType(oldDt1, oldDt2)) =>
          RecordAcc(oldDt1, oldDt2,
            adjustedAcceptor(parallInfo, adjDt1, oldDt1)(RecordAcc1(adjDt1, adjDt2, A)),
            adjustedAcceptor(parallInfo, adjDt1, oldDt1)(RecordAcc1(adjDt1, adjDt2, A)))

        case _ => throw new Exception(s"Expected array types but found ajdDt: $adjDt, oldDt: $oldDt.")
      }
    }
  }

  private def adjustedExpr(parallInfo: List[(ParallelismLevel, Int)], adjDt: DataType,
                           oldDt: DataType)(E: Phrase[ExpType]): Phrase[ExpType] = {
    if (parallInfo.isEmpty) E
    else {
      val (parallLevel, dim) = parallInfo.head
      val stride = determineStride(parallLevel, dim)

      (adjDt, oldDt) match {
        case (ArrayType(adjSize, adjElemT), ArrayType(oldSize, oldElemT)) =>
          val outerDimension = IdxDistribute(adjSize, oldSize, stride, parallLevel, adjElemT, E)

          val arr = identifier(freshName("arr"), exp"[$adjElemT, $read]")
          val mappedBody = adjustedExpr(parallInfo.tail, adjElemT, oldElemT)(arr)

          Map(oldSize, adjElemT, mappedBody.t.dataType, Lambda(arr, mappedBody), outerDimension)

        case (RecordType(adjDt1, adjDt2), RecordType(oldDt1, oldDt2)) =>
          Record(oldDt1, oldDt2,
            adjustedExpr(parallInfo, adjDt1, oldDt1)(Fst(adjDt1, adjDt2, E)),
            adjustedExpr(parallInfo, adjDt2, oldDt2)(Snd(adjDt1, adjDt2, E)))

        case _ => throw new Exception(s"Found unexpected ajdDt: $adjDt, oldDt: $oldDt.")
      }
    }
  }

  private def determineStride(parallLevel: ParallelismLevel, dim: Int): Nat = {
    parallLevel match {
      case Global => get_global_size(dim)
      case WorkGroup => get_num_groups(dim)
      case Local => get_local_size(dim)
      case Sequential => 1
    }
  }

  private def adjustedSizeDataType(oldDt: DataType,
                           info: List[(ParallelismLevel, Int)]): DataType  = {
    def ceilingDiv(a: Nat, b: Nat): Nat = (a + b -1) / b

    (oldDt, info) match {
      case (ArrayType(n, elemType), i :: is) => {
        (i, addrSpace) match {
          case (_, AddressSpace.Global) => oldDt


          case ((Global, _), AddressSpace.Local) =>
            throw new Exception("This should probably not happen")

          case ((WorkGroup, dim), AddressSpace.Local) =>
            ArrayType(ceilingDiv(n, get_num_groups(dim)), adjustedSizeDataType(elemType, is))

          case ((Local, _), AddressSpace.Local) => ArrayType(n, adjustedSizeDataType(elemType, is))

          case ((Sequential, _), AddressSpace.Local) => ArrayType(n, adjustedSizeDataType(elemType, is))


          case ((Global, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_global_size(dim)), adjustedSizeDataType(elemType, is))

          case ((WorkGroup, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_num_groups(dim)), adjustedSizeDataType(elemType, is))

          case ((Local, dim), AddressSpace.Private) =>
            ArrayType(ceilingDiv(n, get_local_size(dim)), adjustedSizeDataType(elemType, is))

          case ((Sequential, _), AddressSpace.Private) => ArrayType(n, adjustedSizeDataType(elemType, is))

          case _ => throw new Exception("This should never happen.")
        }
      }
      //TODO Assuming same information for both elements right now.
      case (RecordType(dt1, dt2), is) => RecordType(adjustedSizeDataType(dt1, is), adjustedSizeDataType(dt2, is))

      case _ => oldDt
    }
  }

  def visitAndGatherInformation[T <: PhraseType](p: Phrase[T], parallInfo: List[(ParallelismLevel, Int)]): List[(ParallelismLevel, Int)] =  {
    p match {
      case mG@MapGlobal(dim) => visitAndGatherInformation(mG.f, (Global, dim) :: parallInfo)

      case mWG@MapWorkGroup(dim) => visitAndGatherInformation(mWG.f, (WorkGroup, dim) :: parallInfo)

      case mL@MapLocal(dim) => visitAndGatherInformation(mL.f, (Local, dim) :: parallInfo)

      case mS: MapSeq => visitAndGatherInformation(mS.f, (Sequential, -1) :: parallInfo)

      case t: Record => {
        val fstInfo = visitAndGatherInformation(t.fst, List.empty)
        //TODO For now assume that both sides return equal information...
        //val sndInfo = visitAndGatherInformation(t.snd, List.empty)
        fstInfo ++ parallInfo
      }

      case Lambda(_, p) => visitAndGatherInformation(p, parallInfo)

      case _: Identifier[_] | _: Literal | _: Natural => parallInfo

      case pattern => throw new Exception(s"this should not happen for now: $pattern")
    }
  }
}
