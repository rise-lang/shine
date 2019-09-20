package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.FunctionalPrimitives.{Map, MapSeq, Record}
import idealised.DPIA.ImperativePrimitives.MapAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.AddressSpace.Private
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.OpenCL.DSL.`new`
import idealised.OpenCL.ImperativePrimitives.{IdxDistributeAcc, IdxDistribute}
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
    val instAdjAcc = adjustedAcceptor(parallInfo, adjDataType, dt) _
    val instAdjExpr = adjustedExpr(parallInfo, adjDataType, dt) _

    println(s"The old data type: $dt : $addrSpace : $input")
    println(s"The new data type: $adjDataType")

    `new`(addrSpace)(adjDataType, tmp => acc(input)(instAdjAcc(tmp.wr)) `;` C(instAdjExpr(tmp.rd)) )
  }

  private def adjustedAcceptor(parallInfo: List[(ParallelismLevel, Int)], adjDt: DataType,
                               oldDt: DataType)(A: Phrase[AccType]): Phrase[AccType] = {
    val (parallLevel, dim) = parallInfo.head
    val stride: Nat =
      parallLevel match {
        case Global => get_global_size(dim)
        case WorkGroup => get_num_groups(dim)
        case Local => get_local_size(dim)
        case Sequential => 1
      }

    (adjDt, oldDt) match {
      case (ArrayType(adjSize, adjElemT@ArrayType(adjSizeIn, adjElemInT)),
            ArrayType(oldSize, _@ArrayType(oldSizeIn, _))) =>

        val outDim = IdxDistributeAcc(adjSize, oldSize, stride, parallLevel, adjElemT, A)

        MapAcc(oldSize, adjElemT, ArrayType(oldSizeIn, adjElemInT),
          fun(acc"[$adjElemT]")(arr => IdxDistributeAcc(adjSizeIn, oldSizeIn, stride, parallLevel, adjElemInT, arr)),
            outDim)
      case _ => throw new Exception("surprise")
    }
  }

  private def adjustedExpr(parallInfo: List[(ParallelismLevel, Int)], adjDt: DataType,
                           oldDt: DataType)(E: Phrase[ExpType]): Phrase[ExpType] = {

    val (parallLevel, dim) = parallInfo.head
    val stride: Nat =
      parallLevel match {
        case Global => get_global_size(dim)
        case WorkGroup => get_num_groups(dim)
        case Local => get_local_size(dim)
        case Sequential => 1
      }

    (adjDt, oldDt) match {
      case (ArrayType(adjSize, adjElemT@ArrayType(adjSizeIn, adjElemInT)),
            ArrayType(oldSize, _@ArrayType(oldSizeIn, _))) =>

        Map(oldSize, adjElemT, ArrayType(oldSizeIn, adjElemInT),
          fun(exp"[$adjElemT, $read]")(arr => IdxDistribute(adjSizeIn, oldSizeIn, stride, parallLevel, adjElemInT, arr)),
            IdxDistribute(adjSize, oldSize, stride, parallLevel, adjElemT, E))
      case _ => throw new Exception("surprise")
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
            throw new Exception("This should probably not happen") //TODO think about this case

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
