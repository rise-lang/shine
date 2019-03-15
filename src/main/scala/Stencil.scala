
import idealised.DPIA.Phrases.{PrettyPhrasePrinter, xmlPrinter}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised._
import lift.arithmetic._

import scala.language.{implicitConversions, postfixOps}

object Stencil extends App {

  val epsilon = 1.0f

  val check = true
  val N = SizeVar("N")
  val xsT = ArrayType(N, float)

  val mult = fun(x => x._1 * x._2)
  val add = fun((x, a) => x + a)

  val high_level = fun(xsT)(xs =>
    xs :>> slide(3, 1) :>> mapSeq(fun(nbh =>
      nbh :>> reduceSeq(fun(x => fun(a => x + a)), 0.0f)
    ))
  )

  // OpenMP specific stuff

  {
    println(s"-- high level --")
    val phrase = TypeInference(high_level, Map()).convertToPhrase
    println(PrettyPhrasePrinter(phrase))
    println(xmlPrinter.asString(phrase))
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

//  {
//    import idealised.OpenMP.SurfaceLanguage.DSL._
//
//    val dotCPUVector1 = fun(xsT)(xs => fun(ysT)(ys =>
//      asScalar() o join() o mapPar(
//        mapSeq(
//          reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
//        ) o split(2048)
//      ) o split(2048 * 64) $ zip(asVector(4) $ xs, asVector(4) $ ys)
//    ))
//    val phrase = TypeInference(dotCPUVector1, Map()).toPhrase
//    val program = OpenMP.ProgramGenerator.makeCode(phrase)
//    println(program.code)
//  }

}

