package util

import scala.language.postfixOps
import scala.sys.process._

object Execute2 {
  case class Exception(msg: String) extends Throwable
  var globalBest:Option[Double] = None

  //noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def apply(code: String, iterations: Int, threshold: Double): String = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      (s"clang -O2 $src -o $bin -lm" !!)

      //repeat execution
      //take median as runtime
      val N = iterations
      val runtimes:Array[Double] = new Array[Double](N)
      var runtime = 0.0
      //check global execution time. Discard any with factor 10
      var i = 0
      while(i < N) {
        runtimes(i) = (s"$bin" !!).toDouble

        println("runtime:(" + i +"): " + runtimes(i))
        println("globalBest: " + globalBest)
        // check if we have to skip this execution round
        globalBest match{
          case Some(value) => {
            runtimes(i) > value * threshold match {
              case true => {
                //break up
                for( j <- Range(i, N)){
                  runtimes(j) = runtimes(i)
                }
                i = N
              }
              case false => // continue
            }
          }
          case _ => globalBest = Some(runtimes(i))
        }
        i = i + 1
      }

      // get runtime (median of iterations)
      runtime = runtimes.sorted.apply(N/2)

      // check if new global best was found
      runtime < globalBest.get match {
        case true => globalBest = Some(runtime)
        case false =>
      }

      runtime.toString
    } catch {
      case e: Throwable =>
        Console.err.println("==========")
        Console.err.println(s"execution failed ($e) for code:")
        Console.err.println("enable printing of code in Execute2")
//        Console.err.println(code)
        Console.err.println("==========")
        throw Exception(s"execution failed ($e) for: `$code'")
    }
  }

}


