package apps

import harrisCornerDetection._
import util._

class harrisCornerDetectionCheck extends shine.test_util.TestsWithExecutor {
  private val H = 20
  private val W = 80
  private val kappa = 1.2f
  // private val threshold = 1.4f

  test("harris produces expected result") {
    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat)
    val gold = computeGold(H, W, input, kappa).flatten
    val runs = Seq(
      "no pipe" -> NoPipe.create.run(input, kappa),
      "half pipe 2" -> HalfPipe2.create.run(input, kappa),
      "half pipe 1" -> HalfPipe1.create.run(input, kappa)
      // TODO: "full pipe" -> FullPipe.create.run(input, kappa)
    )
    runs.foreach(r => {
      var totalT = TimeSpan.inMilliseconds(0.0f)
      r._2._2.foreach { case (n, t) =>
        println(s"$n: $t")
        totalT = totalT + t
      }
      println(s"${r._1}: $totalT")
      assertSame(r._2._1, gold, s"${r._1} output is different from gold")
    })
  }
}
