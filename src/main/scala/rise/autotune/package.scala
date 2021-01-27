package rise

import rise.core.Expr
import rise.core.types.NatIdentifier
import util.{Time, TimeSpan}

package object autotune {
  type Parameters = Seq[NatIdentifier]
  case class Sample(p: Parameters, runtime: Option[TimeSpan[Time.ms]], timestamp: Int)

  def search(e: Expr): Seq[Sample] = ???

  def collectParameters(e: Expr): Parameters = ???

  private def generateJSON(p: Parameters): String = ???

  def applyBest(e: Expr, samples: Seq[Sample]): Expr = ???
}
