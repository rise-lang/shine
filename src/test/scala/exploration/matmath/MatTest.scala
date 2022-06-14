package exploration.matmath

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class MatTest extends AnyFlatSpec with Matchers with TryValues {
  "Construction of a 0 x n Matrix" should "fail" in {
    Try(Mat()).failure.exception shouldBe a [IllegalArgumentException]
  }

  "Construction of a n x 0 Matrix" should "fail" in {
    Try(Mat(Seq())).failure.exception shouldBe a [IllegalArgumentException]
  }
}
