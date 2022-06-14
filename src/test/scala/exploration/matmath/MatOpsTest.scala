package exploration.matmath

import exploration.matmath.Operators.SimpleMatOps
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatOpsTest extends AnyFlatSpec with Matchers with TryValues {
  "Sliding any Matrix" should "not require arithmetic operations" in {
    Mat(Seq(Seq(""))).slide(1,1)() should equal (Seq(Seq(Mat(Seq(Seq(""))))))
  }

  "Sliding a 3x3 Matrix" should "work with even slide size" in {
    val inputMatrix = Mat(
      Seq(1,2,3),
      Seq(4,5,6),
      Seq(7,8,9)
    )

    val s11 = Mat(
      Seq(1,2),
      Seq(4,5)
    )
    val s12 = Mat(
      Seq(2,3),
      Seq(5,6)
    )
    val s21 = Mat(
      Seq(4,5),
      Seq(7,8)
    )
    val s22 = Mat(
      Seq(5,6),
      Seq(8,9)
    )
    val expectedSlides = Seq(
      Seq(s11,s12),
      Seq(s21,s22)
    )

    inputMatrix.slide(2,2)() should equal (expectedSlides)
  }

  it should "work with odd slide sizes" in {
    val inputMatrix = Mat(
      Seq(1,2,3),
      Seq(4,5,6),
      Seq(7,8,9)
    )

    inputMatrix.slide(3,3)() shouldEqual Seq(Seq(inputMatrix))
  }
}
