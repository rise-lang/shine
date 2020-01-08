import com.github.ghik.silencer.silent
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

package object test_util {
  @silent("define classes/objects inside of package objects")
  abstract class Tests extends AnyFunSuite with Matchers
}
