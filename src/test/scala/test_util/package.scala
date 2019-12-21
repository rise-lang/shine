import com.github.ghik.silencer.silent
import org.scalatest.{FunSuite, Matchers}

package object test_util {
    @silent("define classes/objects inside of package objects")
    abstract class Tests extends FunSuite with Matchers
}