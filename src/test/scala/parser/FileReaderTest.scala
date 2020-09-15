package parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class FileReaderTest extends  AnyFlatSpec{
//    val aRISeFileContent:String = "\\x:Int -> x+1\n\\z:Double -> z  +    20\n\\kurt:FlosourceLines ->\n  \\   s:   Float -> (5+7      )*20+kurt*s"


  "FileReader" should " be able to read the contents of 'src/test/scala/parser/readFiles/aRISEFile.rise'" in {
    val fileName:String = "src/test/scala/parser/readFiles/aRISEFile.rise"
    val fR:FileReader = FileReader(fileName)
    fR.sourceLines(0) should equal("\\x:Int -> x+1")
    fR.sourceLines(1) should equal("\\z:Double -> z  +    20")
    fR.sourceLines(2) should equal("\\kurt:FlosourceLines ->")
    fR.sourceLines(3) should equal("  \\   s:   Float -> (5+7      )*20+kurt*s")
  }

}


