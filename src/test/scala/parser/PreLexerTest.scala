package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class PreLexerTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"

  "preLexer" should "work for the matrixMultWithComments" in {
    val fileName: String = testFilePath + "matrixMultWithComments.rise"
    val file: FileReader = FileReader(fileName)

    val arr = file.sourceLines
    val correctArr = Array(
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",

      "f::N:Nat=>M:Nat=>D:Data=>N.M.D->M.N.D->N.N.D ",
      "f=\\N:Nat=>\\M:Nat=>\\D:Data=>",
      "\\mat1->\\mat2->",
      "mapSeq ",
      "(",
      "\\vec1->",
      "mapSeq ",
      "(",
      "\\vec2 ->",
      "    reduceSeq (\\res:I32->\\arg:I32-> + res arg) ",
      "        (mapSeq (\\x:(I32,I32)->+ (fst x) (snd x)) (zip vec1 vec2))",
      ")",
      "(transpose mat2)",
      ")",
      "mat1 ",
    )

    arr.length should be equals(correctArr.length)
    for(i <- 0 until correctArr.length) {
      if( !arr(i).equals(correctArr(i))){
        fail("'"+arr(i) + "'!='"+correctArr(i)+"'")
      }
    }
  }
}
