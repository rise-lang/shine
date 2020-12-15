package parser

import parser.parse.MapFkt

object parseFile {

  def apply(fileName: String, testFilePath: String = ""): MapFkt = {
    parseFile(fileName, testFilePath)
  }

  def parseFile(fileName: String, testFilePath: String): MapFkt = {
    val fName: String = testFilePath + fileName
    val file: FileReader = new FileReader(fName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)
    riseExprByIdent
  }
}
