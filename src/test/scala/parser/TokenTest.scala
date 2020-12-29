package parser

import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec
class matchesToken extends  AnyFlatSpec {

  val beginLocation: Location = new Location( 11, 10)
  val endLocation: Location = new Location( 13, 10)
  val fileName:String = "src/test/scala/parser/readFiles/aRISEFile.rise"
  val file:FileReader = new FileReader(fileName)
  val location: Span = new Span(file, beginLocation, endLocation)


  s"location" should " have the correct name " in {
    location.file.fileName should equal (fileName)
  }

  s"location" should " have the correct file " in {
    location.begin.column should equal (11)
  }

  s"location" should " have the correct row " in {
    location.end.row should equal (10)
  }


  "Location" should "not be negative" in {
    val thrown = intercept[Exception] {
      new Location(-1, 2)
    }
    thrown.getMessage should equal ("requirement failed: column is negative")
  }

  "Span" should "not be unlogical" in {
    val b  = new Location(2, 3)
    val e  = new Location(2, 2)
    val thrown = intercept[Exception] {
        new Span(file, b, e)
    }
    thrown.getMessage should equal ("requirement failed: they have the same column/line and end.row is before begin.row")
  }

  "Span" should "not be unlogical2" in {
    val b  = new Location(3, 1)
    val e  = new Location(2, 2)
    val thrown = intercept[Exception] {
      new Span(file, b, e)
    }
    thrown.getMessage should equal ("requirement failed: end.column is before begin.column")
  }

  "Span" should "not be unlogical3" in {
    val b  = new Location(3, 4)
    val e  = new Location(4, 1)
    val s:String = "Span don't throwed an Exception"
    val thrown = intercept[Exception] {
      new Span(file, b, e)
      throw new Exception(s)
    }
    thrown.getMessage should equal (s)
  }

  "FileReader" should "not be unlogical" in {
    val thrown = intercept[Exception] {
      new FileReader("asdf.jkloe")
    }
    thrown.getMessage should equal ("requirement failed: not a RISE file")
  }

  "FileReader" should "not be unlogical2" in {
    val thrown = intercept[Exception] {
      new FileReader("asdf.rise")
    }
    thrown.getMessage should equal ("The File asdf.rise does not exist!")
  }



  "matche operator" should "work general" in {
    val loc = Location(0,0)
    val loc2 = Location(0,1)
    val span = Span(file, loc, loc2)
    val operator = new BinOp(OpType.BinOpType.ADD, span)
    val ret: String = operator match{
      case operator: BinOp => "Ein Operator!"
      case _ => "Was anderes!"
    }
    ret should be ("Ein Operator!")
  }

    "matche operator" should "work +" in {
      val loc = Location(0,0)
      val loc2 = Location(0,1)
      val span = Span(file, loc, loc2)
      val operator: BinOp = new BinOp(OpType.BinOpType.ADD, span)
      val ret: String = operator match{
        case BinOp(OpType.BinOpType.ADD, _) =>  "Ein Operator mit +!"
        case BinOp(OpType.BinOpType.SUB, _) =>  "Ein Operator mit -!"
        case BinOp(OpType.BinOpType.MUL, _) =>  "Ein Operator mit *!"
        case BinOp(OpType.BinOpType.DIV, _) =>  "Ein Operator mit /!"
        case _ => "Was anderes!"
      }
      ret should be ("Ein Operator mit +!")
    }


    "matche operator" should "work -" in {
      val loc = Location(0,0)
      val loc2 = Location(0,1)
      val span = Span(file, loc, loc2)
      val operator = new BinOp(OpType.BinOpType.SUB, span)
      val ret: String = operator match{
        case BinOp(OpType.BinOpType.ADD, _) =>  "Ein Operator mit +!"
        case BinOp(OpType.BinOpType.SUB, _) =>  "Ein Operator mit -!"
        case BinOp(OpType.BinOpType.MUL, _) =>  "Ein Operator mit *!"
        case BinOp(OpType.BinOpType.DIV, _) =>  "Ein Operator mit /!"
        case _ => "Was anderes!"
      }
      ret should be ("Ein Operator mit -!")
    }

    "matche operator" should "work *" in {
      val loc = Location(0,0)
      val loc2 = Location(0,1)
      val span = Span(file, loc, loc2)
      val operator = new BinOp(OpType.BinOpType.MUL, span)
      val ret: String = operator match{
        case BinOp(OpType.BinOpType.ADD, _) =>  "Ein Operator mit +!"
        case BinOp(OpType.BinOpType.SUB, _) =>  "Ein Operator mit -!"
        case BinOp(OpType.BinOpType.MUL, _) =>  "Ein Operator mit *!"
        case BinOp(OpType.BinOpType.DIV, _) =>  "Ein Operator mit /!"
        case _ => "Was anderes!"
      }
      ret should be ("Ein Operator mit *!")
    }

    "matche operator" should "work /" in {
      val loc = Location(0,0)
      val loc2 = Location(0,1)
      val span = Span(file, loc, loc2)
      val operator = new BinOp(OpType.BinOpType.DIV, span)
      val ret: String = operator match{
        case BinOp(OpType.BinOpType.ADD, _) =>  "Ein Operator mit +!"
        case BinOp(OpType.BinOpType.SUB, _) =>  "Ein Operator mit -!"
        case BinOp(OpType.BinOpType.MUL, _) =>  "Ein Operator mit *!"
        case BinOp(OpType.BinOpType.DIV, _) =>  "Ein Operator mit /!"
        case _ => "Was anderes!"
      }
      ret should be ("Ein Operator mit /!")
    }

  "IdentifierType" should "be like <Identifier>::=[<leer>] <Buchstaben>{<Buchstaben>|<Ziffer>| _ }" in {
    val identifier="PeterVogel_123"
    identifier should be ("PeterVogel_123")
  }

}

