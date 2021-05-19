package parser.ErrorMessage

import parser._
import parser.parse.SpanPlaceholder
import rise.core.types.{Kind, Type}
import rise.core.{types => rt}

import java.nio.file.Paths
//__________________________________________AnsiColors
abstract sealed class AnsiColor_enum()
final case class BLACK() extends AnsiColor_enum{
  override def toString: String = "\u001B[30m"
}
final case class RED() extends AnsiColor_enum{
  override def toString: String = "\u001B[31m"
}
final case class GREEN() extends AnsiColor_enum{
  override def toString: String = "\u001B[32m"
}
final case class YELLOW() extends AnsiColor_enum{
  override def toString: String = "\u001B[33m"
}
final case class BLUE() extends AnsiColor_enum{
  override def toString: String = "\u001B[34m"
}
final case class MAGENTA() extends AnsiColor_enum{
  override def toString: String = "\u001B[35m"
}
final case class CYAN() extends AnsiColor_enum{
  override def toString: String = "\u001B[36m"
}
final case class WHITE() extends AnsiColor_enum{
  override def toString: String = "\u001B[37m"
}
final case class BLACK_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[40m"
}
final case class RED_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[41m"
}
final case class GREEN_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[42m"
}
final case class YELLOW_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[43m"
}
final case class BLUE_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[44m"
}
final case class MAGENTA_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[45m"
}
final case class CYAN_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[46m"
}
final case class WHITE_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[47m"
}
final case class RESET() extends AnsiColor_enum{
  override def toString: String = "\u001B[0m"
}
final case class BOLD() extends AnsiColor_enum{
  override def toString: String = "\u001B[1m"
}
final case class UNDERLINED() extends AnsiColor_enum{
  override def toString: String = "\u001B[4m"
}
final case class BLINK() extends AnsiColor_enum{
  override def toString: String = "\u001B[5m"
}
final case class REVERSED() extends AnsiColor_enum{
  override def toString: String = "\u001B[7m"
}
final case class INVISIBLE() extends AnsiColor_enum{
  override def toString: String = "\u001B[8m"
}
//__________________________________________Object ErrorMessage with useful methods
object ErrorMessage {
  def give_error(fileName:String, description_error:String, what_exp:String,help:Option[String],
                 name_of_error:String,start_column:Int, end_column:Int,
                 codeLines:Array[String], underl: Underline, important_column:Int,
                 important_row_Begin:Int, important_row_End:Int,
                 indentationBefore: Int=5, indentationAfter: Int=1,
                 indentColour: AnsiColor_enum=CYAN(), indentColour_ImportantColumn: AnsiColor_enum=BLUE(),
                ): String ={
    val c = important_column+1
    val r = important_row_Begin+1
    val filePath = Paths.get(fileName+":"+c+":"+r).toUri
    //println("File where Error: "+ filePath+" : "+ important_column + " : "+ important_row_End)
    val res = indentColour_ImportantColumn.toString + name_of_error + Console.RESET+": "+
      description_error+" `"+codeLines(important_column).substring(important_row_Begin,important_row_End)+"`"+"\n"+
      give_char_n_times(important_column.toString.length, ' ', None)+
      indentColour_ImportantColumn.toString+"-->"+Console.RESET+" "+filePath+"\n"

    val help_message = help match {
      case Some(h) => giveIndent(None, BLUE(), indentationBefore,indentationAfter) + "help: " + h +"\n"
      case None => ""
    }
    res + give_code(what_exp,name_of_error,start_column,end_column,
      codeLines,underl,important_column,important_row_Begin,important_row_End,
      indentationBefore,indentationAfter,indentColour,indentColour_ImportantColumn
    ) + help_message
  }

  //Todo:if important column: BLUE, else CYAN as a colour for indentation
  private def give_code(what_exp:String, name_error:String,start_column:Int, end_column:Int,
                        codeLines:Array[String], underl: Underline, important_column:Int,
                        important_row_Begin:Int, important_row_End:Int,
                        indentationBefore: Int, indentationAfter: Int,
                        indentColour: AnsiColor_enum, indentColour_ImportantColumn: AnsiColor_enum): String ={
    val box_indent = name_error.length+": ".length-(indentationBefore+1)
    val outline = giveIndent(None,indentColour, indentationBefore, 0) +
      give_char_n_times(box_indent, '-', Some(indentColour))+"\n"

    var res =""
    for(i <- start_column until end_column+1){
      val (uL,iC) =if(i==important_column){
        (Some(underl),indentColour_ImportantColumn)
      }else{
        (None, indentColour)
      }
      res+=give_line_of_code(what_exp, codeLines, i, uL, important_row_Begin, important_row_End,
        indentationBefore, indentationAfter,iC)
    }
    outline+res+outline
  }

  //alternative we can underline with 'Console.UNDERLINED', which looks great too, but with this method
  //we have more freedom how it looks like and we can use it later for more variants of underlining
  abstract sealed class Underline()
  final case class Underline_With_Char(char:Char, colour: AnsiColor_enum) extends Underline

  private def giveIndent(col:Option[Int], colour: AnsiColor_enum,
                         indentationBefore:Int, indentationAfter:Int,
                         charToSeperate:Char='|', charToIndent:Char=' '):String={
    col match {
      case Some(column) => {
        val real_indentationBefore = indentationBefore-column.toString.size
        if(real_indentationBefore<=0) throw new IllegalArgumentException("ident was chosen too small")
        colour.toString + column + give_char_n_times(real_indentationBefore, charToIndent, None) +
          charToSeperate + give_char_n_times(indentationAfter, charToIndent,None) + Console.RESET
      }
      case None =>colour.toString + give_char_n_times(indentationBefore, charToIndent,None) +
        charToSeperate + give_char_n_times(indentationAfter, charToIndent,None) + Console.RESET
    }
  }

  /*
  give the complete column back
  importantRange is important to know what to higlight
  colour defines with which colour it should be highlighted
   */
  private def give_line_of_code(what_exp:String,
                                codeLines:Array[String], column:Int, underl: Option[Underline],
                                important_row_Begin:Int, important_row_End:Int,
                                indentationBefore: Int, indentationAfter: Int, indentColour: AnsiColor_enum): String={
    val row_begin = 0
    val row_end = codeLines(column).length
    val viewed = Range(Location(column, row_begin),Location(column, row_end))
    val important = Range(Location(column, important_row_Begin),Location(column, important_row_End))
    val which_case= viewed.whoIsSubset(important)

    val res = giveIndent(Some(column+1),indentColour, indentationBefore, indentationAfter)
    val res_of_code_underlining = underl match {
      case Some(Underline_With_Char(char, colour)) => {
        val (underlined_code, pos_start_underline,length_to_underline) = which_case match {
          case This_isSubset() =>
            throw new IllegalStateException("this should not happen, because the whole line is always a overset to an part of the line")
          case Other_isSubset() | Both_Equal()=>
            //println("other_IsSubset:'"+res+"'")
            val code = codeLines(column)
            (code.substring(0,important_row_Begin)+ colour+code.substring(important_row_Begin,important_row_End)+
              Console.RESET+code.substring(important_row_End), important_row_Begin,
              code.substring(important_row_Begin,important_row_End).length)
          case None_isSubset() =>
            (colour.toString + codeLines(column) + Console.RESET, 0, codeLines(column).length)
        }
        val beginNextLineWithoutNumber = giveIndent(None,indentColour, indentationBefore, indentationAfter)
        underlined_code + "\n" + beginNextLineWithoutNumber +
          give_char_n_times(pos_start_underline, ' ',None) +
          colour + give_char_n_times(length_to_underline, char,None)+" " + what_exp + Console.RESET
      }
      case None => codeLines(column)
    }
    res + res_of_code_underlining + "\n"
  }


  def give_char_n_times(n:Int, c:Char, col:Option[AnsiColor_enum]): String={
    val arr = Array.fill(n)(c)
    val res = String.valueOf(arr)
    col match {
      case Some(colour) => colour.toString + res + Console.RESET
      case None => res
    }
  }
}

