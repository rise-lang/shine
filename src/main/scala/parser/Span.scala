package parser

abstract sealed class SubsetCase()
final case class This_isSubset() extends SubsetCase
final case class Other_isSubset() extends SubsetCase
final case class None_isSubset() extends SubsetCase
final case class Both_Equal() extends SubsetCase

final case class Range(begin: Location, end: Location){
  // TODO make more solid (begin is safe to be smaller than end etc)
  if(end.column == begin.column){
    require(end.row >= begin.row, "they have the same column/line and end.row is before begin.row")
  }
  require(end.column >= begin.column, "end.column is before begin.column")

  def isAfter(other:Range):Boolean={
    if(other.end.column<this.begin.column) return true
    if(other.end.column>this.begin.column) return false

    if(other.end.row<=this.begin.row) return true
    if(other.end.row>this.begin.row) return false

    throw new IllegalStateException("The Function isAfter in Span should never reach this point")
  }

  def isSubsetOf(other:Range):Boolean={
    if(this.begin.column>=other.begin.column){
      if(this.end.column<other.end.column){
        true
      }else{
        if(this.end.row<=other.end.row&&this.begin.row>=other.begin.row){
          true
        }else{
          false
        }
      }
    }else{
      false
    }
  }

  def whoIsSubset(other:Range):SubsetCase={
    if(this.isSubsetOf(other)){
      if(this==other){
        Both_Equal()
      }else{
        This_isSubset()
      }
    }else if(other.isSubsetOf(this)){
      Other_isSubset()
    }else{
      None_isSubset()
    }
  }

  def +(other:Range):Range={
    if(other.isAfter(this)){
      println("after:"+  "this.begin: "+ this.begin +"; this.end : " + this.end  + ";other.begin: " + other.begin+ ";other.end: " + other.end)
      Range(this.begin, other.end)
    }else{
      println("before:"+  "this.begin: "+ this.begin +"; this.end : " + this.end  + ";other.begin: " + other.begin+ ";other.end: " + other.end)
      Range(other.begin, this.end)
    }
  }

  def ==(other: Range): Boolean = other.begin == this.begin && other.end == this.end


  override def toString = if(begin.column==end.column){
    if(begin.row==end.row){
      end.toString
    }else{
      "("+begin.c+"," + begin.r+"-"+end.r+")"
    }
  }else{
    begin.toString+ "-"+end.toString
  }
}
/*
  is the span of the specified Token
  the exact span is needed to point to the faulty position if the parser isn't able to continue
 */
// TODO BufferedSource instead of String
final case class Location (column: Int, row: Int){
  //row and line positive numbers
  require(row >= 0, "row is negative")
  require(column >= 0, "column is negative")
  val c = column+1
  val r = row+1

  override def toString: String = s"($c,$r)"
  def ==(end:Location) = this.column == end.column && this.row == end.row
}

final case class Span(file: FileReader, range:Range) {
  def this(file: FileReader, loc: Location) = this(file, Range(loc, loc))
  //def this(file: FileReader, begin: Location, end: Location) = this(file, Range(begin, end))
  override def toString = range.toString + Console.BLUE+ "->" +Console.RESET+ file.toUri()

  def ==(other: Span): Boolean = this.range==other.range &&other.file==this.file

  def whereIs(searched:String):Option[Range]={
    for(i <- range.begin.column until  range.end.column+1){
      if(file.sourceLines(i) contains searched){
        val rowB = file.sourceLines(i) indexOf searched
        val rowE = rowB+searched.size
        return Some(Range(Location(i,rowB),Location(i,rowE)))
      }
    }
    None
  }

  def isAfter(other:Span):Boolean={
    if(this.file==other.file){
      this.range.isAfter(other.range)
    }else{
      throw new IllegalArgumentException("not in the same file this: "+ this.file.toUri() +
        " ,other: "+file.toUri())
    }
  }

  def isSubsetOf(other:Span):Boolean={
    if(this.file==other.file){
      this.range.isSubsetOf(other.range)
    }else{
      throw new IllegalArgumentException("not in the same file this: "+ this.file.toUri() +
        " ,other: "+file.toUri())
    }
  }

  def whoIsSubset(other:Span):SubsetCase={
    if(this.file==other.file){
      this.range.whoIsSubset(other.range)
    }else{
      throw new IllegalArgumentException("not in the same file this: "+ this.file.toUri() +
        " ,other: "+file.toUri())
    }
  }

  def +(other:Span):Span={
    if(this.file.fileName!=other.file.fileName){
      throw new IllegalArgumentException("You try to add two spans of different files")
    }
    Span(this.file, other.range+this.range)
  }

  def returnMessage():String={
    val begin = this.range.begin
    val end = this.range.end
    val f = this.file
    if(begin.column==end.column){
      return f.sourceLines(begin.column).substring(begin.row, end.row)
    }

    var ret = f.sourceLines(begin.column).substring(begin.row)
    for(i <- (begin.column+1) until (end.column)){
      ret+= this.file.sourceLines(i)
    }
    ret += f.sourceLines(end.column).substring(0, end.row)
    ret
  }
}
object Span {
  import parser.parse._
  def getSpanSynElem(synElem:SyntaxElement):Option[Span]={
    synElem match {
      case SExprClutched(_, spanClutch) => Some(spanClutch)
      case SLet(span) => Some(span)
      case SAddrSpace(_, span)=> Some(span)
      case SType(_, span)=> Some(span)
      case SExpr(expr) => expr.span
      case SNat(_, span) => Some(span)
      case SData(_, span) => Some(span)
      case SIntToExpr(_, span)=> Some(span)
      case SSeq(_, span) => Some(span)
    }
  }
  def combineOptionSpan(sp1:Option[Span],sp2:Option[Span]): Option[Span] ={
    sp1 match{
      case None => None
      case Some(span1)=>sp2 match {
        case None =>None
        case Some(span2)=>Some(span1+span2)
      }
    }
  }
  def getSpanOfFirstElemInSeq(seq: Seq[rise.core.types.Constraint]): Option[parser.Span]= seq match {
    case Seq(first, tail @ _*) =>first.constraintTypeError.span
    case Seq() => None
  }

  def getSpanListOfSeq(seq: Seq[rise.core.types.Constraint]): List[parser.Span]= seq match {
    case Seq(first, tail @ _*) =>first.constraintTypeError.span.get :: getSpanListOfSeq(tail)
    case Seq() => Nil
  }
}