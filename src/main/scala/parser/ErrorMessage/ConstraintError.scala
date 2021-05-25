package parser.ErrorMessage

import parser.Span
import rise.core.types.{DataType, DepFunType, FunType, Kind, Type, TypeIdentifier, TypePlaceholder}
import rise.core.{DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Primitive, types => rt, primitives=>rp}
import rise.{core => r}

//__________________________________________________________________________________________________________
//ConstraintTypeError
abstract sealed class ConstraintInputTypes()
case class ExpectedAndFoundT(expectedT:rt.Type,foundT:rt.Type) extends ConstraintInputTypes
case class ExpectedAndFoundTLeftAndRight(expectedT:rt.Type,foundTLeft:rt.Type,
                                         foundTRight:rt.Type) extends ConstraintInputTypes
case class ExpectedAndFoundTLeftAndRightDep[K <: Kind](expectedT:rt.Type,foundTLeft:rt.Type,
                                                       foundTRight:K#T) extends ConstraintInputTypes
case class ExpectedAndFoundN(expected:rt.Nat,found:rt.Nat) extends ConstraintInputTypes
case class ExpectedAndFoundB(expected:arithexpr.arithmetic.BoolExpr,found:arithexpr.arithmetic.BoolExpr) extends ConstraintInputTypes
case class ExpectedAndFoundA(expected:rt.AddressSpace,found:rt.AddressSpace) extends ConstraintInputTypes
case class ExpectedAndFoundNatToData(expected:rt.NatToData,found:rt.NatToData) extends ConstraintInputTypes
case class ExpectedAndFoundNatCollection(expected:rt.NatCollection, found:rt.NatCollection) extends ConstraintInputTypes


trait ConstraintError { self: Throwable =>
  val span: Option[Span]
  def description_error():String
  val what_exp:String
  val help:Option[String]
  val name_of_error:String
  val expr:r.Expr

  //Todo: find better solution than SpanPlaceholder
  var constraintTypes: Option[ConstraintInputTypes] = None
  def getTypes():List[rt.Type]
  def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit

  /*
  problem: we have multiple lines where we look on, so we have to define for each Exeption individually
  what is important
   */
  def getImportantPos():Option[(Int,Int,Int)] = throw new IllegalStateException("this has to be overwritten")
  /*
  if None returned then failure
   */
  def getPos():Option[(Int,Int,Int,Int,Int)]={
    var (start_column, end_column, important_column,
    important_row_Begin, important_row_End):(Option[Int],
      Option[Int], Option[Int],Option[Int],Option[Int])=(None,None,None,None,None)

    val sp = span match {
      case Some(value) => value
      case None => return None
    }
    val (begin,end)=(sp.range.begin,sp.range.end)
    start_column = Some(begin.column)
    end_column = Some(end.column)

    if(begin.column==end.column){
      important_column = Some(begin.column)
      important_row_Begin = Some(begin.row)
      important_row_End = Some(end.row)
    }else{
      val (iC, iRB, iRE) =getImportantPos().get
      important_column=Some(iC)
      important_row_Begin=Some(iRB)
      important_row_End=Some(iRE)
    }


    Some((start_column.get,end_column.get,important_column.get,important_row_Begin.get,important_row_End.get))
  }

  override def toString: String = {
    val (start_column, end_column, important_column,
    important_row_Begin, important_row_End)= getPos() match {
      case Some(value) => value
      case None => ???
    }
    val sp = span match {
      case Some(value) => value
      case None => ???
    }

    val underl = ErrorMessage.Underline_With_Char('^', RED())

    val (newWhatExpr, newHelp) = getWhatExprAndOption
    val error = ErrorMessage.give_error(sp.file.fileName, description_error(),newWhatExpr,newHelp,name_of_error,
      start_column,end_column,sp.file.sourceLines, underl, important_column, important_row_Begin,
      important_row_End)
    error
  }

  def getWhatExprAndOption: (String,Option[String]) ={
    val help = expr match {
      case Identifier(name) => Some("Identifier '"+name+"' has probably a different type <-"+ expr.span.get)
      case Lambda(x, e) => Some("Lambda('"+x+"'): "+e)
      case r.App(r.App(rp.add(sp), x1),x2)=> if(x1.t!=x2.t){
        Some(x1.t.toString+"!="+x2.t.toString+" but Add needs that both arguments are the same type")
      }else{
        Some("Add is used but they seem to have the same type")
      }
      case r.App(f, e) => Some("App('"+f+"'): "+e)
      case DepLambda(x, e) => Some("DepLambda('"+x+"'): "+e)
      case DepApp(f, x) => Some("DepApp('"+f+"'): "+x)
      case Literal(d, span) => Some("Literal('"+d+"')")
      case primitive: Primitive => Some("Primitive: "+primitive)
    }
    constraintTypes match {
      case Some(cT) => cT match {
        case ExpectedAndFoundT(expectedT, foundT) =>{
          val what_exp = "here `"+foundT+"` but expected `"+expectedT+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundTLeftAndRight(expectedT, foundTLeft, foundTRight) =>{
          val what_exp = "here (`"+foundTLeft+"`,`"+foundTRight+"`) but expected `"+expectedT+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundTLeftAndRightDep(expectedT, foundTLeft, foundTRight) =>{
          val what_exp = "here (`"+foundTLeft+"`,`"+foundTRight+"`) but expected `"+expectedT+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundN(expected, found) =>{
          val what_exp = "here `"+found+"` but expected `"+expected+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundB(expected, found) =>{
          val what_exp = "here `"+found+"` but expected `"+expected+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundA(expected, found) =>{
          val what_exp = "here `"+found+"` but expected `"+expected+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundNatToData(expected, found) =>{
          val what_exp = "here `"+found+"` but expected `"+expected+"`"
          (what_exp,help)
        }
        case ExpectedAndFoundNatCollection(expected, found) =>{
          val what_exp = "here `"+found+"` but expected `"+expected+"`"
          (what_exp,help)
        }
      }
      case None =>throw new IllegalStateException("ConstraintTypes are not initialised yet")
    }
  }
}

abstract sealed class AppLikeConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override def getTypes():List[rt.Type] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundTLeftAndRight(expectedT, foundTLeft, foundTRight) => expectedT :: foundTLeft :: foundTRight::Nil
      case a => throw new IllegalStateException("wrong ConstraintType: "+ a)
    }
  }

  override def defineTypes(expectedT: Type, foundT: Type): Unit = throw new IllegalStateException("not call this function")
  def defineTypes(expectedT:rt.Type, foundTLeft:rt.Type, foundTRight:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundTLeftAndRight(expectedT, foundTLeft, foundTRight))
    }
  }
}

abstract sealed class TypeConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
  override def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundT(expectedT, foundT))
    }
  }
  override def getTypes():List[rt.Type] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundT(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }

  override def description_error(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }

  override val name_of_error: String = "TypeConstraintError"
}

case class EqualityTypeConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with TypeConstraint"
  override val help: Option[String] = None

  override def getImportantPos(): Option[(Int, Int, Int)] = {
    span match {
      case Some(s) => {
        Some((s.range.begin.column, s.range.begin.row, s.file.sourceLines(s.range.begin.column).length))
      }
      case None => ???
    }
  }
}

case class IdentConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with IdentConstraint"
  override val help: Option[String] = None
}

case class LambdaConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with LambdaConstraint"
  override val help: Option[String] = None
  override def getImportantPos(): Option[(Int, Int, Int)] = {
    span match {
      case Some(s) => {
        val r = s.whereIs("->").get
        val c = r.end.column
        if(c==s.range.begin.column){
          Some(c,s.range.begin.row,r.end.row)
        }else{
          Some(c,0,r.end.row)
        }
      }
      case None => ???
    }
  }
}

//Todo: delete AppLikeConstraint
case class AppConstraintError(override val span: Option[Span], override val expr: Expr) extends AppLikeConstraintError(span){
  override def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = {
        val (inTofE, exT) = foundT match {
          case rt.FunType(inT, outT) =>  (inT, outT)
          case _ => throw new IllegalStateException("FunType is expected")
        }
        val inTofF = expectedT
        Some(ExpectedAndFoundTLeftAndRight(exT, inTofF, inTofE))
      }
    }
  }
  override def description_error(): String = {
    val expectedT::foundTLeft::foundTRight::Nil = getTypes()
    "expected '" + expectedT + "' but found App('"+foundTLeft+ "','"+
      foundTRight+"')"
  }
  override val what_exp: String = "here problems with AppConstraint"
  override val help: Option[String] = None

  override def getImportantPos(): Option[(Int, Int, Int)] = span match {
    case Some(s) => {
      Some((s.range.end.column, 0, s.range.end.row))
    }
    case None => ???
  }
}

case class DepLambdaConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with DepLambdaConstraint"
  override val help: Option[String] = None
  override def getImportantPos(): Option[(Int, Int, Int)] = {
    span match {
      case Some(s) => {
        val r = s.whereIs("=>").get
        val c = r.end.column
        if(c==s.range.begin.column){
          Some(c,s.range.begin.row,r.end.row)
        }else{
          Some(c,0,r.end.row)
        }
      }
      case None => ???
    }
  }
}

case class DepAppConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def defineTypesDep[K <: Kind](expectedT:rt.Type, foundTLeft:rt.Type, foundRight:K#T): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes =
        Some(ExpectedAndFoundTLeftAndRightDep(expectedT, foundTLeft, foundRight))
    }
  }
  def getTypesDep[K <: Kind]():(rt.Type, rt.Type, K#T) = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT) => cT match {
      case ExpectedAndFoundTLeftAndRightDep(expectedT,foundTLeft, foundTRight) => foundTRight match {
        //case fR:K#T => (expectedT, foundTLeft, fR) //abstract type pattern K#T is unchecked since it is eliminated by erasure
        case fR => (expectedT, foundTLeft, fR.asInstanceOf[K#T])
      }
      case a => throw new IllegalStateException("wrong ConstraintType: "+ a)
    }
  }
  override def description_error(): String = {
    val (expectedT,foundLeft,foundRight) = getTypesDep()
    val foundRightStr: String = foundRight.toString
    "expected '" + expectedT + "' but found DepApp('"+foundLeft+ "','"+
      foundRightStr+"')"
  }
  override val name_of_error: String = "DepAppConstraintError"
  override val what_exp: String = "here problems with DepAppConstraint"
  override val help: Option[String] = None

  override def getImportantPos(): Option[(Int, Int, Int)] = span match {
    case Some(s) => {
      Some((s.range.end.column, 0, s.range.end.row))
    }
    case None => ???
  }
}

case class TypeAnnotationConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override def description_error(): String = {
    val annotatedT::foundT::Nil = getTypes()
    "annotated '" + annotatedT + "' but found '"+foundT+"'"
  }
  override val what_exp: String = "here problems with TypeAnnotationConstraint"
  override val help: Option[String] = None

  override def getImportantPos(): Option[(Int, Int, Int)] = span match {
    case Some(s) => {
      Some((s.range.end.column, 0, s.range.end.row))
    }
    case None => ???
  }
}

case class TypeAssertionConstraintError(override val span: Option[Span], override val expr: Expr) extends TypeConstraintError(span){
  override def description_error(): String = {
    val annotatedT::freezeAnnT::Nil = getTypes()
    "annotated '" + annotatedT +"' but found freeze Type '"+freezeAnnT+"'"
  }
  override val what_exp: String = "here problems with TypeAssertionConstraint"
  override val help: Option[String] = None

  override def getImportantPos(): Option[(Int, Int, Int)] = span match {
    case Some(s) => {
      Some((s.range.end.column, 0, s.range.end.row))
    }
    case None => ???
  }
}

case class NatConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  def getNats():List[rt.Nat] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundN(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }

  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def defineNats(expectedT:rt.Nat, foundT:rt.Nat): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundN(expectedT, foundT))
    }
  }
  override def description_error(): String = {
    val expectedT::foundT::Nil = getNats()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }

  override val name_of_error: String = "NatConstraintError"
  override val what_exp: String = "here problems with NatConstraint"
  override val help: Option[String] = None
}

case class BoolConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getBools():List[arithexpr.arithmetic.BoolExpr] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundB(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineBools(expectedT:arithexpr.arithmetic.BoolExpr, foundT:arithexpr.arithmetic.BoolExpr): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundB(expectedT, foundT))
    }
  }
  override def description_error(): String = {
    val expectedT::foundT::Nil = getBools()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
  override val name_of_error: String = "BoolConstraintError"
  override val what_exp: String = "here problems with BoolConstraint"
  override val help: Option[String] = None
}

case class AddrConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getAddrs():List[rt.AddressSpace] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundA(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineAddrs(expected:rt.AddressSpace, found:rt.AddressSpace): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundA(expected, found))
    }
  }
  override def description_error(): String = {
    val expected::found::Nil = getAddrs()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "AddrConstraintError"
  override val what_exp: String = "here problems with AddrConstraint"
  override val help: Option[String] = None
}


case class NatToDataConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getNatToDatas():List[rt.NatToData] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundNatToData(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineNatToDatas(expected:rt.NatToData, found:rt.NatToData): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundNatToData(expected, found))
    }
  }
  override def description_error(): String = {
    val expected::found::Nil = getNatToDatas()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "NatToDataConstraintError"
  override val what_exp: String = "here problems with NatToDataConstraint"
  override val help: Option[String] = None
}

case class NatCollectionConstraintError(override val span: Option[Span], override val expr: Expr) extends Error with ConstraintError{
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getNatCollection():List[rt.NatCollection] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundNatCollection(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineNatCollection(expected:rt.NatCollection, found:rt.NatCollection): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundNatCollection(expected, found))
    }
  }
  override def description_error(): String = {
    val expected::found::Nil = getNatCollection()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "NatCollectionError"
  override val what_exp: String = "here problems with NatCollection"
  override val help: Option[String] = None
}

//case class OpaqueConstraintError(span: Span, foundT:rt.Type, annotatedT:rt.Type, freezeAnnT:rt.Type) extends ConstraintTypeError(span){
//  override def description(): String = "annotated '" + annotatedT + "' but found '"+
//    foundT+"' or found freeze Type '"+freezeAnnT+"'"
//}

