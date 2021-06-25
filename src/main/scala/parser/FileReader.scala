package parser

import java.io.{File, FileInputStream, FileNotFoundException}
import java.net.URI
import java.nio.file.Paths
import scala.collection.mutable

/*
reads the File and saves the name and the content of the file as an Array of Strings
 */
case class FileReader(fileName: String) {
  require(fileName != null, "FileName should not be null")
  require(fileName.endsWith(".rise"), "not a RISE file") //if not, it's not a RISE file

  val sourceLines_withoutPreLexer = readFile(fileName)
  val sourceLines: Array[String] = preLexer(preprocessor(sourceLines_withoutPreLexer))

  def toUri():URI=Paths.get(fileName).toUri

  def ==(other:FileReader):Boolean=this.fileName==other.fileName

  private def preLexer(array: Array[String]): Array[String] ={
    val withoutComments = deleteSimpleComments(array)
    withoutComments
  }
  private def preprocessor(array: Array[String]): Array[String] ={
    cutAndPasteConstants(array)
  }
  /*
  Constants are always defined only in one line
   */
  private def cutAndPasteConstants(array: Array[String]): Array[String] ={
    type MapConstants = mutable.HashMap[String, String]
    val constants:MapConstants = new MapConstants
    val arr:Array[String] = Array.fill(array.length)("")
    for(i<- 0 until array.length){
      if (array(i).contains(":=")) {
        val pos = array(i).indexOf(":=")
        val name = array(i).substring(0,pos).trim
        val content = if(array(i).contains("--")){
          array(i).substring(pos+2, array(i).indexOf("--")).strip()
        }else{
          array(i).substring(pos+2).strip()
        }
        constants.get(name) match {
          case Some(value) => throw new IllegalStateException("The Constant '"
            +name+"' is already defined with value '"+value+
            "', we tried to overwrite it with '"+content+"'")
          case None =>constants.update(name, content)
        }
      }else{
        var updateLine = array(i)
        for((n,c)<-constants){
          while(updateLine.contains(n)){
            val posBegin = updateLine.indexOf(n)
            val posEnd = posBegin+n.length
            updateLine = updateLine.substring(0,posBegin)+ "("+ c + ")"+ updateLine.substring(posEnd)
          }
        }
        arr(i)=updateLine
      }
    }
    arr
  }
  private def deleteSimpleComments(array: Array[String]): Array[String] ={
    val arr:Array[String] = Array.fill(array.length)("")
    for(i<- 0 until array.length){
      if (array(i).contains("--")) {
       val pos = array(i).indexOf("--")
       arr(i) = array(i).substring(0, pos)
      }else{
        arr(i)=array(i)
      }
    }
    arr
  }
  /*
  returns the Content of the File with name fileName in
  an String-Array

  requirement: FileName is not null //TODO: more stability
   */
  private def readFile(fileName: String): Array[String] ={
    //create File
    val f:File = try {
      new File(fileName)
    }catch {
      case ex: NullPointerException => throw readFileExNullPointerException(fileName,ex) //Error File //Todo: Nullpointer entfernen
    }
    //requirements of the File to be read
    if(!f.exists()){
      //file does not exist
      val s: String= "The File " + fileName + " does not exist!"
      throw new IllegalArgumentException(s)
    }
    if(!f.isFile){
      if(f.isDirectory){
        val s: String= "The File " + fileName + " is a directory and not a File!"
        throw new IllegalArgumentException(s)
      }else{
        //file is not a File and not a directory //Todo: is this a possible case? <- It should't be possible
        val s: String= "The File " + fileName + " is neither a File nor a directory"
        throw new IllegalArgumentException(s)
      }
    }
    if(!f.canRead){
      //file can not be read
      val s: String= "The File " + fileName + " is not readable!"
      throw new IllegalArgumentException(s)
    }
    val input:FileInputStream = try{ //Todo: Schau dir die Try-Klasse von Scala an, weil so kännten unerwartete Fehler auftreten und dann schließen wir ihn nicht
      //read the File with the Java-InputStream
      new FileInputStream(f)
    }catch {
            //both errors should not be able to appear
      case ex: FileNotFoundException => throw readFileExFileNotFound(fileName, ex) //Error FileInputStream
      case ex: SecurityException => throw readFileExSecurityException(fileName, ex) //Error FileInputStream
    }/* finally {
      val b:Boolean = f.delete() //deletes the File (not only the scala object but the whole File)
      if(!b){
        //File is not deleted
        val s: String= "\n\n!!!The File " + fileName + " is not deleted!!!\n\n"
        println(s)
      }
    } */

    val arr:Array[String] = try {
      //Todo:Try with resources
      val arr:Array[String] = scala.io.Source.fromInputStream(input).getLines().toArray
      arr
    }finally{
        input.close //close the InputStream (very important) //throw the IOException if occours
    }
    arr //return the Content of the File
  }

  /*
  returns a List of Files which have similar names
   */
  /*private def isSimilarFile(fileName: String):Array[File]={
    //in the fileName we extract, if contains, the path in which it should be
    val pathName:String = if(fileName.contains('/')){
      fileName.substring(0,fileName.lastIndexWhere(p => p =='/'))
    }else{
      "."
    }
    //Todo: check if path exists
    //takes all Files in the same path
    val filesHere:Array[File] = listAllFilesInDirectory(pathName)
    //returns all Files which FileNames are less than 5 chars different than fileName //https://alvinalexander.com/scala/scala-strings-differences-intersection-distinct-characters/
    filesHere.filter(p => p.getName.toSeq.diff(fileName.toSeq).length<5)
  }*/
  /*
  list all Files in the Directory
   */
/*  private def listAllFilesInDirectory(pathname: String = "."):Array[File]={
    val filesHere:Array[File] = (new java.io.File(pathname)).listFiles()
    filesHere.filter(file => file.isFile && file.getName().endsWith(".rise"))
  }*/

  // if the file does not exist, is a directory rather than a regular file, or for some other reason cannot be opened for reading.
  private def readFileExFileNotFound(fileName: String, exception: Exception):Exception ={
    exception
  }
  // if a security manager exists and its checkRead method denies read access to the file.
  private def readFileExSecurityException(fileName: String, exception: Exception):Exception ={
    exception
  }
  //If the pathname argument is null
  private def readFileExNullPointerException(fileName: String, exception: Exception):Exception ={
    exception
  }


  override def toString = "fileName: '" + fileName + "'; fileContent: {\n"+ sourceLines.mkString + "\n}"

}
