package parser.ErrorMessage

object debug {
  var isOn = false
  def apply(str:String, whatToParse:String)={
    isOn match {
      case false=>
      case true => {
        val toPrint = CYAN().toString + str + BOLD().toString + " : " + whatToParse + RESET().toString
        println(toPrint)
      }
    }
  }
}