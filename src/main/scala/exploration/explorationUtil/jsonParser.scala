package exploration.explorationUtil

import elevate.heuristic_search.Heuristic
import elevate.heuristic_search.heuristics.{Annealing, AutotunerSearch, AutotunerSearch2, AutotunerSearch3, Exhaustive, IterativeImprovement, Random, RandomSampling, TabuSearch}
import rise.elevate.Rise

import scala.io._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object jsonParser {

  // classes to parse
  case class ParseExecutor(name: String, iterations: Int, threshold: Double)

  case class ParseMetaheuristic(heuristic: String, depth: Int, samples: Int)

  //  case class ParseExploration(name: String, strategies: String, output:String, inputSize:Int, metaheuristic: Seq[ParseMetaheuristic], executor:ParseExecutor)
  case class ParseExploration(name: String, output: String, inputSize: Int, metaheuristic: Seq[ParseMetaheuristic], executor: ParseExecutor)

  // implicit readings
  implicit val executorRead: Reads[ParseExecutor] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "iterations").read[Int] and
      (JsPath \ "threshold").read[Double]
    ) (ParseExecutor.apply _)

  implicit val metaheuristicReads: Reads[ParseMetaheuristic] = (
    (JsPath \ "heuristic").read[String] and
      (JsPath \ "depth").read[Int] and
      (JsPath \ "iterations").read[Int]
    ) (ParseMetaheuristic.apply _)

  //      (JsPath \ "strategies").read[String] and
  implicit val explorationReads: Reads[ParseExploration] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "output").read[String] and
      (JsPath \ "inputSize").read[Int] and
      (JsPath \ "metaheuristic").read[Seq[ParseMetaheuristic]] and
      (JsPath \ "executor").read[ParseExecutor]
    ) (ParseExploration.apply _)

  def parse(filePath: String): ParseExploration = {

    // read in json from path to string
    val jsonString = readFile(filePath)

    // parse string to json object
    val json = Json.parse(jsonString)

    // parse elements from object
    val result = json.validate[ParseExploration]

    result.get
  }


  // todo clarify names -> name-conventions
  def getHeuristic(name: String): Heuristic[Rise] = {
    val heuristic = name match {
      case "IterativeImprovement" => new IterativeImprovement[Rise]
      case "Random" => new Random[Rise]
      case "RandomSampling" => new RandomSampling[Rise]
      case "exhaustive" => new Exhaustive[Rise]
      case "annealing" => new Annealing[Rise]
      case "tabuSearch" => new TabuSearch[Rise]
      case "autotuner" => new AutotunerSearch[Rise]
      case "cot" => new AutotunerSearch2[Rise]
      case "cot2" => new AutotunerSearch3[Rise]
      case _ => new Exception("not a supported heuristic option")
    }
    heuristic.asInstanceOf[Heuristic[Rise]]
  }

  def readFile(filePath: String): String = {
    // prepare output string
    var output = """"""
    // open file
    val bufferedSource = Source.fromFile(filePath)
    // read in file
    for (line <- bufferedSource.getLines()) {
      output += line
    }

    // close file properly
    bufferedSource.close

    output
  }

}


