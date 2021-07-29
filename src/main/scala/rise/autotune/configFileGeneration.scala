package rise.autotune

import arithexpr.arithmetic.{ArithExpr, PosInf, RangeAdd, RangeMul, Var}
import rise.core.types.{NatIdentifier, TuningParameter}

import scala.collection.mutable.ListBuffer
import constraints._

object configFileGeneration {


  def generateJSON2(p: Parameters,
                    c: Set[Constraint],
                    tuner: Tuner)
  :String = {

    val parametersWDCImmutable = distributeConstraints(p, c)
    val parametersWDC = scala.collection.mutable.Map.empty[NatIdentifier, (Set[Constraint], Set[NatIdentifier])]

    // copy elements to mutable map
    parametersWDCImmutable.foreach(param => {
      parametersWDC(param._1) = (param._2._1, param._2._2)
    })

    // number of samples for design of experiment phase
//    val doe = p.size * 10
    val doe = tuner.samples

    // create header for hypermapper configuration file
    val header =
      s"""{
         | "application_name" : "${tuner.name}",
         | "optimization_objectives" : ["runtime"],
         | "hypermapper_mode" : {
         |   "mode" : "client-server"
         | },
         | "feasible_output" : {
         |   "enable_feasible_predictor" : true,
         |   "name" : "Valid",
         |   "true_value" : "True",
         |   "false_value" : "False"
         | },
         | "design_of_experiment" : {
         |   "doe_type" : "random sampling",
         |   "number_of_samples" : ${doe}
         | },
         | "optimization_iterations" : ${tuner.samples},
         | "input_parameters" : {
         |""".stripMargin

    // create entry foreach parameter
    var parameter = ""
    parametersWDC.foreach(param => {

      val (values, constraintsFiltered) = param._1.range match {
        case RangeAdd(start, stop, step) => {

          // if step is not evaluable use 1 instead
          val stepWidth = step.isEvaluable match {
            case true => step.eval
            case false => 1
          }

          // avoid filtering of starting 1
          val values = start.eval match {
            case 1 => {
              stepWidth match {
                case 1 => List.range(start.evalInt, stop.evalInt + 1).filter(_ % stepWidth == 0)
                case _ => List(1) ++ List.range(start.evalInt, stop.evalInt + 1).filter(_ % stepWidth == 0)
              }
            }
            case _ =>
              List.range(start.evalInt, stop.evalInt + 1)
                .filter(_ % stepWidth == 0)
          }
          filterList(p, param._2._1, values, param._1)
        }
        case RangeMul(start, stop, mul) => {

          // if step is not evaluable use 1 instead
          val values = mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt)/scala.math.log(mul.evalDouble)

              start.evalInt match{
                case 1 =>
                  List.range(0, maxVal.toInt+1)
                    .map(power => scala.math.pow(mul.evalInt, power).toInt)
                case _ =>
                  List.range(start.evalInt, maxVal.toInt+1)
                    .map(power => scala.math.pow(mul.evalInt, power).toInt)
              }
            }
            case false =>
              List.range(start.evalInt, stop.evalInt)
          }

          // filtering
          filterList(p, param._2._1, values, param._1)
        }

        case _ => println("not yet implemented")

          println("name: " + param._1.name)
          println("range: " + param._1.range)

          (List.empty[Int], parametersWDC.apply(param._1)._1)
      }

      // update with filtered constraints
      parametersWDC(param._1) = (constraintsFiltered, param._2._2)

      // now write constraints

      // check if we have to generate constraints
      val parameterEntry = tuner.hierarchicalHM match {
        case true => {
          // use constraints

          // get dependencies and constraints from map
          val dependencies = parametersWDC(param._1)._2.size match {
            case 0 => "[]"
            case _ =>  elementListToString(parametersWDC(param._1)._2.toList)
          }

          val constraints = parametersWDC(param._1)._1.size match {
            case 0 => "[]"
            case _ => {
              val constraintsList = new ListBuffer[String]
              parametersWDC(param._1)._1.foreach(constraint => {
                // check type of constraint
                val constraintString = constraint match {
                  case RangeConstraint(n, r) => {
                    val (start, stop, step) = r match {
                      case RangeAdd(start, stop, step) => (start, stop, step)
                      case RangeMul(start, stop, step) => (start, stop, step)
                      case _ => (0, 0, 0) // todo catch other types of ranges
                    }

                    // if stop is PosInf, remove constraint
                    // (already catched by the range of parameter)
                    stop.toString match {
                      case "PosInf" =>{
                        val startConstraint = n.toString + " >= " + start
                        val stepConstraint = n.toString + " % " + step + " == 0"

                        startConstraint + " and " + stepConstraint
                      }
                      case _ => {
                        val startConstraint = n.toString + " >= " + start
                        val stopConstraint = n.toString + " <= " + stop
                        val stepConstraint = n.toString + " % " + step + " == 0"

                        startConstraint + " and " + stopConstraint + " and " + stepConstraint
                      }
                    }

                  }
                  case PredicateConstraint(n) => {
                    n.toString.contains("/^") match {
                      case true => constraint.toString.replace("/^", "/")
                      case false => constraint.toString
                    }
                  }
                }
                constraintsList += constraintString
              })
              elementListToString(constraintsList.filter(elem => elem.size != 0).toList)
            }
          }

          val parameterEntry =
            s"""   "${param._1.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${valuesListToString(values)},
               |       "constraints" : ${constraints},
               |       "dependencies" : ${dependencies}
               |   },
               |""".stripMargin

          parameterEntry
        }
        case false => {
          // don't use constraints
          val parameterEntry =
            s"""   "${param._1.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${valuesListToString(values)}
               |   },
               |""".stripMargin

          parameterEntry
        }
      }
      parameter += parameterEntry
    })

    // remove last comma
    val parameterSection = parameter.dropRight(2) + "\n"

    val foot =
      """ }
        |}
        |""".stripMargin

    val file = header + parameterSection + foot
    file
  }

  def generateJSON(p: Parameters,
                   c: Set[Constraint],
                   tuner: Tuner)
  : String = {

    val parametersWDC = distributeConstraints(p, c)

    // number of samples for design of experiment phase
    val doe = p.size * 10

    // create header for hypermapper configuration file
    val header =
    s"""{
       | "application_name" : "${tuner.name}",
       | "optimization_objectives" : ["runtime"],
       | "hypermapper_mode" : {
       |   "mode" : "client-server"
       | },
       | "feasible_output" : {
       |   "enable_feasible_predictor" : true,
       |   "name" : "Valid",
       |   "true_value" : "True",
       |   "false_value" : "False"
       | },
       | "design_of_experiment" : {
       |   "doe_type" : "random sampling",
       |   "number_of_samples" : ${doe}
       | },
       | "optimization_iterations" : ${tuner.samples},
       | "input_parameters" : {
       |""".stripMargin

    // create entry foreach parameter
    var parameter = ""
    p.foreach(elem => {

      val parameterRange = elem.range match {
        case RangeAdd(start, stop, step) =>  {

          // if step is not evaluable use 1 instead
          val stepWidth = step.isEvaluable match{
            case true => step.eval
            case false => 1
          }

          // avoid filtering of starting 1
          start.eval match {
            case 1 => {
              stepWidth match {
                case 1 => valuesListToString(
                  List.range(start.evalInt, stop.evalInt + 1)
                    .filter(_ % stepWidth == 0))
                case _ => valuesListToString(
                  List(1) ++ List.range(start.evalInt, stop.evalInt + 1)
                    .filter(_ % stepWidth == 0))
              }
            }
            case _ => valuesListToString(
              List.range(start.evalInt, stop.evalInt+1)
                .filter(_ % stepWidth == 0))
          }
        }

        case RangeMul(start, stop, mul) => {

          // if step is not evaluable use 1 instead
          mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt)/scala.math.log(mul.evalDouble)

              valuesListToString(
                List.range(start.evalInt, maxVal.toInt+1)
                  .map(power => scala.math.pow(mul.evalInt, power).toInt))
            }
            case false =>
              valuesListToString(List.range(start.evalInt, stop.evalInt))
          }
        }
        case _ => {
          println("Not yet implemented")
          ""
        }
      }

      // check if we have to generate constraints
      val parameterEntry = tuner.hierarchicalHM match {
        case true => {
          // use constraints

          // get dependencies and constraints from map
          val dependencies = parametersWDC(elem)._2.size match {
            case 0 => "[]"
            case _ =>  elementListToString(parametersWDC(elem)._2.toList)
          }

          val constraints = parametersWDC(elem)._1.size match {
            case 0 => "[]"
            case _ => {
              val constraintsList = new ListBuffer[String]
              parametersWDC(elem)._1.foreach(constraint => {
                // check type of constraint
                val constraintString = constraint match {
                  case RangeConstraint(n, r) => {
                    val (start, stop, step) = r match {
                      case RangeAdd(start, stop, step) => (start, stop, step)
                      case RangeMul(start, stop, step) => (start, stop, step)
                      case _ => (0, 0, 0) // todo catch other types of ranges
                    }

                    // if stop is PosInf, remove constraint
                    // (already catched by the range of parameter)
                    stop.toString match {
                      case "PosInf" =>{
                        val startConstraint = n.toString + " >= " + start
                        val stepConstraint = n.toString + " % " + step + " == 0"

                        startConstraint + " and " + stepConstraint
                      }
                      case _ => {
                        val startConstraint = n.toString + " >= " + start
                        val stopConstraint = n.toString + " <= " + stop
                        val stepConstraint = n.toString + " % " + step + " == 0"

                        startConstraint + " and " + stopConstraint + " and " + stepConstraint
                      }
                    }

                  }
                  case PredicateConstraint(n) => {
                    n.toString.contains("/^") match {
                      case true => constraint.toString.replace("/^", "/")
                      case false => constraint.toString
                    }
                  }
                }
                constraintsList += constraintString
              })
              elementListToString(constraintsList.filter(elem => elem.size != 0).toList)
            }
          }

          val parameterEntry =
            s"""   "${elem.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${parameterRange},
               |       "constraints" : ${constraints},
               |       "dependencies" : ${dependencies}
               |   },
               |""".stripMargin

          parameterEntry
        }
        case false => {
          // don't use constraints
          val parameterEntry =
            s"""   "${elem.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${parameterRange}
               |   },
               |""".stripMargin

          parameterEntry
        }
      }
      parameter += parameterEntry
    })

    // remove last comma
    val parameterSection = parameter.dropRight(2) + "\n"

    val foot =
      """ }
        |}
        |""".stripMargin

    val file = header + parameterSection + foot
    println("file: \n" + file )
    file
  }

  def valuesListToString(list: List[Any]): String = {
    var valuesString = ""
    list.foreach(value => {
      valuesString += value.toString +  ", "
    })
    "["  + valuesString.dropRight(2) + "]"
  }

  def elementListToString(list: List[Any]): String = {
    var valuesString = ""
    list.foreach(value => {
      valuesString += "\"" + value.toString + "\"" + ", "
    })
    "["  + valuesString.dropRight(2) + "]"
  }

  def distributeConstraints(parameters: Parameters,
                            constraints: Set[Constraint]
                           ): Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])] =  {

    // initialize output map and add parameters
    val parametersWDC = scala.collection.mutable.
      Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])]()
    parameters.toSeq.sortBy(_.name).foreach(param => {
      parametersWDC(param) = (Set.empty[Constraint], Set.empty[NatIdentifier])
    })

    // get parameters from constraint
    // check for given parameters in the given constraint
    val test = constraints.toSeq.sortBy(_.toString)
    constraints.toSeq.sortBy(_.toString).foreach(constraint => {
      val parametersInConstraint = getParametersFromConstraint(parameters, constraint)

      parametersInConstraint.size match {
        case 0 => // skip constraints without parameters
        case 1 => {
          // use this candidate (we only have one)
          val candidate = parametersInConstraint.last
          val elem = parametersWDC(candidate)
          parametersWDC(candidate) = (
            elem._1 + constraint,
            elem._2 ++ parametersInConstraint.filter(
              param => !(param.name.equals(candidate.name)))
          )
        }
        case _ => {
          // iterate over candidates
          //  true: next candidate
          //  false: add constraint and other parameters to candidate parameter
          // we do not stop if candidate is found!
          var i = 0
          parametersInConstraint.foreach(candidate => {
            // check if pointer occurs in other parameters' dependencies  (avoid cycles)
            parametersWDC.filter(
              paramWDC => !(paramWDC._1.name.equals(candidate.name)))
              .exists(paramWDC => {
                paramWDC._2._2.exists(dependency => candidate.name.equals(dependency.name))
              }) match {
              case false => {
                // use this candidate
                // add candidate to output map
                val elem = parametersWDC(candidate)
                parametersWDC(candidate) = (
                  elem._1 + constraint,
                  elem._2 ++ parametersInConstraint.filter(
                    param => !(param.name.equals(candidate.name))
                  )
                )
              }
              case true => // use next candidate
            }
          })
        }
      }
    })
    parametersWDC.toMap
  }

  // helper function to collect occurring parameters from a constraint
  def getParametersFromConstraint(parameters: Parameters,
                                  constraint: Constraint)
  : Seq[NatIdentifier] = {

    // collect parameters as vars in constraint
    val parametersInConstraint = constraint match {
      case RangeConstraint(n, r) => {
        val rangeVars = r match {
          case RangeAdd(start, stop, step) => {
            start.varList ++ stop.varList ++ step.varList
          }
          case RangeMul(start, stop, mul) => {
            start.varList ++ stop.varList ++ mul.varList
          }
          case _ => List.empty[Var]
        }
        (n.varList ++ rangeVars).toSet
      }
      case PredicateConstraint(n) => {
        n.freeVariables()
      }
    }

    // get occurring parameters form parameter list
    val output = parameters.filter(nat => {
      parametersInConstraint.exists(paramInConstraint => {
        paramInConstraint.name.equals(nat.name)
      })
    })

    output.toSeq.sortBy(_.name)
  }

  def filterList(p: Parameters, constraints: Set[Constraint], values:List[Int], param: NatIdentifier): (List[Int], Set[Constraint]) = {
    val constraintsFiltered:scala.collection.mutable.Set[Constraint] = constraints.to(collection.mutable.Set)

    // check for each value if all constraints pass or are not evaluable
    val valuesFiltered = values.filter(value => {
      constraints.forall(constraint => {
        val params = getParametersFromConstraint(p, constraint)
        // if occurring parameter in constraint matches given parameter, try to evaluate this constraint
        params.size match {
          case 1 => params.last.name match {
            case param.name => {
              // remove constraints from set of constraints
              constraintsFiltered.remove(constraint)

              // check constraint for this value
              constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> value)).isSatisfied()
            }
            case _ => true
          }
          case _ => true
        }
      })
    })
    (valuesFiltered, constraintsFiltered.toSet)
  }
//
//  def filterList2(p: Parameters, constraints: Set[Constraint], values:List[Int], param: NatIdentifier): List[Int] = {
//
//    // check for each value if all constraints pass or are not evaluable
//    values.filter(value => {
//      constraints.forall(constraint => {
//        val params = getParametersFromConstraint(p, constraint)
//        // if occurring parameter in constraint matches given parameter, try to evaluate this constraint
//        params.size match {
//          case 1 => params.last.name match {
//            case param.name => constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> value)).isSatisfied()
//            case _ => true
//          }
//          case _ => true
//        }
//      })
//    })
//  }

//
//
//    constraints.foreach(constraint => {
//
//      val params = getParametersFromConstraint(p, constraint)
//      // if occurring parameter in constraint matches given parameter, filter out
//      val filteredList = params.size match {
//        case 1 => params.last.name match {
//          case param.name =>
//            println("param is: " + params.last)
//            println("constraint is: " + constraint)
//            constraint match{
//              case RangeConstraint(n, r) => {
//                println("n: " + n)
//                println("r: " + r)
//                println("r: " + r.max)
//                println("r is eval: " + r.max.equals(PosInf))
//              }
//              case _ =>
//            }
//
//            val test = constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> 4))
//            println("test: " + test)
//            println("test: " + test.isSatisfied())
//
//            val test2 = constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> 3))
//            println("test2: " + test2)
//            println("test2: " + test2.isSatisfied())
//
//            println("values: " + values)
//            val filter = values.filter(value => constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> value)).isSatisfied())
////            println("filter: " + filter)
//            filter
//          case _ => values
//        }
//        case _ => values
//      }
//      println("filteredList: " + filteredList)
////      println("finalList: " + finalList)
//      finalList = filteredList
//    })
//
//    finalList
//  }
}
