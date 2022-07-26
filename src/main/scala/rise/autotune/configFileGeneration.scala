package rise.autotune

import arithexpr.arithmetic.{PosInf, RangeAdd, RangeMul, Var}
import rise.core.types.{NatIdentifier, TuningParameter}

import scala.collection.mutable.ListBuffer
import constraints._
import play.api.libs.json.Json

import exploration.explorationUtil.jsonParser.readFile


object configFileGeneration {


  def generateJSON(p: Parameters,
                   c: Set[Constraint],
                   tuner: Tuner
                  ): String = {

    val parametersWDCImmutable = distributeConstraints(p, c)

    // number of samples for design of experiment phase
    //    val doe = p.size * 10
    //    val optimization_iterations = tuner.samples

    val doe = p.size + 1
    //    val optimization_iterations = tuner.samples

    val optimization_iterations = p.size match {
      case 0 => 0
      case _ => tuner.samples
    }

    // create header for hypermapper configuration file
    val header =
      s"""{
         | "application_name" : "${tuner.name}",
         | "optimization_objectives" : ["runtime"],
         | "hypermapper_mode" : {
         |   "mode" : "client-server"
         | },
         | "scalarization_method": "linear",
         | "models": {
         |   "model": "gaussian_process"
         | },
         | "log_transform_output": true,
         | "epsilon_greedy_threshold": 0,
         | "log_file" : "${tuner.name}.log",
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
         | "optimization_iterations" : ${optimization_iterations},
         | "input_parameters" : {
         |""".stripMargin

    val header_opentuner =
      s"""{
         | "application_name" : "${tuner.name}",
         | "optimization_objectives" : ["runtime"],
         | "hypermapper_mode" : {
         |   "mode" : "client-server"
         | },
         | "log_file" : "${tuner.name}.log",
         | "feasible_output" : {
         |   "enable_feasible_predictor" : true,
         |   "name" : "Valid",
         |   "true_value" : "True",
         |   "false_value" : "False"
         | },
         | "scalarization_method": "linear",
         | "optimization_method": "opentuner",
         | "optimization_iterations" : ${optimization_iterations},
         | "input_parameters" : {
         |""".stripMargin


    // create entry foreach parameter
    var parameter = ""

    parametersWDCImmutable.foreach { case (param, wdc) => {

      val (values, constraintsFiltered) = param.range match {
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
                case 1 => List.range(start.evalInt, stop.evalInt + 1)
                  .filter(_ % stepWidth == 0)
                case _ => List(1) ++ List.range(start.evalInt, stop.evalInt + 1)
                  .filter(_ % stepWidth == 0)
              }
            }
            case _ =>
              List.range(start.evalInt, stop.evalInt + 1)
                .filter(_ % stepWidth == 0)
          }

          filterList(p, wdc._1, values, param)
        }
        case RangeMul(start, stop, mul) => {

          // if step is not evaluable use 1 instead
          val values = mul.isEvaluable match {
            case true => {
              val maxVal = scala.math.log(stop.evalInt) / scala.math.log(mul.evalDouble)

              start.evalInt match {
                case 1 =>
                  List.range(0, maxVal.toInt + 1)
                    .map(power => scala.math.pow(mul.evalInt, power).toInt)
                case _ =>
                  List.range(start.evalInt, maxVal.toInt + 1)
                    .map(power => scala.math.pow(mul.evalInt, power).toInt)
              }
            }
            case false =>
              List.range(start.evalInt, stop.evalInt)
          }

          // filtering
          filterList(p, wdc._1, values, param)
        }

        // todo implement this
        // hard coded hacky solution
        case _ => println("not yet implemented")

          println("name: " + param.name)
          println("range: " + param.range)

          val values = List.range(1, 1024)
          //          filterList(p, param._2._1, values, param._1)

          //          (List.empty[Int], parametersWDC.apply(param._1)._1)
          filterList(p, wdc._1, values, param)

        //          (List.empty[Int], wdc._1)
      }

      // check if value are empty
      //      println("values: " + values)
      if (values.size == 0) {
        //        println("all values were filtered out - check your constraints")
        throw new Exception("all values were filtered out - check your constraints")
      }

      // get new element with filtered constraints
      val newWdc = (constraintsFiltered, wdc._2)

      // write constraints

      // get dependencies and constraints from map
      val dependencies = elementListToString(newWdc._2.toList)

      // get constraints list as string
      val constraints = constraintsToString(newWdc._1)

      // todo think about why order can change

      // check if we have to generate constraints
      val parameterEntry = tuner.hmConstraints match {
        case true => {

          val parameterEntry =
            s"""   "${param.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${values.mkString("[", ", ", "]")},
               |       "constraints" : ${constraints},
               |       "dependencies" : ${dependencies},
               |       "transform" : "log"
               |   },
               |""".stripMargin

          parameterEntry
        }
        case false => {
          // don't use constraints
          val parameterEntry =
            s"""   "${param.name}" : {
               |       "parameter_type" : "ordinal",
               |       "values" : ${values.mkString("[", ", ", "]")},
               |       "parameter_default" : 1
               |   },
               |""".stripMargin

          parameterEntry
        }
      }
      parameter += parameterEntry

    }
    }

    // todo make this generic
    // warning: this is a workaround introducing a dummy parameter
    parametersWDCImmutable.size match {
      case 0 => // add dummy parameter entry

        val parameterEntry = tuner.hmConstraints match {
          case true => {

            val parameterEntry =
              s"""   "None" : {
                 |       "parameter_type" : "integer",
                 |       "values" : [0, 10],
                 |       "constraints" : [],
                 |       "dependencies" : []
                 |   },
                 |""".stripMargin

            parameterEntry
          }
          case false => {
            // don't use constraints
            val parameterEntry =
              s"""   "None" : {
                 |       "parameter_type" : "ordinal",
                 |       "values" : [0],
                 |   },
                 |""".stripMargin

            parameterEntry
          }
        }
        parameter += parameterEntry


      case _ => //ignore
    }

    // remove last comma
    val parameterSection = parameter.dropRight(2) + "\n"

    val foot =
      """ }
        |}
        |""".stripMargin

    val file = header + parameterSection + foot

    //    println("file: " + file)

    file
  }

  def parseFromJson(filePath: String, word: String) = {
    Json.parse(readFile(filePath)).apply(word).toString().replaceAll("\"", "")
  }

  def constraintsToString(constraints: Set[Constraint]): String = {

    val constraintsList = new ListBuffer[String]
    constraints.foreach(constraint => {
      // check type of constraint
      val constraintsAsString: ListBuffer[String] = constraint match {
        case RangeConstraint(n, r) => {
          r match {
            case RangeAdd(start, stop, step) => {

              // if stop is PosInf, remove constraint
              // (already catched by the range of parameter)
              stop match {
                case PosInf => {
                  val startConstraint = n.toString + " >= " + start
                  val stepConstraint = n.toString + " % " + step + " == 0"

                  startConstraint + " and " + stepConstraint
                  ListBuffer[String](startConstraint, stepConstraint)
                }
                case _ => {
                  val startConstraint = n.toString + " >= " + start
                  val stopConstraint = n.toString + " <= " + stop
                  val stepConstraint = n.toString + " % " + step + " == 0"

                  //              startConstraint + " and " + stopConstraint + " and " + stepConstraint
                  ListBuffer[String](startConstraint, stopConstraint, stepConstraint)
                }
              }
            }
            case RangeMul(start, stop, step) => {
              // if stop is PosInf, remove constraint
              // (already catched by the range of parameter)
              stop match {
                case PosInf => {
                  val startConstraint = n.toString + " >= " + start
                  val stepConstraint = n.toString // todo convert range mul constraint into formula

                  startConstraint + " and " + stepConstraint
                  ListBuffer[String](startConstraint, stepConstraint)
                }
                case _ => {
                  val startConstraint = n.toString + " >= " + start
                  val stopConstraint = n.toString + " <= " + stop
                  val stepConstraint = n.toString + " % " + step + " == 0"

                  //              startConstraint + " and " + stopConstraint + " and " + stepConstraint
                  ListBuffer[String](startConstraint, stopConstraint, stepConstraint)
                }
              }
            }
            case _ => (0, 0, 0) // todo catch other types of ranges
              ListBuffer[String]()
          }
        }
        case PredicateConstraint(n) => ListBuffer[String](n.toString.replace("/^", "/"))
      }
      //      constraintsAsString foreach(elem => constraintsList += elem)
      constraintsList ++= constraintsAsString
    })
    elementListToString(constraintsList.filter(elem => elem.size != 0).toList)
  }

  def elementListToString(list: List[Any]): String = {
    list.size match {
      case 0 => "[]"
      case _ => list.mkString("[\"", "\", \"", "\"]")
    }
  }

  def distributeConstraints(parameters: Parameters,
                            constraints: Set[Constraint]
                           ): Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])] = {

    // initialize output map and add parameters
    val parametersWDC = scala.collection.mutable.
      Map[NatIdentifier, (Set[Constraint], Set[NatIdentifier])]()
    parameters.toSeq.sortBy(_.name).foreach(param => {
      parametersWDC(param) = (Set.empty[Constraint], Set.empty[NatIdentifier])
    })

    // get parameters from constraint
    // check for given parameters in the given constraint
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

  def filterList(p: Parameters, constraints: Set[Constraint], values: List[Int], param: NatIdentifier): (List[Int], Set[Constraint]) = {
    val constraintsFiltered:
      scala.collection.mutable.Set[Constraint] = constraints.to(collection.mutable.Set)

    // check for each value if all constraints pass or are not evaluable
    val valuesFiltered = values.filter(value => {
      constraints.forall(constraint => {
        val params = getParametersFromConstraint(p, constraint)
        // if occurring parameter in constraint matches given parameter
        // try to evaluate this constraint
        params.size match {
          case 1 => params.last.name match {
            case param.name => {
              // remove constraints from set of constraints
              constraintsFiltered.remove(constraint)

              // check constraint for this value
              constraint.substitute(Map(TuningParameter(param.name.substring(6)) -> value))
                .isSatisfied()
            }
            case _ => true
          }
          case _ => true
        }
      })
    })
    (valuesFiltered, constraintsFiltered.toSet)
  }

  // function to check cycle in parameter dependencies
  def check_no_cycle(distribution: Map[NatIdentifier,
    (Set[constraints.Constraint], Set[NatIdentifier])]
                    ): Boolean = {

    def check_no_cycle_rec(param: NatIdentifier,
                           dependencies: Set[NatIdentifier],
                           distribution: Map[NatIdentifier,
                             (Set[constraints.Constraint], Set[NatIdentifier])],
                           visited: Set[NatIdentifier]
                          ): Boolean = {

      dependencies.size match {
        case 0 => true
        case _ => dependencies.exists(dependency => {
          dependency.name.equals(param.name) || visited.contains(dependency)
        }) match {
          case true => false
          case false => dependencies.forall(dependency => {
            check_no_cycle_rec(param,
              distribution.apply(dependency)._2,
              distribution,
              visited + dependency)
          })
        }
      }
    }

    distribution.forall(param =>
      check_no_cycle_rec(param._1, param._2._2, distribution, Set.empty[NatIdentifier]))
  }
}
