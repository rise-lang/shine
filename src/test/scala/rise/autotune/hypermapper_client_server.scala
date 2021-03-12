package rise

class hypermapper_client_server extends test_util.Tests {

  test("minimal example"){
    // spawn subprocess
    val sub = os.proc("python", "-u", "autotuning/sub_proc_example.py").spawn()

    // write message to subprocess
    sub.stdin.write("hi from parent\n")
    sub.stdin.flush()

    // read message from subprocess
    val text0 = sub.stdout.readLine()
    println("message from subprocess: " + text0)

    // read another message from subprocess
    val text1 = sub.stdout.readLine()
    println("message from subprocess: " + text1)

    // write message to subprocess
    sub.stdin.write("hi from parent again\n")
    sub.stdin.flush()

    // read message from subprocess
    val text2 = sub.stdout.readLine()
    println("message from subprocess: " + text2)
  }

  test("hm client server") {

    val sub = os.proc("python", "-u", "autotuning/hm_wrapper.py", "--config_file", "autotuning/convolution.json").spawn()
    val command = sub.stdout.readLine()
    println("command: " + command)

    var iteration = 0

    // while subprocess is alive
    while(sub.isAlive()) {
      iteration = iteration + 1

      // read in reqeust
      val request = sub.stdout.readLine()

      // check if HM has ended
      // maybe request contains a "warning", this should not be a problem
      request match {
        case "End of HyperMapper" => System.exit(0)
        case _ => {

          println("Iteration: " + iteration)
          println(request)

          // write header to string to hypermapper
          val str_to_hypermapper_header = "vec,tile,runtime,Valid\n"
          sub.stdin.write(str_to_hypermapper_header)
          sub.stdin.flush()

          // parse headers
          val headers = sub.stdout.readLine()

          println("headers: " + headers)
          print(str_to_hypermapper_header)

          val num_of_eval_requests = request.split(" ")(1).toInt
          for (row <- Range(0, num_of_eval_requests)) {

            // read in parameters values
            val parameters_values_string = sub.stdout.readLine()
            val parameters_values = parameters_values_string.split(",").map(x => x.trim())

            // parse parameters
            // WARNING: hardcoded
            val vec = parameters_values(0).toFloat
            val tile = parameters_values(1).toFloat

            // call function / execution f(x1, x2)
            // replace parameters in expression
            // execute expression

            // use this to simulated runtime
            val f = vec + tile

            // create output string to hypermapper
            val str_to_hypermapper:String =
              vec.toString + "," +
              tile.toString + "," +
              f.toString + "," +
              "True" +
              "\n"

            print(str_to_hypermapper)

            sub.stdin.write(str_to_hypermapper)
            sub.stdin.flush()
          }
        }
      }
    }
  }}