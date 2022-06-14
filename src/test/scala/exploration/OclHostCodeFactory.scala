package exploration

import exploration.matmath._
import exploration.matmath.Operators._
import rise.autotune.HostCode
import util.writeToTempFile

import java.io.File

object OclHostCodeFactory { // This guy did way too much java!!
  private val initInputBufferCode: (Int, Int, File) => String = (n, length, valueFile) =>
    s"""
       |  Buffer inBuff$n = createBuffer(ctx, $length * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  float* in$n = hostBufferSync(ctx, inBuff$n, $length * sizeof(float), HOST_WRITE);
       |  file = fopen("${valueFile.getAbsolutePath}", "r");
       |  for (int i = 0; i < $length; i++) {
       |    fscanf(file, "%f", &in$n[i]);
       |  }
       |  fclose(file);
       |""".stripMargin

  private val initOutputBufferCode: Int => String = length =>
    s"""
       |  Buffer outputM = createBuffer(ctx, $length * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |""".stripMargin

  private val writeValueToFile: Value[_] => File = {
    case Mat(cols@_*) => writeToTempFile("oclValue", "", cols.map(_.mkString(" ")).mkString("\n"))
    case Vec(values@_*) => writeToTempFile("oclValue", "", values.mkString(" "))
    case Scalar(value) => writeToTempFile("oclValue", suffix = "", value.toString)
  }

  def oclFloatHostCode[T](inputs: Seq[Value[Float]], expectedOutput: Value[Float], args: Seq[Integer]): HostCode = {
    val init =
      s"""
         |  FILE* file;
         |""".stripMargin +
    inputs.map(_.flatSize)
      .lazyZip(inputs.map(writeValueToFile))
      .lazyZip(inputs.indices)
      .map {case (size, file, n) => initInputBufferCode(n, size, file)}
      .mkString("\n") +
      initOutputBufferCode(expectedOutput.flatSize)

    val compute = (
      Seq("ctx", "&fun", "outBuff") ++
      args.map(_.toString) ++
      inputs.indices.map("inBuff" + _)
    ).mkString(s"  fun_run(", ", ", ");")

    val expectedOutputFile = writeValueToFile(expectedOutput)
    val finish =
      s"""
         |  file = fopen("${expectedOutputFile.getAbsolutePath}", "r");
         |  float x;
         |  int diffs = 0;
         |  for (int i = 0; i < ${expectedOutput.flatSize}; i++) {
         |    fscanf(file, "%f", &x);
         |    if(out[i] != x){
         |      diffs++;
         |    }
         |  }
         |""".stripMargin +
      inputs.indices
        .map(n => s" destroyBuffer(ctx, inBuff$n);")
        .mkString("\n")+
      s"""
         |  if(diffs > 0){ //Not OK
         |    exit(42);
         |  }
         |""".stripMargin

    HostCode(init,compute,finish)
  }
}
