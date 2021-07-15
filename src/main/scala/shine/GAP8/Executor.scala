package shine.GAP8

import apps.SobelFilter

import java.nio.file.{Files, Path, Paths}
import scala.language.postfixOps
import scala.sys.process._

case class Executor(gapSdkPath: String, target: ExecutionTarget, OS: TargetOperatingSystem, IO: InputOutput) {

  /**
    * At some point in future: Includes or whatever (to fill Makefile with that info)
    *
    * 1. Create temp folder
    * 2. Create temp file in that folder, write code to that temp file
    * 3. Copy Makefile and runtime lib to the previously created folder
    *
    * 4. Source gapuino_v2.sh
    * 5. CD to previously created folder
    * 6. make clean all run
    * 7. ???
    * 8. Profit
    * */

  val protoFilesPath = "runtime/"
  val gapv2scriptPath = "configs/gapuino_v2.sh"

  val makeClean = "make clean"
  val makeBuild = s"make PMSIS_OS=$OS platform=$target io=$IO all -j4"
  val makeRun = s"make platform=$target io=$IO run"

  def execute(code: String): String = {
    val tmpDir = Files.createTempDirectory("")
    val tmpSourceFile = Files.createTempFile(tmpDir, "code-", ".c")
    val fullProtoFilesPath = Paths.get(protoFilesPath)

    Files
      .walk(fullProtoFilesPath, 2)
      .forEach(filePath => {
        if (!filePath.equals(fullProtoFilesPath)){
          Files.copy(
            filePath,
            Paths.get(tmpDir.toString, filePath.toString.substring(fullProtoFilesPath.toString.length))
          )
        }
    })

    util.writeToFile(tmpSourceFile.toFile, code)
    //Files.writeString(tmpSourceFile, code)

    val makefilePath = Paths.get(tmpDir.toAbsolutePath.toString, "Makefile")
    //val lines = Files.readString(makefilePath)
    //val lines = Files.readAllLines(makefilePath)
    val lines = util.readFile(makefilePath.toString)
    //Files.writeString(makefilePath, lines.replaceAll("#APP_NAME_PLACEHOLDER|#APP_MAIN_SRC_PLACEHOLDER", tmpSourceFile.getFileName.toString))
    util.writeToFile(makefilePath.toFile, lines.replaceAll("#APP_NAME_PLACEHOLDER|#APP_MAIN_SRC_PLACEHOLDER", tmpSourceFile.getFileName.toString))

    val pathToConfigScript = Paths.get(gapSdkPath, gapv2scriptPath)

    val cleanBuildRunSeq =
      s"""
        |/bin/bash -c "source ${pathToConfigScript.toAbsolutePath.toString};
        | cd ${tmpDir.toAbsolutePath.toString};
        | $makeClean;
        | $makeBuild;
        | $makeRun;"
        |""".stripMargin

    println(cleanBuildRunSeq)

    cleanBuildRunSeq !!
  }
}

object Executor {
  def apply(sdkPath: String) = new Executor(sdkPath, Gvsoc, FreeRTOS, Host)
  def apply(sdkPath: String, target: ExecutionTarget, OS: TargetOperatingSystem, IO: InputOutput) =
    new Executor(sdkPath, target, OS, IO)
}

class ExecutionTarget
object Gvsoc extends ExecutionTarget{
  override def toString: String = "gvsoc"
}
object Board extends ExecutionTarget{
  override def toString: String = "board"
}

class TargetOperatingSystem
object FreeRTOS extends TargetOperatingSystem{
  override def toString: String = "freertos"
}
object PulpOS extends TargetOperatingSystem{
  override def toString: String = "pulpos"
}

class InputOutput
object Host extends InputOutput{
  override def toString: String = "host"
}
object Uart extends InputOutput{
  override def toString: String = "uart"
}
