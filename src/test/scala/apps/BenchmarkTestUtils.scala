package apps

import java.io._

import org.scalatest.BeforeAndAfterAll
import rise.core.Expr
import shine.OpenCL.GlobalSize
import test_util.Tests
import util.{KernelArgCreator, gen}
import yacx.Executor.BenchmarkResult
import yacx._

import scala.collection.mutable.ListBuffer

class BenchmarkTest extends Tests with BeforeAndAfterAll {
  private val compilerOptions = scala.Array("--gpu-architecture=compute_70",
    //TODO only for run on palma
    "--include-path=/Applic.HPC/Easybuild/skylake/2019a/software/CUDA/10.1.105-GCC-8.2.0-2.31.1/targets/x86_64-linux/include",
    //doris
    "--include-path=/opt/cuda/targets/x86_64-linux/include")

//  protected var latexPrinter: LatexPrinter.LatexFilePrinter = _
//  protected var dataSizes: Array[Long] = _
//  protected var iterations: Int = _
//
//  var device: Device = _
//
//  override def beforeAll() : Unit = {
//    yacx.Executor.loadLibrary()
//    device = Devices.findDevice()
//    LatexPrinter.init()
//
//    println(device)
//    println("MultiprocessorCount:" + device.getMultiprocessorCount)
//    println("SharedMemoryBlock: " + device.getSharedMemPerBlock)
//    println("SharedMemorySM: " + device.getSharedMemPerMultiprocessor)
//  }
//
//  override def afterAll() : Unit = {
//    LatexPrinter.end()
//  }
//
//  def benchmarkExecutor(mmKernel: Expr, name: String) : Unit = {
//    //TODO
//  }
//
//  def benchmarkYacx(kernelname: String, filename: String, name: String, creator: CudaGemmBenchmark) : BenchmarkResult = {
//    if (latexPrinter == null) { //Warmup
//      val code = new String(java.nio.file.Files.readAllBytes(new File("kernels/", filename).toPath))
//
//      creator.benchmark(code, kernelname, iterations, dataSizes, Options.createOptions(compilerOptions:_*))
//    } else if (!DataFile(name).exists){
//      println("Benchmark " + name)
//
//      val code = new String(java.nio.file.Files.readAllBytes(new File("kernels/", filename).toPath))
//
//      val benchmarkResult = creator.benchmark(code, kernelname, iterations, dataSizes, Options.createOptions(compilerOptions:_*))
//
//      latexPrinter.add(benchmarkResult, name)
//
//      benchmarkResult
//    } else {
//      latexPrinter.addPlot(name)
//      null
//    }
//  }
//
//  def benchmarkYacx(mmKernel: Expr, name: String, creator: KernelArgCreator) : BenchmarkResult = {
//    if (latexPrinter == null) { //Warmup
//      val kernel = gen.cuKernel(mmKernel, name)
//      compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))
//
//      kernel.kernel.benchmark(creator, iterations, dataSizes)
//    } else if (!DataFile(name).exists){
//      println("Benchmark " + name)
//      val kernel = gen.cuKernel(mmKernel, name)
//
//      compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))
//
//      val benchmarkResult = kernel.kernel.benchmark(creator, iterations, dataSizes)
//
//      latexPrinter.add(benchmarkResult)
//
//      benchmarkResult
//    } else {
//      latexPrinter.addPlot(name)
//      null
//    }
//  }
//
//  def benchmarkYacxWithSizes(mmKernel: Expr, name: String, creator: KernelArgCreator) : BenchmarkResult = {
//    if (latexPrinter == null) { //Warmup
//      val gridDim = creator.getGridDim(0)
//      val localSize = creator.getBlockDim(0)
//      val globalSize = GlobalSize(gridDim.x * localSize.size.x, gridDim.y * localSize.size.y, gridDim.z * localSize.size.z)
//
//      val kernel = gen.cuKernel(localSize, globalSize)(mmKernel, name)
//
//      compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))
//        kernel.kernel.addCompilerOption("-maxrregcount=91")
//
//      println("Benchmark with DataSize: " + dataSizes(0))
//      kernel.kernel.benchmark(creator, iterations, dataSizes)
//    } else if (!DataFile(name).exists){
//      println("Benchmark " + name)
//
//      val gridDim = creator.getGridDim(0)
//      val localSize = creator.getBlockDim(0)
//      val globalSize = GlobalSize(gridDim.x * localSize.size.x, gridDim.y * localSize.size.y, gridDim.z * localSize.size.z)
//
//      val kernel = gen.cuKernel(localSize, globalSize)(mmKernel, name)
//
//      compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))
//      if (creator.getBlockDim(0).size.x.eval /32 == 16)
//          kernel.kernel.addCompilerOption("-maxrregcount=91")
//
//      println("Benchmark with DataSize: " + dataSizes(0))
//      val benchmarkResult = kernel.kernel.benchmark(creator, iterations, dataSizes)
//
//      latexPrinter.add(benchmarkResult)
//
//      benchmarkResult
//    } else {
//      latexPrinter.addPlot(name)
//      null
//    }
//  }
//}
//
//case class DataFile(name: String){
//  val file = new java.io.File("latex_performance-test/data/" + name.replaceAll("_", "") + ".csv")
//  def exists: Boolean = file.exists()
//  override def toString: String = "data/" + name.replaceAll("_", "") + ".csv"
//}
//
//object LatexPrinter {
//  var FASTGEMMBENCHMATK: BenchmarkResult = null
//  var SIMPLEGEMMBENCHMARK: BenchmarkResult = null
//
//  val latexDir = new File("latex_performance-test")
//  val latexDataDir = new File(latexDir, "data")
//  val fileMain = new File(latexDir, "LatexMain.tex")
//  val fileDiagramsMain = new File(latexDir, "diagrams.tex")
//
//  val title = "matrix multiplication"
//  val xLabel = "dimension of matrix (m=n=k)"
//  val xTick = "4608, 8448"
//  val figureBegin = "\\begin{figure}[H]\n\t\\begin{tikzpicture}\n\t\\begin{axis}[legend pos=north west,const plot, axis on top, width=1.05\\textwidth,height=0.7\\textheight,\n\ttitle={" + title + "},\n\txlabel={" + xLabel + "},\n\txtick={" + xTick + "},\n\tscaled x ticks = false,\n\tylabel={time in ms}]\n\t"
//  val figureEnd = "\n\t\\end{axis}\n\t\\end{tikzpicture}\n\t\n\t\\centering\n\t\\label{fig:comparisonupload}\n\\end{figure}\n\t"
//
//  val printer = ListBuffer[LatexFilePrinter]()
//
//  def init(): Unit = {
//    if (!latexDir.exists()) latexDir.mkdir()
//    if (!latexDataDir.exists()) latexDataDir.mkdir()
//
//    java.nio.file.Files.copy(new File("LatexMain.tex").toPath, fileMain.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
//  }
//
//  def end(): Unit = {
//    printer.foreach(p => p.printDiagrams())
//
//    val filePrinter = new BufferedWriter(new FileWriter(fileDiagramsMain))
//
//    printer.foreach(p => {
//      filePrinter.write("\\input{" + p.fileDiagrams.getName + "}")
//      filePrinter.newLine()
//    })
//
//    filePrinter.close()
//  }
//
//  def printDataFile(benchmarkResult: BenchmarkResult, name: String): String = {
//    val file = DataFile(name)
//
//    val filePrinter = new BufferedWriter(new FileWriter(file.file))
//
//    filePrinter.write("%Benchmark " + name)
//    filePrinter.newLine()
//    filePrinter.newLine()
//    filePrinter.write("DataSize execution-time speedupFASTGEMM speedupSIMPLEGEMM total-time upload-time download-time max min standard-deviation")
//    filePrinter.newLine()
//
//    for (i <- benchmarkResult.getDataSizes.indices) {
//      val dataSize = benchmarkResult.getDataSizes()(i)
//      val kernelTime = benchmarkResult.getAverage()(i)
//
//      filePrinter.write("" + Math.sqrt(dataSize/4).toInt)
//      filePrinter.write(" " + kernelTime.getLaunch)
//      filePrinter.write(" " + (FASTGEMMBENCHMATK.getAverage()(i).getLaunch / kernelTime.getLaunch))
//      filePrinter.write(" " + 0)
//      filePrinter.write(" " + kernelTime.getTotal)
//      filePrinter.write(" " + kernelTime.getUpload)
//      filePrinter.write(" " + kernelTime.getDownload)
//
//      val resultsI = benchmarkResult.getResult()(i)
//      var max = resultsI(0)
//      var min = resultsI(0)
//      var varianz = 0D
//
//      for (j <- resultsI.indices){
//        val launchIJ = resultsI(j)
//        val launchIJLaunch = launchIJ.getLaunch
//
//        if (launchIJLaunch > max.getLaunch)
//          max = launchIJ
//
//        if (launchIJLaunch < min.getLaunch)
//          min = launchIJ
//
//        varianz += (launchIJLaunch - benchmarkResult.getAverage()(i).getLaunch) * (launchIJLaunch - benchmarkResult.getAverage()(i).getLaunch)
//      }
//
//      filePrinter.write(" " + max.getLaunch)
//      filePrinter.write(" " + min.getLaunch)
//      filePrinter.write(" " + Math.sqrt(varianz/resultsI.length.asInstanceOf[Double]))
//
//      filePrinter.newLine()
//    }
//
//    filePrinter.close()
//
//    file.name
//  }
//
//  case class LatexFilePrinter(name: String) {
//    printer += this
//
//    val fileDiagrams = new File(latexDir, "diagrams-" + name + ".tex")
//
//    val plots = ListBuffer[String]()
//    val kernelNames = ListBuffer[String]()
//
//    def add(benchmarkResult: BenchmarkResult): Unit = {
//      add(benchmarkResult, benchmarkResult.getKernelName)
//    }
//
//    def add(benchmarkResult: BenchmarkResult, name: String): Unit = {
//      addPlot(printDataFile(benchmarkResult, name))
//    }
//
//    def addPlot(name: String): Unit = {
//      val file = DataFile(name)
//      kernelNames += file.name
//      plots += "\\addplot table [x=DataSize, y=execution-time] {" + file.toString + "};"
//    }
//
//    def printDiagrams(): File = {
//      val filePrinter = new BufferedWriter(new FileWriter(fileDiagrams))
//
//      filePrinter.write(figureBegin)
//
//      filePrinter.newLine()
//      plots.foreach(plot => {
//        filePrinter.write(plot)
//        filePrinter.newLine()
//      })
//
//      filePrinter.write("\\legend{" + kernelNames.map(name => name.replaceAll("_", " ")).mkString(", ") + "}")
//      filePrinter.newLine()
//
//      filePrinter.write(figureEnd)
//
//      filePrinter.close()
//
//      fileDiagrams
//    }
//  }
}