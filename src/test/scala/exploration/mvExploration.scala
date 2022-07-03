package exploration

import apps.separableConvolution2D.mulT
import exploration.matmath.Operators._
import exploration.matmath._
import exploration.strategies.{defaultStrategiesGPU, simpleStrategiesGPU}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL.mapGlobal
import rise.openCL.primitives.oclReduceSeq


class mvExploration extends test_util.Tests {


  test("mvExploration"){
    val test = Array.empty[String]
    mvExploration.main(test)
  }
}

object mvExploration {

  // sub expressions
  val mult = impl { dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))

  // lowered versions
  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))

  val dotOcl: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> mapSeq(mulT) |> oclReduceSeq(AddressSpace.Global)(add)(lf32(0.0f))
  ))

  val scalOcl = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  // main expressions
  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |>
        map(mulT) |>
        reduce(add)(lf32(0.0f))
    ))
  ))

  val mvHighLevel2 = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |>
        map(fun(x => fst(x) * snd(x))) |>
        reduce(add)(lf32(0.0f))
    ))
  ))


  val gemvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32) ->: f32 ->: f32 ->:
      (m `.` f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(map(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta)) |>
      map(fun(x => x._1 + x._2))
  ))


  val mvOcl = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32) ->: f32 ->: f32 ->:
      (m `.` f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(mapSeq(fun(row => alpha * dotOcl(row, xs)))(mat))(scalOcl(ys, beta)) |>
      mapGlobal(0)(fun(x => x._1 + x._2))
  ))

  /*
  def writeMat[A] (mat: Mat[A]): File = writeToTempFile("mvInput", "", mat.cols.map(_.values.mkString(" ")).mkString("\n"))
  def writeVec[A] (vec: Vec[A]): File = writeToTempFile("mvInput", "", vec.values.mkString(" "))

  case class OCLBufferSpec(i: Int, dimension: (Int, Int), size: Int, dataFile: File)

  def oclFloatHostCode(inputs: Seq[Value[Float]], args: Seq[Integer], expectedOutput: Value[Float]): HostCode = {
    def declareVars(n:Int, value: Value[Float]) = {
      case mat@Mat(vals)  => declareInVars(n, mat.shape._1 * mat.shape._2)
      case vec@Vec(_) => declareInVars(n, vec.size)
      case Scalar(value) => s"float in$n = $value;"
    }
    def declareInVars(n:Int, length:Int) =
      s"""
         |  int length$n = $length
         |  Buffer inBuff$n = createBuffer(ctx, length$n * sizeof(float), HOST_WRITE | DEVICE_READ);
         |  float* in$n = hostBufferSync(ctx, inBuff$n, length$n * sizeof(float), HOST_WRITE);
         |""".stripMargin

    def writeValue(value: Value[Float]) = {
      case Mat(vecs) =>
      case Vec(values,v2) => writeToTempFile("mvInput", "", values.mkString(" "))
    }

    def fillInVar(n:Int, value: Value[Float]) = {
      def tmpFile = writeValue(value)
      s"""
         |  file = fopen("${spec.dataFile.getAbsolutePath}", "r");
         |  for (int i = 0; i < ${spec.size}; i++) {
         |    fscanf(file, "%f", &in${spec.i}[i]);
         |  }
         |  fclose(file);
         |""".stripMargin
    }


    val init = "FILE* file;" +
      inputs.zipWithIndex.map(declareVars(_.))

  }
  */

  /*
  def oclHostCode(inputs: Seq[(Int,Int)], output: (Int,Int), args: Seq[Int]): HostCode = {

    val Seq(inputSpecs, Seq(outputSpec)) = Seq(inputs, Seq(output))
      .map(_.zipWithIndex
        .map { case (dimension, i) => OCLBufferSpec(
          i = i,
          dimension = dimension,
          size = dimension._1 * dimension._2,
          dataFile = writeMat(Mat.generate(dimension._1, dimension._2)(Math.random.toFloat))
        )}
      )
    */
/*
    val inputSpecs = inputs.zipWithIndex.map { case (dimension, i) => OCLBufferSpec(
      i = i,
      dimension = dimension,
      size = dimension._1 * dimension._2,
      dataFile = writeMat(Mat.generate(dimension._1, dimension._2)(Math.random.toFloat))
    )}
 */

    //val outputSpec =

/*
    val init =
      "FILE* file;" +
      inputSpecs.map(spec =>
      s"""
         |  Buffer inBuff${spec.i} = createBuffer(ctx, ${spec.size} * sizeof(float), HOST_WRITE | DEVICE_READ);
         |  float* in${spec.i} = hostBufferSync(ctx, inBuff${spec.i}, ${spec.size} * sizeof(float), HOST_WRITE);
         |  file = fopen("${spec.dataFile.getAbsolutePath}", "r");
         |  for (int i = 0; i < ${spec.size}; i++) {
         |    fscanf(file, "%f", &in${spec.i}[i]);
         |  }
         |  fclose(file);
         |
         |""".stripMargin
    ).mkString +
      s"""
         | Buffer outBuff = createBuffer(ctx, ${outputSpec.size} * sizeof(float), HOST_READ | DEVICE_WRITE);
         | float* out = hostBufferSync(ctx, outBuff, ${outputSpec.size} * sizeof(float), HOST_READ);
         |""".stripMargin

    val runArgs = Seq("ctx", "&fun", "outBuff") ++ args.map(_.toString) ++ inputSpecs.map("inBuff" + _.i)
    val compute = s"  fun_run(${runArgs.mkString(", ")});";

    val finish =
      s"""
         |  file = fopen("${outputSpec.dataFile.getAbsolutePath}", "r");
         |  float x;
         |  int diffs = 0;
         |  for (int i = 0; i < ${outputSpec.size}; i++) {
         |    if(out[i] != fscanf(file, "%f", &x)){
         |      diffs++;
         |    }
         |  }
         |
         |${inputSpecs.map(spec => s" destroyBuffer(ctx, inBuff${spec.i});\n").mkString}
         |  destroyBuffer(ctx, outBuff);
         |
         |  if(diffs > 0){ //Not OK
         |    exit(42);
         |  }
         |""".stripMargin

    HostCode(init , compute, finish)
    HostCode("","","")
  }
  */

  def main(args: Array[String]): Unit = {
    val m = Mat.generate(1024,1024)(Math.random.toFloat)
    val v = Vec.generate(1024)(Math.random.toFloat)

    val hostCode = OclHostCodeFactory.oclFloatHostCode(Seq(m,v), m*v, Seq(1024, 1024))

    //val hostCode = oclHostCode(Seq((1024,1024),(1,1024)),(1,1024), Seq(1024, 1024))
    //val hostCode = mvHostCode(1024, 1024)
    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner.json", Some(hostCode))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner_debug.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//        riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_ii.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel2, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel2, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_cot_debug.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel2, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_cot_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel2, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_cot2_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    riseExploration(mvHighLevel2, simpleStrategiesGPU.lowering, simpleStrategiesGPU.strategies, "exploration/configuration/mv/mv_rs.json", None)
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive_debug.json", None)
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_debug.json", None)
  }

}
