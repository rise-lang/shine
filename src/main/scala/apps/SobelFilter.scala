package apps

import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import rise.elevate.Rise
import rise.openMP.primitives.mapPar
import util.writeToPath

import scala.language.postfixOps

// scalastyle:off
object SobelFilter {

  val gapSqrt = foreignFun("gap_sqrt",
    Seq("a_nInput"),
    """
      | {
      |   uint32_t op  = a_nInput;
      |   uint32_t res = 0;
      |
      |   uint32_t one = 1uL << 30;
      |   while (one > op){
      |     one >>= 2;
      |   }
      |   while (one != 0) {
      |     if (op >= res + one){
      |       op = op - (res + one);
      |       res = res +  2 * one;
      |     }
      |     res >>= 1;
      |     one >>= 2;
      |   }
      |   return res;
      | }
      |""".stripMargin,
    u32 ->: u32
  )

  /**
    * TODO: Embed sobelX and sobelY matrices
    * */
  val clusterFun: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
    fun((n`.`m`.`u8) ->: (3`.`3`.`int) ->: (3`.`3`.`int) ->: (n`.`m`.`u8))((pic, h_w, v_w) =>
      pic |>
        padClamp2D(l = 1, r = 1) |>
        slide2D(sz = 3, st = 1) |>
        mapPar(mapSeq(fun(submat => {
          zip(submat |> join)(h_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(h =>
            zip(submat |> join)(v_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(v =>
              cast(gapSqrt(h * h + v * v)) :: u8
            )
          )
        }
        )))
    )
  )

  val functionCode = util.gen.gap8.function("cluster_core_task").asStringFromExpr(clusterFun)

  val code =
    s"""
       | #include <stdio.h>
       | #include "pmsis.h"
       | #include "gaplib/ImgIO.h"
       |
       | #define IMG_LINES 240
       | #define IMG_COLS 320
       |
       | #define STACK_SIZE 2048
       |
       | typedef unsigned char byte;
       | byte* ImageIn_L2;
       | byte* ImageOut_L2;
       |
       | int G_X[] = {
       |     -1, 0, 1,
       |     -2, 0, 2,
       |     -1, 0, 1
       | };
       |
       | int G_Y[] = {
       |     -1, -2, -1,
       |     0, 0, 0,
       |     1, 2, 1
       | };
       |
       | ${functionCode}
       |
       | void cluster_entry_point(void* args)
       | {
       |     printf("Main cluster entry point\\n");
       |
       |     pi_perf_conf(1 << PI_PERF_ACTIVE_CYCLES);
       |     pi_perf_reset();
       |     pi_perf_start();
       |     int time1 = pi_perf_read(PI_PERF_ACTIVE_CYCLES);
       |
       |     /* Fork the cluster_core_task on all of the cluster cores */
       |     pi_cl_team_fork(pi_cl_cluster_nb_cores(), cluster_core_task, args);
       |
       |     /* Stop the counter and print # active cycles */
       |     pi_perf_stop();
       |     int time2 = pi_perf_read(PI_PERF_ACTIVE_CYCLES);
       |     printf("Total cycles: %d\\n", time2 - time1);
       | }
       |
       | void sobel_filter_main()
       | {
       |     printf("Main FC entry point\\n");
       |
       |     char *in_image_file_name = "valve.pgm";
       |     char path_to_in_image[64];
       |     sprintf(path_to_in_image, "../../../%s", in_image_file_name);
       |
       |     int image_size_bytes = IMG_COLS * IMG_LINES * sizeof(byte);
       |
       |     ImageIn_L2 = (byte *) pi_l2_malloc(image_size_bytes);
       |     ImageOut_L2 = (byte *) pi_l2_malloc(image_size_bytes);
       |
       |     if(ReadImageFromFile(path_to_in_image, IMG_COLS, IMG_LINES, 1, ImageIn_L2, image_size_bytes, IMGIO_OUTPUT_CHAR, 0))
       |     {
       |         printf("Failed to load image %s\\n", path_to_in_image);
       |         pmsis_exit(-1);
       |     }
       |
       |     struct pi_device cl_device;
       |     struct pi_cluster_conf cl_configuration;
       |     pi_cluster_conf_init(&cl_configuration);
       |     cl_configuration.id = 0;
       |     pi_open_from_conf(&cl_device, &cl_configuration);
       |     if(pi_cluster_open(&cl_device))
       |     {
       |         printf("Cluster open failed\\n");
       |         pmsis_exit(-1);
       |     }
       |
       |     struct cluster_params* cl_params = pmsis_l2_malloc(sizeof(struct cluster_params));
       |     memset(cl_params, 0, sizeof(struct cluster_params));
       |     cl_params->n1 = IMG_LINES;
       |     cl_params->n2 = IMG_COLS;
       |     cl_params->e3 = ImageIn_L2;
       |     cl_params->output = ImageOut_L2;
       |     cl_params->e4 = G_X;
       |     cl_params->e5 = G_Y;
       |
       |     struct pi_cluster_task * cl_task = pmsis_l2_malloc(sizeof(struct pi_cluster_task));
       |     memset(cl_task, 0, sizeof(struct pi_cluster_task));
       |     cl_task->entry = cluster_entry_point;
       |     cl_task->arg = (void*)cl_params;
       |     cl_task->stack_size = (uint32_t) STACK_SIZE;
       |
       |     pi_cluster_send_task_to_cl(&cl_device, cl_task);
       |
       |     char *out_image_file_name = "img_out.ppm";
       |     char path_to_out_image[50];
       |     sprintf(path_to_out_image, "../../../%s", out_image_file_name);
       |     printf("Path to output image: %s\\n", path_to_out_image);
       |     WriteImageToFile(path_to_out_image, IMG_COLS, IMG_LINES, 1, ImageOut_L2, GRAY_SCALE_IO);
       |
       |     pi_cluster_close(&cl_device);
       |
       |     printf("Cluster closed, over and out\\n");
       |
       |     pmsis_exit(0);
       | }
       |
       | int main(int argc, char *argv[])
       | {
       |     printf("\\n\\n\\t *** Sobel Filter (RISE) ***\\n\\n");
       |     return pmsis_kickoff((void *) sobel_filter_main);
       | }
       |
       |""".stripMargin

  /**
    * First CLI parameter is path to the generated code output file
    * */
  def main(args: Array[String]) = {
    execute(code, args.head)
  }

  /**
    * TODO: Integrate with the GAP8 execution management
    * */
  def execute(code: String, path: String): Unit= {
    writeToPath(path, code)
  }
}
