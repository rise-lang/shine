package apps

import cameraPipe._
import util._
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.primitives._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.predicate
import elevate.rise.Rise
import elevate.rise.rules._
import elevate.rise.strategies.normalForm._
import elevate.rise.rules.traversal._
// import elevate.rise.strategies.traversal._
import elevate.rise.rules.movement._
import elevate.rise.rules.algorithmic._
// import elevate.rise.strategies.predicate._

class cameraPipeCheck extends shine.test_util.TestsWithExecutor {
  val N = 121
  val M = 160

  // test values are taken from Halide
  val color_temp = 3700.0f
  val gamma = 2.0f
  val contrast = 50.0f
  val sharpen_strength = 1.0f
  val black_level = 25
  val white_level = 1023

  val matrix_3200: Array[Float] = Array(
    1.6697f, -0.2693f, -0.4004f, -42.4346f,
    -0.3576f, 1.0615f, 1.5949f, -37.1158f,
    -0.2175f, -1.8751f, 6.9640f, -26.6970f
  )
  val matrix_7000: Array[Float] = Array(
    2.2997f, -0.4478f, 0.1706f, -39.0923f,
    -0.3826f, 1.5906f, -0.2080f, -25.4311f,
    -0.0888f, -0.7344f, 2.2832f, -20.0826f
  )

  val cHeader =
    s"""
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

int16_t min_i16(int16_t a, int16_t b) {
  return (a < b) ? a : b;
}
int16_t max_i16(int16_t a, int16_t b) {
  return (a > b) ? a : b;
}
int16_t clamp_i16(int16_t v, int16_t l, int16_t h) {
  return min_i16(max_i16(v, l), h);
}
uint16_t abs_diff_i16(int16_t a, int16_t b) {
  if (a > b) {
    return (uint16_t) a - (uint16_t) b;
  } else {
    return (uint16_t) b - (uint16_t) a;
  }
}
float min_f32(float a, float b) {
  return (a < b) ? a : b;
}
float max_f32(float a, float b) {
  return (a > b) ? a : b;
}
float clamp_f32(float v, float l, float h) {
  return min_f32(max_f32(v, l), h);
}
#define pow_f32 powf
"""

  val ctyToFormat: String => String = {
    case "uint16_t" => "%hu"
    case "int16_t" => "%hd"
    case "uint8_t" => "%hhu"
  }

  def read_csv(cty: String): String = s"""
void read_csv_${cty}(size_t n, ${cty}* buf, const char* path) {
  FILE* f = fopen(path, "r");

  for (size_t i = 0; i < n; i++) {
    if (fscanf(f, " ${ctyToFormat(cty)}", &buf[i]) != 1) {
      fprintf(stderr, "could not read csv file\\n");
      exit(1);
    }
  }

  fclose(f);
}
"""

  def check(
    lowered: Rise, callCFun: String => String,
    inputSize: Int, inputCty: String, inputPath: String,
    outputSize: Int, outputCty: String, outputPath: String,
    delta: Int,
  ): Unit = {
    val prog = printTime("codegen", gen.CProgram(lowered))
    val testCode = s"""
${cHeader}

${prog.code}

${read_csv(inputCty)}
${if (inputCty != outputCty) read_csv(outputCty) else ""}

int main(int argc, char** argv) {
  ${inputCty}* input = malloc(${inputSize} * sizeof(${inputCty}));
  ${outputCty}* gold = malloc(${outputSize} * sizeof(${outputCty}));
  ${outputCty}* output = malloc(${outputSize} * sizeof(${outputCty}));

  read_csv_${inputCty}(${inputSize}, input, "golds/camera_pipe/${inputPath}");
  read_csv_${outputCty}(${outputSize}, gold, "golds/camera_pipe/${outputPath}");

  ${callCFun(prog.function.name)}

  size_t differences = 0;
  size_t errors = 0;
  for (int i = 0; i < ${outputSize}; i++) {
    int64_t d = labs((int64_t)(gold[i]) - (int64_t)(output[i]));
    if (d > 0) differences++;
    if (d > ${delta}) {
      if (errors < 10) {
        fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      }
      errors++;
    }
  }

  if (differences > 0) {
    fprintf(stderr, "WARNING: %zu differences\\n", differences);
  }

  int exit_status = 0;
  if (errors > 0) {
    fprintf(stderr, "ERROR: %zu errors\\n", errors);
    exit_status = 1;
  }

  free(input);
  free(gold);
  free(output);
  return exit_status;
}
"""
    printTime("execute", util.Execute(testCode))
  }

  def dotPrintTmp(name: String, e: Rise): Unit = {
    val generateDot = (e: Rise) => {
      rise.core.dotPrinter.generateDotString(e,
        printTypes = false,
        inlineLambdaIdentifier = true,
        applyNodes = false)
    }
    rise.core.dotPrinter.exprToDot("/tmp", name, e, generateDot)
  }

  test("hot pixel suppression passes checks") {
    val typed = printTime("infer", infer(hot_pixel_suppression))
    println(s"hot pixel suppression: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;`
      repeatNTimes(2, oncetd(lowering.mapSeq))
    val lowered = printTime("lower", lower(typed).get)
    println(s"lowered: ${lowered}")
    check(
      lowered, fName => s"${fName}(output, ${N*2 + 4}, ${M*2 + 4}, input);",
      (N*2 + 4) * (M*2 + 4), "int16_t", "shifted.dump",
      N*2 * M*2, "int16_t", "denoised.dump",
      0
    )
  }

  test("deinterleave passes checks") {
    val typed = printTime("infer", infer(nFun(h => nFun(w =>
      deinterleave(h)(w) >> mapSeq(mapSeq(mapSeq(fun(x => x))))
    ))))
    println(s"deinterleave: ${typed.t}")
    /* TODO
    val lower: Strategy[Rise] =
    val lowered = printTime(lower(typed).get)
     */
    val lowered = typed
    println(s"lowered: ${lowered}")
    check(
      lowered, fName => s"${fName}(output, $N, $M, input);",
      N*2 * M*2, "int16_t", "denoised.dump",
      4 * N * M, "int16_t", "deinterleaved.dump",
      0
    )
  }

  def checkDemosaic(lowered: Rise): Unit = {
    check(
      lowered, fName => s"${fName}(output, $N, $M, input);",
      4 * N * M, "int16_t", "deinterleaved.dump",
      3 * (2*N - 4) * (2*M - 4), "int16_t", "demosaiced.dump",
      0
    )
  }

  test("demosaic passes checks") {
    val typed = printTime("infer", infer(nFun(h => nFun(w =>
      demosaic(h)(w) >> mapSeqUnroll(mapSeq(mapSeq(fun(x => x))))
    ))))
    println(s"demosaic: ${typed.t}")
    // TODO
    val lower: Strategy[Rise] = strategies.basic.id()
    val lowered = printTime("lower", lower(typed).get)
    checkDemosaic(lowered)
  }

  test("demosaic passes checks with reordering") {
    val typed = printTime("infer", infer(nFun(h => nFun(w =>
      demosaic(h)(w) >> transpose >> map(transpose) >>
      split(2) >> mapSeq(mapSeqUnroll(
        mapSeq(
          mapSeqUnroll(fun(x => x)))
      )) >> join >> map(transpose) >> transpose
    ))))
    println(s"demosaic: ${typed.t}")
    // TODO
    val lower: Strategy[Rise] = strategies.basic.id()
    val lowered = printTime("lower", lower(typed).get)
    checkDemosaic(lowered)
  }

  test("demosaic passes checks with circular buffers") {
    val typed = printTime("infer", infer(demosaic))

    var nRewrite = 0
    def rewrite(e: Rise, s: Strategy[Rise]): Rise = {
      nRewrite += 1
      val r = printTime(s"rewrite $nRewrite", s(e).get)
      // dotPrintTmp(s"demosaic$nRewrite", r)
      r
    }

    // 1. normalize a bit
    val demosaic1 = rewrite(typed, normalize.apply(gentleBetaReduction))

    def gentleFmap(s: Strategy[Rise]): Strategy[Rise] =
      mapFusion `;` function(argument(body(s)) `;` mapLastFission)

    // 2. push take/drop towards input
    val demosaic2 = rewrite(demosaic1, normalize.apply(
      gentleBetaReduction <+ etaReduction <+
      takeAll <+ dropNothing <+ mapIdentity <+
      takeBeforeMap <+ dropBeforeMap <+
      gentleFmap(takeBeforeMap <+ dropBeforeMap) <+
      takeInZip <+ dropInZip <+
      takeInSelect <+ dropInSelect <+
      (mapFusion `;`
       function(argument(body(
         normalize.apply(gentleBetaReduction) `;`
         (takeInZip <+ dropInZip)
       ))) `;`
       fBeforeZipMap
      ) <+ mapFBeforeSlide
    ))

    // 3. push line mapping towards output
    val demosaic3 = rewrite(demosaic2, body(body(body(
      function(body(function(body(
        repeat(oncebu(
          gentleBetaReduction <+ etaReduction <+ mapFusion <+ mapOutsideZip
        )) `;`
        // generate/select 1.1
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var exprFound: Rise = null
              function(argument(argument(argument({ expr =>
                exprFound = expr
                Success(expr)
              })))).apply(x).flatMapSuccess(
                argument(argument(
                  argument(argument(slideAfter2) `;` dropBeforeMap) `;` takeBeforeMap `;`
                  argument(zipSndAfter(exprFound))
                ) `;` mapFusion `;` mapFusion)
              )
            })) `;`
            mapOutsideGenerateSelect
          ))
        ) `;`
        // generate/select 1.2
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var rightExpr: Rise = null
              argument(argument(argument(function(argument({ expr =>
                rightExpr = expr
                Success(expr)
              }))))).apply(x).flatMapSuccess({ x =>
                var leftExpr: Rise = null
                function(argument(argument(
                  zipSame `;` argument(
                  zipSwap `;` argument(
                  zipRotate `;` argument(
                    function(argument(zipSame)) `;`
                    argument(
                      function(argument({ expr =>
                        leftExpr = expr
                        zipSndAfter(rightExpr)(expr)
                      })) `;`
                      argument(mapIdentityAfter) `;` mapOutsideZip
                    ) `;` mapOutsideZip
                  ) `;` mapFusion) `;` mapFusion) `;` mapFusion
                ) `;` mapFusion)).apply(x).flatMapSuccess(
                argument(argument(
                  argument(
                    function(argument(zipFstAfter(leftExpr))) `;`
                    argument(mapIdentityAfter) `;` mapOutsideZip
                  ) `;`
                  function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` mapFusion)
                )
              })
            })) `;` mapOutsideGenerateSelect
          ))
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // generate/select 1
        oncetd(function(predicate.isEqualTo(generate)) `;`
          argument(body({ x =>
            var rightExpr: Rise = null
            argument(argument(argument(function(argument({ expr =>
              rightExpr = expr
              Success(expr)
            }))))).apply(x).flatMapSuccess({ x =>
              var leftExpr: Rise = null
              function(argument(
                argument(
                  function(argument(
                    takeBeforeDrop `;` argument(takeInSlide) `;` dropBeforeMap
                  )) `;`
                  argument(argument(takeInSlide)) `;`
                  argument(function(argument({ expr =>
                    leftExpr = expr
                    zipSndAfter(rightExpr)(expr)
                  })) `;` mapOutsideZip) `;` mapOutsideZip
                ) `;` mapFusion
              )).apply(x).flatMapSuccess(
                argument(argument(
                  argument(
                    function(argument(zipFstAfter(leftExpr))) `;`
                    argument(mapIdentityAfter) `;` mapOutsideZip
                  ) `;`
                  function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` mapFusion)
              )
            })
          })) `;` mapOutsideGenerateSelect
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // generate/select 2.1
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var rightExpr: Rise = null
              argument(argument({ expr =>
                rightExpr = expr
                Success(expr)
              })).apply(x).flatMapSuccess({ x =>
                var leftExpr: Rise = null
                function(argument(argument({ expr =>
                  leftExpr = expr
                  zipSndAfter(rightExpr)(expr)
                }) `;` mapFusion)).apply(x).flatMapSuccess(
                  argument(argument(
                    zipFstAfter(leftExpr)
                  ) `;` mapFusion)
                )
              })
            })) `;` mapOutsideGenerateSelect
          ))
        ) `;`
        // generate/select 2.2
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var rightExpr: Rise = null
              argument(argument({ expr =>
                rightExpr = expr
                Success(expr)
              })).apply(x).flatMapSuccess({ x =>
                var leftExpr: Rise = null
                function(argument(argument({ expr =>
                  leftExpr = expr
                  zipSndAfter(rightExpr)(expr)
                }) `;` mapFusion)).apply(x).flatMapSuccess(
                  argument(argument(
                    zipFstAfter(leftExpr)
                  ) `;` mapFusion)
                )
              })
            })) `;` mapOutsideGenerateSelect
          ))
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // generate/select 2
        oncetd(function(predicate.isEqualTo(generate)) `;`
          argument(body({ x =>
            var rightExpr: Rise = null
            argument(argument({ expr =>
              rightExpr = expr
              Success(expr)
            })).apply(x).flatMapSuccess({ x =>
              var leftExpr: Rise = null
              function(argument(argument({ expr =>
                leftExpr = expr
                zipSndAfter(rightExpr)(expr)
              }) `;` mapFusion)).apply(x).flatMapSuccess(
                argument(argument(
                  zipFstAfter(leftExpr)
                ) `;` mapFusion)
              )
            })
          })) `;` mapOutsideGenerateSelect
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // generate/select 3.1
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var rightExpr: Rise = null
              argument(argument(
                zipSame `;` argument(
                zipSwap `;` argument(
                zipRotate `;` argument(
                  function(argument(zipSame)) `;`
                  argument(function(argument({ expr =>
                    rightExpr = expr
                    Success(expr)
                  }))) `;`
                  argument(mapIdentityAfter) `;` mapOutsideZip
                ) `;` mapFusion) `;` mapFusion) `;` mapFusion
              ) `;` mapFusion).apply(x).flatMapSuccess({ x =>
                var leftExpr: Rise = null
                function(argument(argument(
                  argument(
                    function(argument({ expr =>
                      leftExpr = expr
                      zipSndAfter(rightExpr)(expr)
                    })) `;`
                    argument(mapIdentityAfter) `;` mapOutsideZip
                  ) `;`
                  function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` mapFusion)).apply(x).flatMapSuccess(
                  argument(argument(
                    argument(
                      function(argument(zipFstAfter(leftExpr))) `;`
                      argument(mapIdentityAfter) `;` mapOutsideZip
                    ) `;`
                    function(argument(mapIdentityAfter)) `;` mapOutsideZip
                  ) `;` mapFusion)
                )
              })
            })) `;` mapOutsideGenerateSelect
          ))
        ) `;`
        // generate/select 3.2
        oncetd(function(predicate.isEqualTo(generate)) `;`
          oncetd(one(function(predicate.isEqualTo(generate)) `;`
            argument(body({ x =>
              var rightExpr: Rise = null
              argument(argument(argument({ expr =>
                rightExpr = expr
                Success(expr)
              }))).apply(x).flatMapSuccess(
              function(argument(argument(
                takeBeforeDrop `;`
                argument(argument(slideAfter2) `;` takeBeforeMap) `;` dropBeforeMap `;`
                argument(zipSndAfter(rightExpr)) `;` mapFusion
              ) `;` mapFusion))
              )
            })) `;` mapOutsideGenerateSelect
          ))
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // generate/select 3
        oncetd(function(predicate.isEqualTo(generate)) `;`
          argument(body({ x =>
            var leftExpr: Rise = null
            function(argument(argument(
              argument(function(argument({ expr =>
                leftExpr = expr
                Success(expr)
              })))
            ))).apply(x).flatMapSuccess({ x =>
              var rightExpr: Rise = null
              argument(argument(
                function(argument(dropBeforeTake `;` argument(dropInSlide) `;` takeBeforeMap)) `;`
                argument(
                  argument(dropInSlide) `;`
                  function(argument({ expr =>
                    rightExpr = expr
                    zipFstAfter(leftExpr)(expr)
                  })) `;`
                  mapOutsideZip
                ) `;` mapOutsideZip
              ) `;` mapFusion).apply(x).flatMapSuccess(
                function(argument(argument(
                  argument(
                    function(argument(zipSndAfter(rightExpr))) `;`
                    argument(mapIdentityAfter) `;` mapOutsideZip
                  ) `;`
                  function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` mapFusion))
              )
              })
          })) `;` mapOutsideGenerateSelect
        ) `;`
        normalize.apply(gentleBetaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion) `;`
        // makeArray
        // makeArray .1
        { x =>
          var expr1: Rise = null
          function(function(argument(argument(argument(
            // zip ordering
            argument(zipRotate) `;`
            zipSwap `;`
            argument(argument(mapIdentityAfter) `;` mapOutsideZip `;` argument(zipRotate)) `;`
            mapFusion `;` mapFusion `;`
            argument(
              argument(
                zipSwap `;` argument(
                  argument(
                    function(argument(zipSwap)) `;`
                    argument(mapIdentityAfter) `;` mapOutsideZip `;` argument(zipRotate) `;`
                    mapFusion `;`
                    argument(argument(zipSwap)) `;`
                    argument(function(argument(mapIdentityAfter)) `;` mapOutsideZip) `;`
                    mapFusion
                  ) `;`
                  function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` mapFusion
              ) `;`
              function(argument(mapIdentityAfter)) `;` mapOutsideZip
            ) `;` mapFusion `;`
            argument(
              // zip branch unification
              argument(
                function(argument({ expr =>
                  expr1 = expr
                  Success(expr)
                })) `;`
                argument(
                  argument(
                    argument(
                      argument(slideAfter2) `;` dropBeforeMap `;` argument(dropInSlide) `;` mapFusion
                    ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
                  ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
                ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
              ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
            )
          ) `;` repeat(mapFusion))))).apply(x).flatMapSuccess(
          function(argument(argument(argument(
            // zip ordering
            zipRotateRight `;` argument(
              argument(
                argument(zipSwap) `;`
                function(argument(mapIdentityAfter)) `;` mapOutsideZip `;`
                argument(zipRotateLeft `;` argument(
                  function(argument(zipSwap)) `;`
                  argument(mapIdentityAfter) `;` mapOutsideZip `;`
                  argument(zipRotateRight)
                ))
              ) `;`
              argument(repeatNTimes(3, mapFusion)) `;`
              function(argument(mapIdentityAfter)) `;` mapOutsideZip
            ) `;` mapFusion `;`
            argument(
              // zip branch unification
              argument(
                argument(
                  argument(
                    argument(slideAfter2) `;` dropBeforeMap `;` argument(dropInSlide) `;` mapFusion
                  ) `;`
                  function(argument(
                    argument(slideAfter2) `;` takeBeforeMap `;` argument(takeInSlide) `;` mapFusion
                  )) `;` mapOutsideZip
                ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
              ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
            )
          ) `;` repeatNTimes(2, mapFusion)))) `;`
          { x: Rise =>
            var expr2: Rise = null
            argument(argument(argument(
              // zip ordering
              argument(
                function(argument(
                  zipRotateRight `;` argument(argument(zipSwap))
                )) `;`
                argument(mapIdentityAfter) `;` mapOutsideZip `;`
                argument(zipRotateRight `;` argument(argument(
                  argument(mapIdentityAfter) `;` mapOutsideZip `;`
                  argument(zipRotateRight)
                )))
              ) `;`
              argument(mapFusion) `;` function(argument(mapIdentityAfter)) `;`
              mapOutsideZip `;` argument(
                zipRotateLeft `;` argument(
                  function(argument(zipSwap))
                ) `;`
                argument(
                  argument(mapFusion) `;` mapOutsideZip `;`
                  argument(zipRotateRight)
                )
              )
            ) `;` repeatNTimes(4, mapFusion) `;`
            argument(
              // zip branch unification
              argument(
                function(argument({ expr =>
                  expr2 = expr
                  Success(expr)
                })) `;`
                argument(
                  argument(
                    function(argument(
                      argument(slideAfter2) `;` takeBeforeMap `;` argument(takeInSlide) `;` mapFusion
                    )) `;` argument(mapIdentityAfter) `;` mapOutsideZip
                  ) `;` function(argument(dropBeforeTake `;` mapIdentityAfter)) `;` mapOutsideZip
                ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
              ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
            ) `;` mapFusion
          )).apply(x).flatMapSuccess(
          // zip branch injection
          function(function(argument(argument(argument(
            argument(
              argument(zipFstAfter(expr2)) `;`
              function(argument(mapIdentityAfter)) `;` mapOutsideZip
            ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
          ) `;` mapFusion)))) `;`
          function(argument(argument(argument(
            argument(
              zipFstAfter(expr1) `;`
              argument(
                argument(zipFstAfter(expr2)) `;`
                function(argument(mapIdentityAfter)) `;` mapOutsideZip
              ) `;` mapFusion
            ) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
          ) `;` mapFusion))) `;`
          argument(argument(argument(
            argument(zipFstAfter(expr1)) `;` function(argument(mapIdentityAfter)) `;` mapOutsideZip
          ) `;` mapFusion))
          )}
          )
        } `;`
        fOutsideMakeArray `;` argument(
          mapOutsideMakeArray `;` argument(function(argument(
            normalize.apply(
              betaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion <+
              idxReduction <+ fstReduction <+ sndReduction
            )
          )))
        )
      ))))
    ))))

    // 4. line mapping input as single slide
    val demosaic4 = rewrite(demosaic3, body(body(body(
      function(body(function(body(
        argument(argument(argument(
          argument(
            argument(
              argument(
                argument(slideOutsideZip) `;`
                function(argument(
                  argument(
                    argument(slideAfter2) `;`
                    dropBeforeMap `;` argument(dropInSlide) `;` mapFusion
                  ) `;` takeBeforeMap `;` argument(takeBeforeSlide)
                )) `;` mapOutsideZip `;` argument(slideOutsideZip) `;` mapFusion
              ) `;`
              function(argument(takeBeforeSlide `;` mapIdentityAfter)) `;`
              mapOutsideZip `;` argument(slideOutsideZip) `;` mapFusion
            ) `;`
            function(argument(dropBeforeSlide `;` mapIdentityAfter)) `;`
            mapOutsideZip `;` argument(slideOutsideZip) `;` mapFusion
          ) `;`
          function(argument(
            argument(
              argument(slideAfter2) `;`
              dropBeforeMap `;` argument(dropInSlide) `;` mapFusion
            ) `;` takeBeforeMap `;` argument(takeBeforeSlide)
          )) `;` mapOutsideZip `;` argument(slideOutsideZip) `;` mapFusion
        ) `;` mapFusion))
      ))))
    ))))

    // 5. lowering with slideSeq
    val demosaic5 = rewrite(demosaic4, body(body(body(
      function(body(function(body(
        argument(argument(
          argument(function(oncetd(lowering.slideSeq(SlideSeq.Indices, {
            val (f, s, w) = (mapFst, mapSnd, mapSeq(fun(x => x)))
            // TODO: this does not work for code generation
            f(w) >> s(f(w) >> s(f(w) >> s(f(w) >> s(f(w) >> s(w)))))
          })))) `;`
          normalize.apply(gentleBetaReduction) `;`
          slideSeqFusion `;`
          // TODO: use proper rewriting to achieve this
          function(argument(body({ expr =>
            Success(
              expr |> transpose >> map(transpose) >>
              // 2 bands of y. all x. rgb channels.
              mapSeqUnroll(mapSeq(mapSeqUnroll(fun(x => x)))) >>
              map(transpose) >> transpose
            )
          }))) `;`
          normalize.apply(
            betaReduction <+ etaReduction <+ removeTransposePair <+ mapFusion <+
            idxReduction <+ fstReduction <+ sndReduction
          )
        ))
      ))))
    ))))

    // TODO: check the generated code and if the loop nests are similar to:
    // demosaic(h)(w) >> transpose >> map(transpose) >>
    //   split(2) >> mapSeq(mapSeqUnroll(
    //     mapSeq(
    //       mapSeqUnroll(fun(x => x)))
    // )) >> join >> map(transpose) >> transpose
    checkDemosaic(demosaic5)
  }

  test("color correction passes checks") {
    val typed = printTime("infer", infer(color_correct))
    println(s"color correction: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;`
      repeatNTimes(2, oncetd(lowering.mapSeq)) `;`
      oncetd(lowering.mapSeqUnroll)
    val lowered = printTime("lower", lower(typed).get)
    println(s"lowered: ${lowered}")
    // TODO: investigate output difference of 1
    check(
      lowered, fName => s"""
  float matrix_3200[3 * 4] = { ${matrix_3200.mkString(", ")} };
  float matrix_7000[3 * 4] = { ${matrix_7000.mkString(", ")} };

  ${fName}(output,
    2*$N - 4, 2*$M - 4, 3, 4,
    input, matrix_3200, matrix_7000, ${color_temp});
""",
      3 * (2*N - 4) * (2*M - 4), "int16_t", "demosaiced.dump",
      3 * (2*N - 4) * (2*M - 4), "int16_t", "corrected.dump",
      1
    )
  }

  test("apply curve passes checks") {
    val typed = printTime("infer", infer(apply_curve))
    println(s"apply curve: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;`
      repeatNTimes(3, oncetd(lowering.mapSeq))
    val lowered = printTime("lower", lower(typed).get)
    println(s"lowered: ${lowered}")
    check(
      lowered, fName => s"""
${fName}(output, 2*$N - 4, 2*$M - 4,
  input, ${gamma}, ${contrast}, ${black_level}, ${white_level});
""",
      3 * (2*N - 4) * (2*M - 4), "int16_t", "corrected.dump",
      3 * (2*N - 4) * (2*M - 4), "uint8_t", "curved.dump",
      0
    )
  }

  test("sharpen passes checks") {
    val typed = printTime("infer", infer(nFun(h => nFun(w => fun(
      (3`.`h`.`w`.`u8) ->: f32 ->: (3`.`(h-2)`.`(w-2)`.`u8)
    )((input, strength) =>
      sharpen(h)(w)(input)(strength) |> mapSeq(mapSeq(mapSeq(fun(x => x))))
    )))))
    println(s"sharpen: ${typed.t}")
    val lower: Strategy[Rise] = strategies.basic.id()
    val lowered = printTime("lower", lower(typed).get)
    println(s"lowered: ${lowered}")
    check(
      lowered, fName => s"""
${fName}(output, 2*$N - 4, 2*$M - 4, input, ${sharpen_strength});
""",
      3 * (2*N - 4) * (2*M - 4), "uint8_t", "curved.dump",
      3 * (2*N - 6) * (2*M - 6), "uint8_t", "sharpened.dump",
      0
    )
  }

  // TODO: read the same input size as Halide
  ignore("camera pipe passes checks") {
    val typed = printTime("infer", infer(camera_pipe))
    check(
      typed, fName => s"""
  float matrix_3200[3 * 4] = { ${matrix_3200.mkString(", ")} };
  float matrix_7000[3 * 4] = { ${matrix_7000.mkString(", ")} };

  ${fName}(output, $N, $M, 3, 4,
    input, matrix_3200, matrix_7000, ${color_temp},
    ${gamma}, ${contrast}, ${black_level}, ${white_level},
    ${sharpen_strength});
""",
      // the shifted dump is the same as the input dump
      (2*N + 42) * (2*M + 26), "uint16_t", "shifted.dump",
      3 * (2*N - 6) * (2*M - 6), "uint8_t", "sharpened.dump",
      0
    )
  }

  test("type inference") {
    def assertClosedT(e: rise.core.Expr, t: Type): Unit = {
      val typed = infer(e)
      assert(typed.t == t)
      assert(IsClosedForm(typed))
    }

    assertClosedT(avg(i16)(i32), i16 ->: i16 ->: i16)
    assertClosedT(blur121(i16)(i32), (3`.`i16) ->: i16)

    assertClosedT(
      nFun(h => nFun(w => fun(
        (h`.`w`.`2`.`i16) ->: (h`.`w`.`i16)
      )(a =>
        interpolate(Image(0, w, 0, h, a)).expr
      ))),
      nFunT(h => nFunT(w => (h`.`w`.`2`.`i16) ->: (h`.`w`.`i16)))
    )

    assertClosedT(
      nFun(h => nFun(w => fun(
        (h`.`w`.`2`.`i16) ->: (h`.`w`.`u16)
      )(a =>
        pointAbsDiff(Image(0, w, 0, h, a)).expr
      ))),
      nFunT(h => nFunT(w => (h`.`w`.`2`.`i16) ->: (h`.`w`.`u16)))
    )

    val cameraPipeT = (1968`.`2592`.`u16) ->:
      (3`.`4`.`f32) ->: (3`.`4`.`f32) ->: f32 ->:
      f32 ->: f32 ->: int ->: int ->:
      f32 ->:
      (3`.`1920`.`2560`.`u8)
    assertClosedT(
      implN(h => implN(w => camera_pipe(h)(w)(3)(4))) :: cameraPipeT,
      cameraPipeT
    )
  }
}
