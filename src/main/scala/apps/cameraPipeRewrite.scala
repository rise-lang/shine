package apps

import util.printTime
import rise.core._
import rise.core.TypedDSL._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.predicate._
import elevate.rise._
import elevate.rise.rules._
import elevate.rise.rules.movement._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.traversal._

object cameraPipeRewrite {
  val dotPrint = true
  def dotPrintTmp(name: String, r: RewriteResult[Rise]): Unit = r match {
    case Success(e) if dotPrint =>
      val generateDot = (e: Rise) => {
        rise.core.dotPrinter.generateDotString(e,
          printTypes = false,
          inlineLambdaIdentifier = true,
          applyNodes = false)
      }
      rise.core.dotPrinter.exprToDot("/tmp", name, e, generateDot)
    case Success(_) =>
      println(s"not generating $name")
    case _ =>
  }

  private def rewriteSteps(steps: Seq[Strategy[Rise]]): Strategy[Rise] = a => {
    var nRewrite = 0
    steps.foldLeft[RewriteResult[Rise]](Success(a))({ case (r, s) =>
      r.flatMapSuccess { e =>
        nRewrite += 1
        val result = printTime(s"rewrite $nRewrite", s(e))
        dotPrintTmp(s"rewrite$nRewrite", result)
        result
      }
    })
  }

  private def gentleFmap(s: Strategy[Rise]): Strategy[Rise] =
    mapFusion `;` function(argument(body(s)) `;` mapLastFission)

  def demosaicCircularBuffers: Strategy[Rise] = {
    rewriteSteps(Seq(
      // 1. normalize a bit
      normalize.apply(gentleBetaReduction),

      // 2. push take/drop towards input
      normalize.apply(
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
      ),

      // 3. push line mapping towards output
      body(body(body(
        function(body(function(body(
          repeat(oncebu(
            gentleBetaReduction <+ etaReduction <+ mapFusion <+ mapOutsideZip
          )) `;`
            // generate/select 1.1
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
                argument(body({ x =>
                  var exprFound: Rise = null
                  function(argument(argument(argument({ expr =>
                    exprFound = expr
                    Success(expr)
                  })))).apply(x).flatMapSuccess(
                    argument(argument(
                      argument(argument(slideAfter2) `;` dropBeforeMap) `;`
                      takeBeforeMap `;` argument(zipSndAfter(exprFound))
                    ) `;` mapFusion `;` mapFusion)
                  )
                })) `;`
                mapOutsideGenerateSelect
              ))
            ) `;`
            // generate/select 1.2
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
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
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // generate/select 1
            oncetd(function(isEqualTo(DSL.generate)) `;`
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
                        takeBeforeDrop `;` argument(takeInSlide) `;`
                        dropBeforeMap
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
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // generate/select 2.1
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
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
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
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
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // generate/select 2
            oncetd(function(isEqualTo(DSL.generate)) `;`
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
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // generate/select 3.1
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
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
            oncetd(function(isEqualTo(DSL.generate)) `;`
              oncetd(one(function(isEqualTo(DSL.generate)) `;`
                argument(body({ x =>
                  var rightExpr: Rise = null
                  argument(argument(argument({ expr =>
                    rightExpr = expr
                    Success(expr)
                  }))).apply(x).flatMapSuccess(
                    function(argument(argument(
                      takeBeforeDrop `;`
                        argument(argument(slideAfter2) `;` takeBeforeMap) `;`
                        dropBeforeMap `;`
                        argument(zipSndAfter(rightExpr)) `;` mapFusion
                    ) `;` mapFusion))
                  )
                })) `;` mapOutsideGenerateSelect
              ))
            ) `;`
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // generate/select 3
            oncetd(function(isEqualTo(DSL.generate)) `;`
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
                    function(argument(
                      dropBeforeTake `;` argument(dropInSlide) `;` takeBeforeMap
                    )) `;`
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
            normalize.apply(gentleBetaReduction <+ etaReduction
              <+ removeTransposePair <+ mapFusion) `;`
            // makeArray
            // makeArray .1
            { x =>
              var expr1: Rise = null
              function(function(argument(argument(argument(
                // zip ordering
                argument(zipRotate) `;`
                  zipSwap `;`
                  argument(argument(mapIdentityAfter) `;`
                    mapOutsideZip `;` argument(zipRotate)) `;`
                  mapFusion `;` mapFusion `;`
                  argument(
                    argument(
                      zipSwap `;` argument(
                        argument(
                          function(argument(zipSwap)) `;`
                            argument(mapIdentityAfter) `;` mapOutsideZip `;`
                            argument(zipRotate) `;` mapFusion `;`
                            argument(argument(zipSwap)) `;`
                            argument(function(argument(mapIdentityAfter)) `;`
                              mapOutsideZip) `;`
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
                              argument(slideAfter2) `;` dropBeforeMap `;`
                              argument(dropInSlide) `;` mapFusion
                            ) `;` function(argument(mapIdentityAfter)) `;`
                              mapOutsideZip
                          ) `;` function(argument(mapIdentityAfter)) `;`
                            mapOutsideZip
                        ) `;` function(argument(mapIdentityAfter)) `;`
                        mapOutsideZip
                    ) `;` function(argument(mapIdentityAfter)) `;`
                      mapOutsideZip
                  )
              ) `;` repeat(mapFusion))))).apply(x).flatMapSuccess(
                function(argument(argument(argument(
                  // zip ordering
                  zipRotateRight `;` argument(
                    argument(
                      argument(zipSwap) `;`
                        function(argument(mapIdentityAfter)) `;`
                        mapOutsideZip `;`
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
                            argument(slideAfter2) `;` dropBeforeMap `;`
                            argument(dropInSlide) `;` mapFusion
                          ) `;`
                            function(argument(
                              argument(slideAfter2) `;` takeBeforeMap `;`
                              argument(takeInSlide) `;` mapFusion
                            )) `;` mapOutsideZip
                        ) `;` function(argument(mapIdentityAfter)) `;`
                          mapOutsideZip
                      ) `;` function(argument(mapIdentityAfter)) `;`
                        mapOutsideZip
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
                        argument(mapFusion) `;`
                        function(argument(mapIdentityAfter)) `;`
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
                                  argument(slideAfter2) `;` takeBeforeMap `;`
                                  argument(takeInSlide) `;` mapFusion
                                )) `;` argument(mapIdentityAfter) `;`
                                  mapOutsideZip
                              ) `;` function(argument(
                                dropBeforeTake `;` mapIdentityAfter
                              )) `;` mapOutsideZip
                            ) `;` function(argument(mapIdentityAfter)) `;`
                            mapOutsideZip
                        ) `;` function(argument(mapIdentityAfter)) `;`
                          mapOutsideZip
                      ) `;` mapFusion
                    )).apply(x).flatMapSuccess(
                      // zip branch injection
                      function(function(argument(argument(argument(
                        argument(
                          argument(zipFstAfter(expr2)) `;`
                            function(argument(mapIdentityAfter)) `;`
                            mapOutsideZip
                        ) `;` function(argument(mapIdentityAfter)) `;`
                          mapOutsideZip
                      ) `;` mapFusion)))) `;`
                        function(argument(argument(argument(
                          argument(
                            zipFstAfter(expr1) `;`
                              argument(
                                argument(zipFstAfter(expr2)) `;`
                                  function(argument(mapIdentityAfter)) `;`
                                  mapOutsideZip
                              ) `;` mapFusion
                          ) `;` function(argument(mapIdentityAfter)) `;`
                            mapOutsideZip
                        ) `;` mapFusion))) `;`
                        argument(argument(argument(
                          argument(zipFstAfter(expr1)) `;`
                          function(argument(mapIdentityAfter)) `;` mapOutsideZip
                        ) `;` mapFusion))
                    )}
              )
            } `;`
            fOutsideMakeArray `;` argument(
            mapOutsideMakeArray `;` argument(function(argument(
              normalize.apply(
                betaReduction <+ etaReduction <+
                removeTransposePair <+ mapFusion <+
                idxReduction <+ fstReduction <+ sndReduction
              )
            )))
          )
        ))))
      ))),

      // 4. lowering with slideSeq
      {
        import DSL._
        body(body(body(
          function(body(function(body(
            argument(argument(
              function(function(lowering.mapStream)) `;`
                argument(argument(argument(argument(argument(
                  repeatNTimes(2, oncetd(
                    lowering.slideSeq(primitives.SlideSeq.Indices,
                      mapSeq(fun(x => x)))
                  ))
                ))))) `;`
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
                  betaReduction <+ etaReduction <+
                  removeTransposePair <+ mapFusion <+
                  idxReduction <+ fstReduction <+ sndReduction
                )
            ))
          ))))
        )))
      }
    ))
  }

  def letContinuation(s: Strategy[Rise]): Strategy[Rise] =
    function(function(isEqualTo(primitives.Let()()))) `;`
    function(argument(s))

  def afterTopLevel(s: Strategy[Rise]): Strategy[Rise] = p => {
    (body(afterTopLevel(s)) <+
      letContinuation(afterTopLevel(s)) <+
      s)(p)
  }

  def precomputeSharpenStrengthX32: Strategy[Rise] = {
    // |> toMem() |> let(fun(strength_x32 =>
    normalize.apply(gentleBetaReduction) `;`
    afterTopLevel(
      function(argument( // sharpen
        ???
      ))
    )
  }

  def letHoist: Strategy[Rise] = {
    case expr @ App(f, App(App(primitives.Let(), Lambda(x, b)), v)) =>
      Success(let(lambda(untyped(x), typed(f)(b)), v) :: expr.t)
    /*
  case expr @ App(Map(Lambda(y, App(App(Let(), Lambda(x, b)), v)) =>
    expr.t match {
      case FunType(_: DataType, _: DataType) if !contains[Rise](y).apply(v) =>
        Success(let(lambda(untyped(x), lambda(untyped(y), b)), v) :: expr.t)
      case _ => Failure(letHoist)
    }
     */
    case _ => Failure(letHoist)
  }

  def precomputeColorCorrectionMatrix: Strategy[Rise] = {
    normalize.apply(gentleBetaReduction) `;`
    afterTopLevel(
      argument(argument({
        case expr @ App(Lambda(x, color_correct), matrix) =>
          Success(let(lambda(toTDSL(x), color_correct),
            mapSeq(mapSeq(fun(x => x)), matrix)) :: expr.t)
        case _ => Failure(precomputeColorCorrectionMatrix)
      })) `;`
      repeat(oncetd(letHoist))
    )
  }

  def precomputeCurve: Strategy[Rise] = {
    // TODO: apply_curve curve:
    // |> mapSeq(fun(x => x)) |> let(fun(curve =>
    normalize.apply(gentleBetaReduction) `;`
    afterTopLevel(
      argument(function(argument(
        oncetd(
          function(function(isEqualTo(primitives.Idx()()))) `;`
          argument(function(isEqualTo(primitives.Generate()()))) `;`
          argument({ curve =>
            Success(let(fun(x => x),
              mapSeq(fun(x => x), curve)) :: curve.t)
          })
        )
      ))) `;`
      repeat(oncetd(letHoist)) // TODO: not hoisted far enough
    )
  }

  def circularBuffers: Strategy[Rise] = {
    rewriteSteps(Seq(
      normalize.apply(gentleBetaReduction),
      precomputeColorCorrectionMatrix,
      precomputeCurve
      // circular buffer demosaic
    ))
  }
}
