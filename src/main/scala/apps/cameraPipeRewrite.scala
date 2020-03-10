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


  case class depFunction(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ DepApp(f, x) => s(f).mapSuccess(DepApp(_, x)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"depFunction($s)"
  }

  def dropOrTake: Strategy[Rise] = {
    depFunction(isEqualTo(DSL.drop)) <+ depFunction(isEqualTo(DSL.take))
  }

  def fBeforeZipMapFilter(fPredicate: Strategy[Rise]): Strategy[Rise] = {
    function(argument(body( // map lambda
      function(argument( // left zip
        function(fPredicate))) `;`
      argument( // right zip
        function(fPredicate))
    ))) `;`
    fBeforeZipMap
  }

  def takeDropTowardsInput: Strategy[Rise] = {
    normalize.apply(
      gentleBetaReduction <+ etaReduction <+
      takeAll <+ dropNothing <+
      mapFusion <+ mapIdentity <+
      takeBeforeMap <+ dropBeforeMap <+
      takeInZip <+ dropInZip <+
      takeInSelect <+ dropInSelect // <+
      // TODO: think more about following
      // fBeforeZipMap <+ // fBeforeZipMapFilter(dropOrTake) <+
    )
  }

  private def debugS(msg: String) =
    elevate.core.strategies.debug.debug[Rise](msg)
  def printS(msg: String) =
    elevate.core.strategies.debug.print[Rise](msg)
  private val idS = elevate.core.strategies.basic.id[Rise]()

  type Traversal = Strategy[Rise] => Strategy[Rise]

  def select(predicate: Strategy[Rise], a: Strategy[Rise], b: Strategy[Rise])
  : Strategy[Rise] = p => {
    if (predicate(p)) { a(p) } else { b(p) }
  }

  def isAppliedMap: Strategy[Rise] = function(function(isEqualTo(DSL.map)))
  def isAppliedZip: Strategy[Rise] = function(function(isEqualTo(DSL.zip)))

  def anyMapOutsideZip: Strategy[Rise] = {
    // function(function(isEqualTo(DSL.zip))) `;`
    val noMapsInsideZip = not(
      argument(isAppliedMap) <+ function(argument(isAppliedMap)))
    noMapsInsideZip <+ (
      argument(isAppliedMap <+ mapIdentityAfter) `;`
      function(argument(isAppliedMap <+ mapIdentityAfter)) `;`
      mapOutsideZip
    )
  }

  def afterMaps: Traversal = s => {
    select(isAppliedMap, argument(p => afterMaps(s)(p)), s)
  }

  // assumption: program is applied zip, output can be prefixed with a map
  def normalizeZipInput: Strategy[Rise] = {
    def normalizeZipStructure: Strategy[Rise] = { p =>
      val s = anyMapOutsideZip `;` afterMaps(
        `try`(zipRotateRight) `;`
        argument(not(isAppliedZip) <+ normalizeZipStructure) `;`
        afterMaps(anyMapOutsideZip)
      ) `;` repeat(mapFusion)
      s(p)
    }

    def normalizeZipOrder: Strategy[Rise] = { p =>
      val s =
        function(argument(normalizeSingleInput)) `;`
        argument(select(isAppliedZip,
          normalizeZipOrder, normalizeSingleInput
        )) `;`
        anyMapOutsideZip `;` afterMaps(orderLeftZip) `;` repeat(mapFusion)
      s(p)
    }

    def orderLeftZip: Strategy[Rise] = { p =>
      var aNum = -1
      var bNum = -1
      val s = select(argument(isAppliedZip), {
        function(argument(singleInputId(aNum = _))) `;`
        argument(function(argument(singleInputId(bNum = _)))) `;` { p =>
          if (aNum == bNum) {
            (zipRotateLeft `;`
              argument(function(argument(zipSameId)) `;` anyMapOutsideZip)
              )(p)
          } else if (aNum > bNum) {
            (zipRotateLeft `;`
              argument(function(argument(zipSwap)) `;` anyMapOutsideZip) `;`
              mapFusion `;` argument(zipRotateRight) `;`
              afterMaps(argument(orderLeftZip) `;` anyMapOutsideZip)
              )(p)
          } else {
            Success(p)
          }
        }
      }, {
        function(argument(singleInputId(aNum = _))) `;`
        argument(singleInputId(bNum = _)) `;` { p =>
          if (aNum == bNum) {
            zipSameId(p)
          } else if (aNum > bNum) {
            zipSwap(p)
          } else {
            Success(p)
          }
        }
      }) `;` repeat(mapFusion)
      s(p)
    }

    normalizeZipStructure `;`
    afterMaps(normalizeZipOrder) `;`
    repeat(mapFusion)
  }

  // TODO: should be mapAfterSlide?
  def mapAfterSlide: Strategy[Rise] = mapFBeforeSlide

  def normalizeSingleInput: Strategy[Rise] = normalize.apply(
    dropBeforeTake <+ dropBeforeMap <+ takeBeforeMap <+
    mapAfterSlide <+ mapFusion
  )

  def normalizeInput: Strategy[Rise] =
    afterMaps(select(isAppliedZip, normalizeZipInput, normalizeSingleInput)) `;`
    repeat(mapFusion)

  def zipSameId: Strategy[Rise] = {
    // TODO: generalize
    zipSame <+ (
      debugS("zsi0") `;`
      function(argument(
        argument(argument(slideAfter2) `;` dropBeforeMap) `;` takeBeforeMap
      )) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi1")
    ) <+ (
      argument(
        argument(argument(slideAfter2) `;` dropBeforeMap) `;` takeBeforeMap
      ) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi2")
    ) <+ (
      argument(takeInSlide) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi3")
    ) <+ (
      function(argument(dropInSlide)) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi4")
    ) <+ (
      argument(
        takeBeforeDrop `;` argument(takeInSlide) `;` dropBeforeMap
      )`;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi5")
    ) <+ (
      function(argument(
        argument(dropInSlide) `;` takeBeforeMap
      ))`;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi6")
    ) <+ (
      argument(
        argument(slideAfter2) `;` dropBeforeMap `;`
        argument(dropInSlide) `;` mapFusion
      ) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi7")
    ) <+ (
      function(argument(
        argument(slideAfter2) `;` takeBeforeMap `;`
        argument(takeInSlide) `;` mapFusion
      )) `;`
      anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion) `;`
      debugS("zsi8")
    )
  }

  def singleInputId(ret: Int => Unit): Strategy[Rise] = p => {
    import primitives.Idx
    import semantics.IndexData
    import arithexpr.arithmetic.Cst

    var num = -1
    def traverse: Strategy[Rise] = { p =>
      val s = (
        (
          isAppliedMap <+ function(dropOrTake) <+
          function(depFunction(depFunction(isEqualTo(DSL.slide))))
        ) `;` argument(traverse)
      ) <+ {
        case p @ App(
          App(Idx(), Literal(IndexData(Cst(i), _))),
          Identifier(name)
        ) =>
          // FIXME: could go wrong
          num = name.drop(1).toInt - 10000 + i.toInt
          Success(p)
        case p @ Identifier(name) =>
          num = name.drop(1).toInt
          Success(p)
        case _ => Failure(traverse)
      }
      s(p)
    }
    val r = traverse(p)
    ret(num)
    r
  }

  def unifyMapInputs(toMaps: Seq[Traversal]): Strategy[Rise] = p => {
    var ps = Seq[Rise]()
    val normalized = toMaps.foldLeft[RewriteResult[Rise]](Success(p)) {
      case (r, toMap) => r.flatMapSuccess(toMap(
        argument(normalizeInput) `;` repeat(mapFusion) `;`
        argument { e => ps = ps :+ e; Success(e) }
      ))
    }
    var leftPs = Seq[Rise]()
    toMaps.foldLeft(normalized) {
      case (r, toMap) => r.flatMapSuccess(toMap(ps match {
        case current +: rightPs =>
          val withSnd = if (rightPs.nonEmpty) {
            argument(zipSndAfter(
              rightPs.reduceRight[Rise] { case (a, b) => zip(a, b) }
            )) `;` mapFusion
          } else {
            idS
          }
          val withFst = leftPs.foldRight[Strategy[Rise]](idS) { case (p, s) =>
            s `;` argument(zipFstAfter(p)) `;` mapFusion
          }
          ps = rightPs
          leftPs = leftPs :+ current
          withSnd `;` withFst
      }))
    }
  }

  def unifyMapOutsideGenerateSelect: Strategy[Rise] = {
    function(isEqualTo(DSL.generate)) `;`
    argument(body(
      function(function(function(isEqualTo(DSL.select)))) `;`
      unifyMapInputs(Seq(argument, s => function(argument(s))))
    )) `;` mapOutsideGenerateSelect `;`
    argument(argument(normalizeInput) `;` repeat(mapFusion))
  }

  def unifyMapOutsideMakeArray: Strategy[Rise] = {
    // TODO: can work for arbitrary size arrays
    function(function(function(isEqualTo(DSL.makeArray(3))))) `;`
    unifyMapInputs(Seq(
      argument,
      s => function(argument(s)),
      s => function(function(argument(s)))
    )) `;`
    mapOutsideMakeArray `;`
    argument(argument(normalizeInput) `;` repeat(mapFusion))
  }

  def demosaicCircularBuffers: Strategy[Rise] = {
    rewriteSteps(Seq(
      normalize.apply(gentleBetaReduction),

      takeDropTowardsInput,

      // 3. push line mapping towards output
      body(body(body(
        function(body(function(body(
          // generate/select 3x2
          repeatNTimes(6, oncetd(function(isEqualTo(DSL.generate)) `;`
            oncetd(one(unifyMapOutsideGenerateSelect))
          )) `;`
          normalize.apply(gentleBetaReduction <+ etaReduction <+
            removeTransposePair <+ mapFusion) `;`
          // generate/select 3x2
          repeatNTimes(3,
            oncetd(unifyMapOutsideGenerateSelect)
          ) `;`
          normalize.apply(gentleBetaReduction <+ etaReduction <+
            removeTransposePair <+ mapFusion) `;`
          // makeArray
          fOutsideMakeArray `;` argument(unifyMapOutsideMakeArray `;`
            argument(function(argument(
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
              function(function(lowering.iterateStream)) `;`
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
    // TODO: normal form / non-map specific?
    case expr @ App(App(primitives.Map(), Lambda(y,
      App(App(primitives.Let(), Lambda(x, b)), v)
    )), in) if !contains[Rise](y).apply(v) =>
      Success(let(lambda(untyped(x), map(lambda(untyped(y), b), in)), v) :: expr.t)
    case expr @ App(primitives.Map(), Lambda(y,
      App(App(primitives.Let(), Lambda(x, b)), v)
    )) if !contains[Rise](y).apply(v) =>
      Success(fun(in => let(lambda(untyped(x), map(lambda(untyped(y), b), in)), v)) :: expr.t)
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
      precomputeCurve,
      takeDropTowardsInput,

      // demosaic: push line mapping towards output
      afterTopLevel(
        argument(argument(argument(argument(argument(
          function(body(
            function(body(
              function(body(
                // generate/select 3x2
                repeatNTimes(6, oncetd(function(isEqualTo(DSL.generate)) `;`
                  oncetd(one(unifyMapOutsideGenerateSelect))
                )) `;`
                normalize.apply(gentleBetaReduction <+ etaReduction <+
                  removeTransposePair <+ mapFusion) `;`
                // generate/select 3x2
                repeatNTimes(3,
                  oncetd(unifyMapOutsideGenerateSelect)
                ) `;`
                normalize.apply(gentleBetaReduction <+ etaReduction <+
                  removeTransposePair <+ mapFusion) `;`
                // makeArray
                fOutsideMakeArray `;` argument(unifyMapOutsideMakeArray `;`
                  argument(function(argument(
                    normalize.apply(
                      betaReduction <+ etaReduction <+
                      removeTransposePair <+ mapFusion <+
                      idxReduction <+ fstReduction <+ sndReduction
                    )
                  )))
                )
                // transposeBeforeMapJoin `;` argument(argument(mapFusion))
              )) `;` argument(normalizeInput)
            )) `;` argument(normalizeInput)
          ))
        )))))
      ),
/* curve and sharpen
      argument(function(body(

      )))
*/
      // circular buffer demosaic
    ))
  }
}
