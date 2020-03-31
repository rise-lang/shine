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
  private def rewriteSteps(steps: Seq[Strategy[Rise]]): Strategy[Rise] = a => {
    var nRewrite = 0
    steps.foldLeft[RewriteResult[Rise]](Success(a))({ case (r, s) =>
      r.flatMapSuccess { e =>
        nRewrite += 1
        val result = printTime(s"rewrite $nRewrite", s(e))
        // util.dotPrintTmp(s"rewrite$nRewrite", result)
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

  def debugS(msg: String) =
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
  def isAppliedDrop: Strategy[Rise] = function(depFunction(isEqualTo(DSL.drop)))
  def isAppliedTake: Strategy[Rise] = function(depFunction(isEqualTo(DSL.take)))
  def isAppliedSlide: Strategy[Rise] =
    function(depFunction(depFunction(isEqualTo(DSL.slide))))

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

  // idx i >> f -> map f >> idx i
  def idxAfterF: Strategy[Rise] = {
    case expr @ App(f, App(App(primitives.Idx(), i), in)) =>
      Success(idx(i, map(f, in)) :: expr.t)
    case _ => Failure(idxAfterF)
  }

  def normalizeSingleInput: Strategy[Rise] = normalize.apply(
    dropBeforeTake <+ dropBeforeMap <+ takeBeforeMap <+
    slideBeforeMap <+ mapFusion // <+ TODO
    // (not(isAppliedMap) `;` idxAfterF `;` debugS("idx"))
  )

  def normalizeInput: Strategy[Rise] =
    afterMaps(select(isAppliedZip, normalizeZipInput, normalizeSingleInput)) `;`
    repeat(mapFusion)

  def same(toA: Traversal, toB: Traversal): Strategy[Rise] = { p =>
    var a: Rise = null
    toA { e => a = e; Success(e) }(p) flatMapSuccess { p2 =>
      toB { isEqualTo(erase(a)) }(p2)
    }
  }

  def unifySingleInputsRec(toA: Traversal, toB: Traversal): Strategy[Rise] = { p =>
    def symProgress(f: Traversal => Traversal => Strategy[Rise]): Strategy[Rise] =
      f(toA)(toB) <+ f(toB)(toA)
    (
      same(toA, toB) <+
      select(same(s => toA(function(s)), s => toB(function(s))),
        unifySingleInputsRec(s => toA(argument(s)), s => toB(argument(s))),
        symProgress(to1 => to2 =>
          (to1(takeBeforeDrop) `;` unifySingleInputsRec(to1, to2)) <+
          (to1(takeInSlide) `;` unifySingleInputsRec(s => to1(argument(s)), to2)) <+
          (to1(dropInSlide) `;` unifySingleInputsRec(s => to1(argument(s)), to2)) <+
          (to2(isAppliedSlide) `;` to1(slideAfter2) `;`
            // slides can be different here and should be patched later
            unifySingleInputsRec(s => to1(argument(argument(s))), s => to2(argument(s)))) <+
          (to2(isAppliedSlide) `;` to1(isAppliedDrop <+ isAppliedTake) `;`
            unifySingleInputsRec(s => to1(argument(s)), to2) `;`
            to1((dropBeforeMap `;` argument(dropInSlide) `;` mapFusion) <+
              (takeBeforeMap `;` argument(takeInSlide) `;` mapFusion)))
        )
      )
    )(p)
  }

  def unifySingleInputs(toA: Traversal, toB: Traversal): Strategy[Rise] =
    unifySingleInputsRec(toA, toB) `;`
    toA(normalizeInput) `;` toB(normalizeInput)

  def zipSameId: Strategy[Rise] = (
    unifySingleInputs(argument, s => function(argument(s))) `;`
    anyMapOutsideZip `;` afterMaps(zipSame) `;` repeat(mapFusion)
  ) <+ debugS("zipSameId failed")

  def singleInputId(ret: Int => Unit): Strategy[Rise] = p => {
    import primitives.Idx
    import semantics.IndexData
    import arithexpr.arithmetic.Cst

    var num = 0
    def traverse: Strategy[Rise] = { p =>
      val s = (
        (
          isAppliedMap <+ isAppliedDrop <+ isAppliedTake <+ isAppliedSlide
        ) `;` argument(traverse)
      ) <+ {
        case p @ App(
          App(Idx(), Literal(IndexData(Cst(i), _))),
          _
        ) =>
          // FIXME: could go wrong
          num += i.toInt - 10000
          argument(traverse)(p)
        case p @ Identifier(name) =>
          num += name.drop(1).toInt
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
      case (r, toMap) => r.flatMapSuccess { p =>
        toMap(ps match {
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
        })(p)
      }
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

  def stronglyReducedForm: Strategy[Rise] = normalize.apply(
    betaReduction <+ etaReduction <+
    removeTransposePair <+ mapFusion <+
    idxReduction <+ fstReduction <+ sndReduction
  )

  def gentlyReducedForm: Strategy[Rise] = normalize.apply(
    gentleBetaReduction <+ etaReduction <+
    removeTransposePair <+ mapFusion <+
    idxReduction <+ fstReduction <+ sndReduction
  )

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
          gentlyReducedForm `;`
          // generate/select 3x2
          repeatNTimes(3,
            oncetd(unifyMapOutsideGenerateSelect)
          ) `;`
          gentlyReducedForm `;`
          // makeArray
          fOutsideMakeArray `;` argument(unifyMapOutsideMakeArray `;`
            argument(function(argument(stronglyReducedForm)))
          )
        ))))
      ))),

      // 4. lowering with slideSeq
      {
        import TypedDSL._
        body(body(body(
          function(body(function(body(
            argument(argument(
              function(function(lowering.iterateStream)) `;`
                argument(argument(argument(argument(argument(
                  repeatNTimes(2, oncetd(
                    lowering.circularBuffer(mapSeq(fun(x => x)))
                  ))
                ))))) `;`
                // TODO: use proper rewriting to achieve this
                function(argument(body({ expr =>
                  Success(
                    typed(expr) |> transpose >> map(transpose) >>
                      // 2 bands of y. all x. rgb channels.
                      mapSeqUnroll(mapSeq(mapSeqUnroll(fun(x => x)))) >>
                      map(transpose) >> transpose
                  )
                }))) `;` stronglyReducedForm
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
                gentlyReducedForm `;`
                // generate/select 3x2
                repeatNTimes(3,
                  oncetd(unifyMapOutsideGenerateSelect)
                ) `;`
                gentlyReducedForm `;`
                // makeArray
                fOutsideMakeArray `;` argument(unifyMapOutsideMakeArray `;`
                  argument(function(argument(stronglyReducedForm)))
                )
              )) `;`
              argument(normalizeInput `;` stronglyReducedForm)
            )) `;`
            argument(normalizeInput `;` stronglyReducedForm)
          ))
        )))))
      ),

      afterTopLevel(function(argument(body(function(body(
        printS("hello sharpen") `;`
        normalizeInput `;` stronglyReducedForm
      ))))))
      // circular buffer demosaic
    ))
  }
}
