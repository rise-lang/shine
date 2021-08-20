package apps

import util.printTime
import rise.core.{primitives => p, _}
import rise.core.DSL._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.elevate.rules.traversal.alternative._
import elevate.core.strategies.predicate._
import rise.elevate._
import rise.elevate.rules._
import rise.elevate.rules.movement._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.traversal._

object cameraPipelineRewrite {
  private def rewriteSteps(steps: scala.collection.Seq[Strategy[Rise]]): Strategy[Rise] = a => {
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
      case ap @ DepApp(kind, f, x) => s(f).mapSuccess(DepApp(kind, _, x)(ap.t))
      case _ => Failure(s)
    }
    override def toString: String = s"depFunction($s)"
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
    normalize(
      gentleBetaReduction() <+ etaReduction() <+
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
  private def printS(msg: String) =
    elevate.core.strategies.debug.echo[Rise](msg)
  private val idS = elevate.core.strategies.basic.id[Rise]

  type Traversal = Strategy[Rise] => Strategy[Rise]

  def select(predicate: Strategy[Rise], a: Strategy[Rise], b: Strategy[Rise])
  : Strategy[Rise] = p => {
    if (predicate(p)) { a(p) } else { b(p) }
  }

  def isAppliedMap: Strategy[Rise] = function(function(isEqualTo(p.map.primitive)))
  def isAppliedZip: Strategy[Rise] = function(function(isEqualTo(p.zip.primitive)))
  def isAppliedDrop: Strategy[Rise] = function(depFunction(isEqualTo(p.drop.primitive)))
  def isAppliedTake: Strategy[Rise] = function(depFunction(isEqualTo(p.take.primitive)))
  def isAppliedSlide: Strategy[Rise] =
    function(depFunction(depFunction(isEqualTo(p.slide.primitive))))

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
    case expr @ App(f, App(App(p.idx(), i), in)) =>
      Success(p.idx(i)(p.map(f)(in)) !: expr.t)
    case _ => Failure(idxAfterF)
  }

  def normalizeSingleInput: Strategy[Rise] = normalize(
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
      toB { isEqualTo(eraseTypeFromExpr(a)) }(p2)
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
    import rise.core.primitives.idx
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
          App(idx(), Literal(IndexData(Cst(i), _))),
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

  def unifyMapInputs(toMaps: scala.collection.Seq[Traversal]): Strategy[Rise] = p => {
    var ps = scala.collection.Seq[Rise]()
    val normalized = toMaps.foldLeft[RewriteResult[Rise]](Success(p)) {
      case (r, toMap) => r.flatMapSuccess(toMap(
        argument(normalizeInput) `;` repeat(mapFusion) `;`
        argument { e => ps = ps :+ e; Success(e) }
      ))
    }
    var leftPs = scala.collection.Seq[Rise]()
    toMaps.foldLeft(normalized) {
      case (r, toMap) => r.flatMapSuccess { p =>
        toMap(ps match {
          case current +: rightPs =>
            val withSnd = if (rightPs.nonEmpty) {
              argument(zipSndAfter(
                rightPs.reduceRight[Rise] { case (a, b) => rise.core.primitives.zip(a)(b) }
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
    function(isEqualTo(p.generate.primitive)) `;`
    argument(body(
      function(function(function(isEqualTo(p.select.primitive)))) `;`
      unifyMapInputs(scala.collection.Seq(argument, s => function(argument(s))))
    )) `;` mapOutsideGenerateSelect() `;`
    argument(argument(normalizeInput) `;` repeat(mapFusion))
  }

  def unifyMapOutsideMakeArray: Strategy[Rise] = {
    // TODO: can work for arbitrary size arrays
    function(function(function(isEqualTo(p.makeArray(3).primitive)))) `;`
    unifyMapInputs(scala.collection.Seq(
      argument,
      s => function(argument(s)),
      s => function(function(argument(s)))
    )) `;`
    mapOutsideMakeArray `;`
    argument(argument(normalizeInput) `;` repeat(mapFusion))
  }

  def stronglyReducedForm: Strategy[Rise] = normalize(
    betaReduction <+ etaReduction() <+
    removeTransposePair <+ mapFusion <+
    idxReduction <+ fstReduction <+ sndReduction
  )

  def gentlyReducedForm: Strategy[Rise] = normalize(
    gentleBetaReduction() <+ etaReduction() <+
    removeTransposePair <+ mapFusion <+
    idxReduction <+ fstReduction <+ sndReduction
  )

  def demosaicCircularBuffers: Strategy[Rise] = {
    rewriteSteps(scala.collection.Seq(
      normalize(gentleBetaReduction()),

      takeDropTowardsInput,

      // 3. push line mapping towards output
      body(body(body(
        function(body(function(body(
          // generate/select 3x2
          repeatNTimes(6)(topDown(function(isEqualTo(p.generate.primitive)) `;`
            topDown(one(unifyMapOutsideGenerateSelect))
          )) `;`
          gentlyReducedForm `;`
          // generate/select 3x2
          repeatNTimes(3)(
            topDown(unifyMapOutsideGenerateSelect)
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
        import DSL._
        import rise.core.primitives._
        body(body(body(
          function(body(function(body(
            argument(argument(
              function(function(lowering.iterateStream)) `;`
                argument(argument(argument(argument(argument(
                  repeatNTimes(2)(topDown(
                    lowering.circularBuffer(mapSeq(fun(x => x)))
                  ))
                ))))) `;`
                // TODO: use proper rewriting to achieve this
                function(argument(body({ expr =>
                  Success(
                    preserveType(expr) |> transpose >> map(transpose) >>
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
    function(function(isEqualTo(p.let.primitive))) `;`
    argument(s)

  def afterTopLevel(s: Strategy[Rise]): Strategy[Rise] = p => {
    (body(afterTopLevel(s)) <+
      letContinuation(afterTopLevel(s)) <+
      s)(p)
  }

  def precomputeSharpenStrengthX32: Strategy[Rise] = {
    // |> toMem() |> letf(fun(strength_x32 =>
    normalize(gentleBetaReduction()) `;`
    afterTopLevel(
      function(argument( // sharpen
        ???
      ))
    )
  }

  def letHoist: Strategy[Rise] = {
    case expr @ App(f, App(App(p.let(), v), Lambda(x, b))) =>
      Success(letf(lambda(eraseType(x), preserveType(f)(b)))(v) !: expr.t)
    // TODO: normal form / non-map specific?
    case expr @ App(App(p.map(), Lambda(y,
      App(App(p.let(), v), Lambda(x, b))
    )), in) if !contains[Rise](y).apply(v) =>
      Success(letf(lambda(eraseType(x), p.map(lambda(eraseType(y), b))(in)))(v) !: expr.t)
    case expr @ App(p.map(), Lambda(y,
      App(App(p.let(), v), Lambda(x, b))
    )) if !contains[Rise](y).apply(v) =>
      Success(fun(in =>
        letf(lambda(eraseType(x), p.map(lambda(eraseType(y), b))(in)))(v)
      ) !: expr.t)
    case _ => Failure(letHoist)
  }

  def precomputeColorCorrectionMatrix: Strategy[Rise] = {
    normalize(gentleBetaReduction()) `;`
    afterTopLevel(
      argument(argument({
        case expr @ App(Lambda(x, color_correct), matrix) =>
          Success(letf(lambda(toBeTyped(x), color_correct))(
            p.mapSeq(p.mapSeq(fun(x => x)))(matrix)) !: expr.t)
        case _ => Failure(precomputeColorCorrectionMatrix)
      })) `;`
      normalize(gentleBetaReduction() <+ letHoist)
    )
  }

  def precomputeCurve: Strategy[Rise] = {
    // TODO: apply_curve curve:
    // |> mapSeq(fun(x => x)) |> letf(fun(curve =>
    normalize(gentleBetaReduction()) `;`
    afterTopLevel(
      argument(function(argument(
        topDown(
          function(function(isEqualTo(p.idx.primitive))) `;`
          argument(function(isEqualTo(p.generate.primitive))) `;`
          argument({ curve =>
            Success(letf(fun(x => x))(
              p.mapSeq(fun(x => x))(curve)) !: curve.t)
          })
        )
      ))) `;`
      repeat(topDown(letHoist)) // TODO: not hoisted far enough
    )
  }

  def circularBuffers: Strategy[Rise] = {
    rewriteSteps(scala.collection.Seq(
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
                repeatNTimes(6)(topDown(function(isEqualTo(p.generate.primitive)) `;`
                  topDown(one(unifyMapOutsideGenerateSelect))
                )) `;`
                gentlyReducedForm `;`
                // generate/select 3x2
                repeatNTimes(3)(
                  topDown(unifyMapOutsideGenerateSelect)
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
