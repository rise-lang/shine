package idealised.rewriting

import idealised.rewriting.Elevate._

object Strategies {

  def id: Strategy =
    e => e

  def seq: Strategy => Strategy => Strategy =
    f => s => e => s(f(e))

  def leftChoice: Strategy => Strategy => Strategy =
    f => s => e => {
      if (isDefined(f)(e)) { f(e) } else { s(e) }
    }

  def `try`: Strategy => Strategy =
    s => leftChoice(s)(id)

  def repeat: Strategy => Strategy =
    s => `try`(s `;` (e => repeat(s)(e)))

  def countingRepeat: (Int => Strategy) => Int => Strategy =
    s => i => `try`(s(i) `;` (e => countingRepeat(s)(i+1)(e)))

  def normalize: Strategy => Strategy =
    s => repeat(applyAt(s)(FindFirst(isDefined(s))))

  def codeGenNormalForm: Strategy = normalize(Rules.mapFusion)

  def rewriteNormalForm: Strategy = normalize(Rules.mapFission)

  def listLocations: Strategy => LiftExpr => Seq[Location] =
    s => e => Location.findAll(isDefined(s)(_))(e)
}
