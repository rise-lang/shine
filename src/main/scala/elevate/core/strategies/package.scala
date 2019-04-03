package elevate.core

import lift.core.Expr

package object strategies {
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

  def peek(f: Expr => Unit): Strategy =
    e => { f(e); e }

  def repeat: Strategy => Strategy =
    s => `try`(s `;` (e => repeat(s)(e)))

  def countingRepeat: (Int => Strategy) => Int => Strategy =
    s => i => `try`(s(i) `;` (e => countingRepeat(s)(i+1)(e)))

  def repeatNTimes: Int => Strategy => Strategy =
    n => s => if (n > 0) { s `;` repeatNTimes(n-1)(s) } else { id }

  def applyDFS: Strategy => Strategy =
    s => applyAt(s)(FindFirst(isDefined(s)))

  def applyDFSSkip: Int => Strategy => Strategy =
    n => s => e => { // function of 'e' to create fresh vars
      var skip = n
      applyAt(s)(FindFirst({ e =>
        isDefined(s)(e) && {
          if (skip > 0) {
            skip -= 1
            false
          } else {
            true
          }
        }
      }))(e)
    }

  def normalize: Strategy => Strategy =
    s => repeat(applyDFS(s))

  def listLocations: Strategy => Expr => Seq[Location] =
    s => e => Location.findAll(isDefined(s)(_))(e)
}
