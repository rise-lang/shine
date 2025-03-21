package shine.DPIA.Types

object MatchingDSL {

  object ->: {
    def unapply[T <: PhraseType, R <: PhraseType](funType: FunType[T, R]): Option[(T, R)] = {
      Some((funType.inT, funType.outT))
    }
  }

}
