sealed trait PhraseType

case class ExpType(dataType : DataType) extends PhraseType

case class AccType(dataType : DataType) extends PhraseType

object Command extends PhraseType

case class PairType(t1 : PhraseType, t2 : PhraseType) extends PhraseType

case class FunctionType(inT : PhraseType, outT : PhraseType) extends PhraseType

case class PassiveFunctionType(inT : PhraseType, outT : PhraseType) extends PhraseType

// TODO: deal with "type variable" related to Kind