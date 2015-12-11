sealed class Type


class DataType extends Type

object bool extends DataType
object int extends DataType
object int4 extends DataType
object float extends DataType

class PhraseType extends Type

case class ExpType(t : DataType) extends PhraseType

case class AccType(t : DataType) extends PhraseType

object Command extends PhraseType

case class PairType(t1 : PhraseType, t2 : PhraseType) extends PhraseType

case class FunctionType(inT : PhraseType, outT : PhraseType) extends PhraseType

case class PassiveFunctionType(inT : PhraseType, outT : PhraseType) extends PhraseType

// TODO: deal with "type variable" related to Kind