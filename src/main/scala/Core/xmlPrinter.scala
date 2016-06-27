package Core

import java.io.{File, PrintWriter}

import scala.xml._

object ToString {
  def apply(a: Any): String = {
    if (a == null) { "null" } else { a.toString }
  }
}

object xmlPrinter {

  def toFile[T <: PhraseType](filename: String, p: Phrase[T]): Unit = {
    val pw = new PrintWriter(new File(filename))
    try pw.write(asString(p)) finally pw.close()
  }

  def asString[T <: PhraseType](p: Phrase[T]): String = {
    <expression>{apply(p)}</expression>.toString()
  }

  def apply[T <: PhraseType](p: Phrase[T]): xml.Elem = {
    val elem = p match {
      case app: ApplyPhrase[a, T] =>
        <apply>
          <fun>{apply(app.fun)}</fun>
          <arg>{apply(app.arg)}</arg>
        </apply>

      case app: NatDependentApplyPhrase[T] =>
        <natApply>
          <fun>{apply(app.fun)}</fun>
          <arg t="Nat">{app.arg}</arg>
        </natApply>

      case p1: Proj1Phrase[a, b] =>
        <getFst>{apply(p1.pair)}</getFst>

      case p2: Proj2Phrase[a, b] =>
        <getSnd>{apply(p2.pair)}</getSnd>

      case IfThenElsePhrase(cond, thenP, elseP) =>
        <ifThenElse>
          <if>{apply(cond)}</if>
          <then>{apply(thenP)}</then>
          <else>{apply(elseP)}</else>
        </ifThenElse>

      case UnaryOpPhrase(op, x) =>
        <unary op={op.toString}>{apply(x)}</unary>

      case BinOpPhrase(op, lhs, rhs) =>
        <binary op={op.toString}>
          <lhs>{apply(lhs)}</lhs>
          <rhs>{apply(rhs)}</rhs>
        </binary>

      case IdentPhrase(name) =>
        <identifier name={name} />

      case LambdaPhrase(param, body) =>
        <lambda param={param.name}>
          {apply(body)}
        </lambda>

      case NatDependentLambdaPhrase(param, body) =>
        <natLambda>
          <param t="Nat">{param.name}</param>
          <body>{apply(body)}</body>
        </natLambda>

      case LiteralPhrase(d) => <lit>{d}</lit>

      case PairPhrase(fst, snd) =>
        <pair>
          <fst>{apply(fst)}</fst>
          <snd>{apply(snd)}</snd>
        </pair>

      case p: ExpPattern => p.xmlPrinter

      case p: AccPattern => p.xmlPrinter

      case p: IntermediateCommandPattern => p.xmlPrinter

    }
    elem.copy(attributes =
      append(elem.attributes,
        Attribute("type", Text(ToString(p.t)), Null)))
  }

  def append(head: MetaData, node: MetaData): MetaData = {
    head.next match {
      case null | Null => head.copy(next = node)
      case _ => head.copy(next = append(head.next, node))
    }
  }

}
