package komb

sealed trait Token
case class TOpen() extends Token
case class TClose() extends Token
case class TNumber(v: String) extends Token
case class TString(v: String) extends Token
case class TSymbol(v: String) extends Token

object HandRolledParser {
  def tokenize(source: List[Char]) = {}
}
