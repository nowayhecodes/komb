package komb

import scala.annotation.tailrec

sealed trait Token

case class TOpen() extends Token

case class TClose() extends Token

case class TNumber(v: String) extends Token

case class TString(v: String) extends Token

case class TSymbol(v: String) extends Token

object HandRolledParser {
  def tokenize(source: List[Char]): Unit = {
    @tailrec
    def string(acc: String, cs: List[Char]): (String, List[Char]) = cs match {
      case '\\' :: '"' :: t => string(acc + "\"", t)
      case '"' :: t => (acc, t)
      case c :: t => string(acc + c, t)
      case _ => throw new IllegalArgumentException
    }

    @tailrec
    def token(acc: String, cs: List[Char]): (String, List[Char]) = cs match {
      case t@')' :: _ => (acc, t)
      case w :: t if w.isWhitespace => (acc, t)
      case List() => (acc, List())
      case c :: t => token(acc + c, t)
    }

    @tailrec
    def doTokenize(acc: List[Token], cs: List[Char]): List[Token] = cs match {
      case w :: t if w.isWhitespace => doTokenize(acc, t)
    }
  }
}
