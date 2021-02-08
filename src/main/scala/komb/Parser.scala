package komb

import scala.util.parsing.combinator._

object Parser extends JavaTokenParers {
  val value: Parser[ValueT] = stringLiteral ^^ (x => Name(x.tail.init)) | floatingPointNumber ^^ (x => Num(BigDecimal(x)))
  val expression: Parser[ExprT] = value ^^ (v => Value(v)) | """[^()\s]+""".r ^^ (s => Symbol(s)) | combination
  val combination: Parser[Comb] = "(" ~> req(expression) <~ ")" ^^ (c => Comb(c))
  val program: Parser[List[ExprT]] = req(expression)

  def parse(source: String) = parseAll(program, source).get
}
