package komb

import org.scalatest._
import flatspec._
import matchers._

class EnvTest extends AnyFlatSpec with should.Matchers {
  val entry: (String, Value) = ("foo" -> Value(Num(1)))
  val testEnv: Env = Env().addEntry(entry)

}
