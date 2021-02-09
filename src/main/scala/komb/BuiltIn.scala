package komb

import komb.Interpreter._

object BuiltIn {
  def aritFn(op: ((BigDecimal, BigDecimal) => BigDecimal))
            (env: Env, comb: List[ExprT]): (Env, Value) = {
    def error = throw new IllegalArgumentException("arithmetic error")

    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft(first)((acc, e) =>
          e match {
            case Value(Num(v)) => op(acc, v)
            case _ => error
          })
        (env, Value(Num(res)))
      case _ => error
    }
  }

  def combFn(op: ((BigDecimal, BigDecimal) => Boolean))
            (env: Env, comb: List[ExprT]): (Env, Value) = {
    def error = throw new IllegalArgumentException("comparison error")

    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft((true, first))((acc, e) => e match {
          case Value(Num(v)) => (acc._1 && op(acc._2, v), v)
          case _ => error
        })
        (env, Value(Bool(res._1)))
      case _ => error
    }
  }

  def buildList(comb: List[ExprT]): List[ValueT] = comb match {
    case List() => List()
    case Symbol(sym) :: t => Name(sym) :: buildList(t)
    case Value(v) :: t => v :: buildList(t)
    case _ => throw new IllegalArgumentException("define args")
  }
}
