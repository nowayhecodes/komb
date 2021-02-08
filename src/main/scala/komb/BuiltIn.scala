package komb

object BuiltIn {
  def aritFn(op: ((BigDecimal, BigDecimal) => BigDecimal))
            (env: Env, comb: List[ExprT]) = {
    def error = throw new IllegalArgumentException("arithmetic error")

    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft(first)((acc, e) =>
          e match { case Value(Num(v)) => op(acc, v); case _ => error })
        (env, Value(Num(res)))
        case _ => error
    }
  }
}
