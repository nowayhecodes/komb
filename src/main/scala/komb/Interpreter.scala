package komb

import scala.annotation.tailrec

sealed trait ValueT

case class Num(v: BigDecimal) extends ValueT

case class Bool(v: Boolean) extends ValueT

case class Name(v: String) extends ValueT

sealed trait ExprT

case class NullExpr() extends ExprT

case class Comb(v: List[ExprT]) extends ExprT

case class EList(v: List[ExprT]) extends ExprT

case class Func(args: List[ValueT], body: List[ExprT]) extends ExprT

case class Proc(f: (Env, List[ExprT]) => (Env, ExprT)) extends ExprT

case class Symbol(v: String) extends ExprT

case class Value(v: ValueT) extends ExprT

object Interpreter {
  def eval(env: Env, expr: ExprT): (Env, ExprT) = expr match {
    case NullExpr() => throw new IllegalStateException("invalid interpreter state")
    case Comb(List()) => throw new IllegalStateException("invalid combination")
    case Comb(h :: t) =>
      eval(env, h) match {
        case (_, Proc(f)) => apply(f, t, env)
        case (nEnv, Func(args, body)) => {
          if (args.length != t.length) throw new IllegalArgumentException("invalid number of arguments")
          val newEnv = (args zip t).foldLeft(nEnv.expand())((acc, av) => bindargs(acc, av._1, av._2))
          evalAll(newEnv, body)
        }
        case (newEnv, expr) => (nEnv, expr)
      }

    case Proc(f) => (env, Proc(f))
    case Func(args, body) => throw new IllegalArgumentException("invalid function call")
    case v@Value(_) => (env, v)
    case l@List(_) => (env, l)
    case Symbol(sym) =>
      env.lookUp(sym) match {
        case Some(e) => (env, e)
        case None => throw new IllegalArgumentException("unbound symbol '" + s + "'")
      }
  }

  @tailrec
  def evalAll(env: Env, comb: List[ExprT]): (Env, ExprT) = comb match {
    case List() => (env, NullExpr())
    case h :: t => {
      val (nEnv, res) = eval(env, h)
      t.length match {
        case 0 => (env, res)
        case 1 => eval(nEnv, t.head)
        case _ => evalAll(nEnv, t)
      }
    }
  }

  private def apply(f: (Env, List[ExprT]) => (Env, ExprT),
                    args: List[ExprT], env: Env) =
    f(env, args)

  private def bindArgs(env: Env, arg: ValueT, expr: ExprT) = arg match {
    case Name(name) => env.addEntry(n -> eval(env, expr)._2)
    case _ => throw new IllegalArgumentException
  }
}
