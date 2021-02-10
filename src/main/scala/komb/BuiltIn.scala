package komb

import komb.Error._
import komb.Interpreter._

import scala.annotation.tailrec

object BuiltIn {
  val globalEnv: Env = Env(EnvT(EnvMapT(
    ("+" -> Proc(aritFn(_ + _))),
    ("-" -> Proc(aritFn(_ - _))),
    ("*" -> Proc(aritFn(_ * _))),
    ("/" -> Proc(aritFn(_ / _))),

    ("=" -> Proc(combFn(_ == _))),
    (">" -> Proc(combFn(_ > _))),
    ("<" -> Proc(combFn(_ < _))),
    (">=" -> Proc(combFn(_ >= _))),
    ("<=" -> Proc(combFn(_ <= _))),

    ("not" -> Proc(_not)),
    ("if" -> Proc(_if)),
    ("cond" -> Proc(_cond)),
    ("define" -> Proc(_define)),
    ("cons" -> Proc(_cons)),
    ("list" -> Proc(_list)),
    ("append" -> Proc(_append)),
    ("car" -> Proc(_car)),
    ("cdr" -> Proc(_cdr)),
    ("null?" -> Proc(_null)),
    ("let" -> Proc(_let)),
    ("begin" -> Proc(_begin)),
    ("lambda" -> Proc(_lambda)),
    ("display" -> Proc(_display)),
    ("newline" -> Proc(_newline)),

    ("true" -> Value(Bool(true))),
    ("false" -> Value(Bool(false)))
  )))

  def aritFn(op: ((BigDecimal, BigDecimal) => BigDecimal))
            (env: Env, comb: List[ExprT]): (Env, Value) = {
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft(first)((acc, e) =>
          e match {
            case Value(Num(v)) => op(acc, v)
            case _ => arithmeticError
          })
        (env, Value(Num(res)))
      case _ => arithmeticError
    }
  }

  def combFn(op: ((BigDecimal, BigDecimal) => Boolean))
            (env: Env, comb: List[ExprT]): (Env, Value) = {
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft((true, first))((acc, e) => e match {
          case Value(Num(v)) => (acc._1 && op(acc._2, v), v)
          case _ => comparisonError
        })
        (env, Value(Bool(res._1)))
      case _ => comparisonError
    }
  }

  def buildList(comb: List[ExprT]): List[ValueT] = comb match {
    case List() => List()
    case Symbol(sym) :: t => Name(sym) :: buildList(t)
    case Value(v) :: t => v :: buildList(t)
    case _ => argumentDefinitionError
  }

  def _not(env: Env, comb: List[ExprT]): (Env, Value) = comb match {
    case expr :: Nil => eval(env, expr) match {
      case (_, Value(Bool(v))) => (env, Value(Bool(!v)))
      case _ => notError
    }
    case _ => notError
  }

  def _if(env: Env, comb: List[ExprT]): (Env, ExprT) = {
    val (condExpr, posExpr, negExpr) = comb match {
      case condExpr :: posExpr :: negExpr :: Nil => (condExpr, posExpr, Some(negExpr))
      case condExpr :: posExpr :: Nil => (condExpr, posExpr, None)
      case _ => ifError
    }
    (eval(env, condExpr))._2 match {
      case Value(Bool(c)) =>
        if (c)
          eval(env, posExpr)
        else negExpr match {
          case Some(e) => eval(env, e)
          case None => (env, NullExpr())
        }
      case _ => ifError
    }
  }

  def _cond(env: Env, comb: List[ExprT]): Unit = {
    def doExpression(comb: List[ExprT]) = comb match {
      case Symbol("else") :: posExpr :: Nil => Some(eval(env, posExpr)._2)
      case condExpr :: posExpr :: Nil => eval(env, condExpr)._2 match {
        case Value(Bool(true)) => Some(eval(env, posExpr)._2)
        case Value(Bool(false)) => None
        case _ => conditionError
      }
      case _ => conditionError
    }

    @tailrec
    def runExpressions(comb: List[ExprT]): ExprT = comb match {
      case Comb(c) :: rst => doExpression(c) match {
        case Some(e) => e
        case None => runExpressions(rst)
      }

      case _ => NullExpr()
    }

    (env, runExpressions(comb))
  }

  def _define(env: Env, comb: List[ExprT]): (Env, NullExpr) = {
    def getString(expr: ExprT) = expr match {
      case Symbol(sym) => sym
      case Value(Name(n)) => n
      case _ => definitionError
    }

    comb match {
      case Symbol(sym) :: expr :: Nil => {
        val (nEnv, res) = eval(env, expr)
        (nEnv.addEntry(sym -> res), NullExpr())
      }
      case Comb(ns) :: body => {
        val fName = getString(ns.head)
        val args = buildList(ns.tail)
        (env.addEntry(fName -> Func(args, body)), NullExpr())
      }
      case _ => definitionError
    }
  }

  def _cons(env: Env, comb: List[ExprT]): (Env, EList) = comb match {
    case e1 :: e2 :: Nil => (eval(env, e1)._2, eval(env, e2)._2) match {
      case (expr1, NullExpr()) => (env, EList(List(expr1)))
      case (expr1, EList(l2)) => (env, EList(expr1 :: l2))
      case (expr1, expr2) => (env, EList(List(expr1, expr2)))
    }
    case _ => consError
  }

  def _list(env: Env, comb: List[ExprT]): (Env, EList) = {
    def doExpression(comb: List[ExprT]): List[ExprT] = comb match {
      case List() => Nil
      case h :: t => eval(env, h)._2 :: doExpression(t)
    }

    (env, EList(doExpression(comb)))
  }

  def _append(env: Env, comb: List[ExprT]): (Env, EList) = {
    def doExpression(comb: List[ExprT]): List[ExprT] = comb match {
      case List() => Nil
      case h :: t => eval(env, h)._2 match {
        case EList(l) => l ::: doExpression(t)
        case expr => expr :: doExpression(t)
      }
    }

    (env, EList(doExpression(comb)))
  }

  def _car(env: Env, comb: List[ExprT]): (Env, ExprT) = comb match {
    case h :: Nil => eval(env, h)._2 match {
      case EList(l) => (env, l.head)
      case _ => carError
    }
    case _ => carError
  }

  def _cdr(env: Env, comb: List[ExprT]): (Env, EList) = comb match {
    case h :: Nil => eval(env, h)._2 match {
      case EList(List()) => (env, EList(List()))
      case EList(l) => (env, EList(l.tail))
      case _ => cdrError
    }
    case _ => cdrError
  }

  def _null(env: Env, comb: List[ExprT]): (Env, Value) = comb match {
    case h :: Nil => eval(env, h)._2 match {
      case EList(List()) => (env, Value(Bool(true)))
      case _ => (env, Value(Bool(false)))
    }
    case _ => nullError
  }

  def _let(env: Env, comb: List[ExprT]): (Env, ExprT) = {
    @tailrec
    def bind(acc: Env, binds: List[ExprT]): Env = binds match {
      case List() => acc
      case Comb(c) :: t => c match {
        case Symbol(sym) :: expr :: Nil => bind(acc.addEntry(sym -> eval(env, expr)._2), t)
        case _ => letError
      }
      case _ => letError
    }

    comb match {
      case Comb(binds) :: body :: Nil => val newEnv = bind(env.expand(), binds)
        eval(newEnv, body)
      case _ => letError
    }
  }

  def _begin(env: Env, comb: List[ExprT]): (Env, ExprT) = evalAll(env, comb)

  def _lambda(env: Env, comb: List[ExprT]): (Env, Func) = comb match {
    case Comb(args) :: body => (env, Func(buildList(args), body))
    case _ => lambdaError
  }

  def _display(env: Env, comb: List[ExprT]): (Env, NullExpr) = {
    comb match {
      case expr :: Nil => (eval(env, expr)._2) match {
        case Value(Num(v)) => println(v)
        case Value(Name(v)) => println(v)
        case Value(Bool(v)) => println(v)
        case EList(l) => println(listToString(l))
        case _ => displayError
      }
      case _ => displayError
    }
    (env, NullExpr())
  }

  def listToString(ls: List[ExprT]): String = {
    def convert(ls: List[ExprT]): String = ls match {
      case List() => ""
      case Value(Num(v)) :: t => v.toString + ", " + convert(t)
      case Value(Bool(v)) :: t => v.toString + ", " + convert(t)
      case Value(Name(v)) :: t => v + ", " + convert(t)
      case EList(l) :: t => "(" + convert(l) + "), " + convert(t)
      case _ :: t => convert(t)
    }

    "(" + convert(ls) + ")"
  }

  def _newline(env: Env, comb: List[ExprT]): (Env, NullExpr) = {
    comb match {
      case List() => println()
      case _ => newlineError
    }
    (env, NullExpr())
  }
}
