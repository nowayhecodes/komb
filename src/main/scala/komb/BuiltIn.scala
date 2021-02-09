package komb

import komb.Interpreter._

import scala.annotation.tailrec

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

  def _not(env: Env, comb: List[ExprT]): (Env, Value) = comb match {
    case expr :: Nil => eval(env, expr) match {
      case (_, Value(Bool(v))) => (env, Value(Bool(!v)))
      case _ => throw new IllegalArgumentException("not")
    }
    case _ => throw new IllegalArgumentException("not")
  }

  def _if(env: Env, comb: List[ExprT]): (Env, ExprT) = {
    val (condExpr, posExpr, negExpr) = comb match {
      case condExpr :: posExpr :: negExpr :: Nil => (condExpr, posExpr, Some(negExpr))
      case condExpr :: posExpr :: Nil => (condExpr, posExpr, None)
      case _ => throw new IllegalArgumentException("if")
    }
    (eval(env, condExpr))._2 match {
      case Value(Bool(c)) =>
        if (c)
          eval(env, posExpr)
        else negExpr match {
          case Some(e) => eval(env, e)
          case None => (env, NullExpr())
        }
      case _ => throw new IllegalArgumentException("if")
    }
  }

  def _cond(env: Env, comb: List[ExprT]): Unit = {
    def doExpression(comb: List[ExprT]) = comb match {
      case Symbol("else") :: posExpr :: Nil => Some(eval(env, posExpr)._2)
      case condExpr :: posExpr :: Nil => eval(env, condExpr)._2 match {
        case Value(Bool(true)) => Some(eval(env, posExpr)._2)
        case Value(Bool(false)) => None
        case _ => throw new IllegalArgumentException("cond")
      }
      case _ => throw new IllegalArgumentException("cond")
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
      case _ => throw new IllegalArgumentException("define")
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
      case _ => throw new IllegalArgumentException("define")
    }
  }

  
}
