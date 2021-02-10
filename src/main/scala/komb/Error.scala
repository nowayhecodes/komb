package komb

object Error {
  def argumentDefinitionError = throw new IllegalArgumentException("define args")

  def arithmeticError = throw new IllegalArgumentException("arithmetic error")

  def carError = throw new IllegalArgumentException("car")

  def cdrError = throw new IllegalArgumentException("cdr")

  def comparisonError = throw new IllegalArgumentException("comparison error")

  def conditionError = throw new IllegalArgumentException("cond")

  def consError = throw new IllegalArgumentException("cons")

  def definitionError = throw new IllegalArgumentException("define")

  def displayError = throw new IllegalArgumentException("display")

  def ifError = throw new IllegalArgumentException("if")

  def lambdaError = throw new IllegalArgumentException("lambda")

  def letError = throw new IllegalArgumentException("let")

  def newlineError = throw new IllegalArgumentException("newline")

  def notError = throw new IllegalArgumentException("not")

  def nullError = throw new IllegalArgumentException("null?")
}
