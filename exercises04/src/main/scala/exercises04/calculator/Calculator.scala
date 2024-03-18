package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  private def processResult(left: Result[T], right: Result[T], f: (T, T) => Result[T]): Result[T] =
    (left, right) match {
      case (Success(valueLeft: T), Success(valueRight: T)) => f(valueLeft, valueRight)
      case _                                               => DivisionByZero: Result[T]
    }
  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Mul(left, right) => processResult(calculate(left), calculate(right), (left, right) => Success(left * right))
    case Div(left, right) =>
      processResult(
        calculate(left),
        calculate(right),
        (left, right) => if (isZero(right)) DivisionByZero else Success(left / right)
      )
    case Plus(left, right)  => processResult(calculate(left), calculate(right), (left, right) => Success(left + right))
    case Minus(left, right) => processResult(calculate(left), calculate(right), (left, right) => Success(left - right))
    case Val(v)             => Success(v)
    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(value) => if (iff(value)) calculate(left) else calculate(right)
      }
  }
}
