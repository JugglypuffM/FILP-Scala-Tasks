package exercises04.calculator

import org.scalatest.wordspec.AnyWordSpec

class CalculatorSpec extends AnyWordSpec {
  val calculator: Calculator[Int] = new Calculator[Int]

  "EvalEitherIntegral.calculate" should {
    "calculate mul" in {
      assert(calculator.calculate(Mul(Val(2), Val(2))) == Success(4))
      assert(calculator.calculate(Mul(Val(15), Val(34))) == Success(510))
    }

    "calculate div" in {
      assert(calculator.calculate(Div(Val(15), Val(5))) == Success(3))
      assert(calculator.calculate(Div(Val(42), Val(0))) == DivisionByZero)
    }

    "calculate plus" in {
      assert(calculator.calculate(Plus(Val(3), Val(0))) == Success(3))
      assert(calculator.calculate(Plus(Val(10), Val(-10))) == Success(0))
      assert(calculator.calculate(Plus(Val(134), Val(-200))) == Success(-66))
    }

    "calculate minus" in {
      assert(calculator.calculate(Minus(Val(42), Val(50))) == Success(-8))
      assert(calculator.calculate(Minus(Val(50), Val(42))) == Success(8))
      assert(calculator.calculate(Minus(Val(-200), Val(-134))) == Success(-66))
    }

    "calculate val" in {
      assert(calculator.calculate(Val(42)) == Success(42))
      assert(calculator.calculate(Val(100500)) == Success(100500))
    }

    "calculate if" in {
      def expr(cond: Int): Expr[Int] =
        If[Int](_ > 3, Val(cond), Plus(Val(1), Div(Val(3), Val(0))), Plus(Val(6), Val(36)))

      assert(calculator.calculate(expr(4)) == DivisionByZero)
      assert(calculator.calculate(expr(2)) == Success(42))
    }
  }
}
