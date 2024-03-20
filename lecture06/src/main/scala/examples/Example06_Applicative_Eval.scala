package examples

import examples.typeclasses.Applicative
import examples.typeclasses.ApplicativeSyntax._

object Example06_Applicative_Eval extends App {

  sealed trait Eval[+A] {
    def run(): A
  }

  object Eval {
    case class Pure[+A](value: A) extends Eval[A] {
      def run(): A = value
    }

    case class Lazy[+A](thunk: () => A) extends Eval[A] {
      def run(): A = thunk()
    }

    def pure[A](value: A): Eval[A]     = Pure(value)
    def delay[A](value: => A): Eval[A] = Lazy(() => value)

    implicit val ioApplicative: Applicative[Eval] = new Applicative[Eval] {
      override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] =
        fa match {
          case Eval.Pure(value) => Eval.Pure(f(value))
          case Eval.Lazy(thunk) => Eval.Lazy(() => f(thunk()))
        }

      override def product[A, B](fa: Eval[A], fb: Eval[B]): Eval[(A, B)] = {
        (fa, fb) match {
          case (Pure(a), Pure(b))                  => Pure((a, b))
          case (Pure(a), Lazy(thunk))              => Lazy(() => (a, thunk()))
          case (Lazy(thunk), Pure(b))              => Lazy(() => (thunk(), b))
          case (Lazy(leftThunk), Lazy(rightThunk)) => Lazy(() => (leftThunk(), (rightThunk())))
        }
      }

      def pure[A](x: A): Eval[A] = Eval.Pure(x)
    }
  }

  sealed trait Result
  case class Temperature(value: Double)  extends Result
  case class ExchangeRate(value: Double) extends Result

  def getTemperature(city: String): Eval[Temperature] = Eval.delay {
    println(s"Getting temperature for $city")
    // Представим, что мы получаем эту информацию из внешнего источника
    city match {
      case "Moscow"        => Temperature(-5)
      case "Yekaterinburg" => Temperature(-10)
      case "Sochi"         => Temperature(15)
    }
  }

  def getExchangeRate(fromCurrency: String, toCurrency: String): Eval[ExchangeRate] = Eval.delay {
    println(s"Getting exchange rate from $fromCurrency to $toCurrency")
    ExchangeRate(94.85) // Аналогично, представим, что это результат запроса к API
  }

  // Независимые вычисления с product
  val productExampleApp: Eval[(Result, Result)] = {
    getTemperature("Yekaterinburg").product(getExchangeRate("USD", "RUB"))
  }

  // Запуск комбинированного вычисления
  private val (temperature, exchangeRate) = productExampleApp.run()
  println(s"Temperature in Yekaterinburg is ${temperature}°C, USD to RUB rate is ${exchangeRate}.")

  // Независимые вычисления с mapN
  val mapNExampleApp: Eval[(Temperature, Temperature, Temperature)] = {
    (
      getTemperature("Moscow"),
      getTemperature("Yekaterinburg"),
      getTemperature("Sochi")
    ).mapN {
      case (moscow, yekb, sochi) => (moscow, yekb, sochi)
    }
  }

  // Запуск комбинированного вычисления
  private val (moscowT, yekbT, sochiT) = mapNExampleApp.run()
  println(s"Temperature in Moscow is ${moscowT}°C")
  println(s"Temperature in Yekaterinburg is ${yekbT}°C")
  println(s"Temperature in Sochi is ${sochiT}°C")
}
