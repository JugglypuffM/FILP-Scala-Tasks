package examples

import scala.language.implicitConversions

object Example04 extends App {

  trait Currency
  case class Dollar(amount: Double) extends Currency
  case class Euro(amount: Double)   extends Currency

  object Currency {
    implicit def euroToDollar(euro: Euro): Dollar = Dollar(euro.amount * 1.13)
  }

  implicit def euroToDollar(euro: Euro): Dollar = Dollar(euro.amount)

  val dollar: Dollar = Euro(100) // euroToDollar

  println(dollar)
}
