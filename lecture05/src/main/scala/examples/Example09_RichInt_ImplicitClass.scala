package examples

import scala.language.implicitConversions

object Example09_RichInt_ImplicitClass extends App {

  implicit class RichInt(private val i: Int) extends AnyVal {
    def isEven: Boolean = i % 2 == 0
    def isOdd: Boolean  = !isEven
  }

  // Создание экземпляра адаптера и использование его методов
  println(42.isEven)
  println(42.isOdd)
}
