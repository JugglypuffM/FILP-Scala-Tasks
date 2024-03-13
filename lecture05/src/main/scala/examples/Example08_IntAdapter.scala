package examples

import scala.language.implicitConversions

object Example08_IntAdapter extends App {

  class IntAdapter(val i: Int) {
    def isEven: Boolean = i % 2 == 0
    def isOdd: Boolean  = !isEven
  }

  // Создание экземпляра адаптера и использование его методов
  println(new IntAdapter(42).isEven)
  println(new IntAdapter(42).isOdd)
}
