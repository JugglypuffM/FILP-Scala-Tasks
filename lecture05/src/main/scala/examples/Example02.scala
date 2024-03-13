package examples

import scala.language.implicitConversions

object ExternalImplicits {
  implicit def intToString(x: Int): String = x.toString
}

object Example02 extends App {

  import ExternalImplicits.intToString
  // ИЛИ импортируем все
  import ExternalImplicits._

  val x: String = 123
}
