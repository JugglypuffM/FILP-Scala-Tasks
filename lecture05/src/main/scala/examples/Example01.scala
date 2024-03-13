package examples

import scala.language.implicitConversions

object Example01 extends App {

  implicit def intToString(x: Int): String = x.toString

  val x: String = 123 // будет вызван intToString
}
