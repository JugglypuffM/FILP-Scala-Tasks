package lecture

import scala.util.Random

object Example02PureFunctions {

  // Is pure?

  def func1(foo: String, bar: Int): String = {
    foo + bar
  }
  // Answer - YES



  var number2 = 5

  def func2(foo: String, bar: Int): String = {
    number2 = 6
    foo + bar
  }
  // Answer - NO



  var number3 = 5

  def func3(foo: String, bar: Int): String = {
    foo + bar + number3
  }
  // Answer - NO



  def func4(foo: String, bar: Int): String = {
    val number4 = 5
    foo + bar + number4
  }
  // Answer - YES



  val number5 = 5

  def func5(foo: String, bar: Int): String = {
    foo + bar + number5
  }
  // Answer - YES



  def func6(foo: String, bar: Int): String = {
    val number6 = new Random().nextInt(13)
    foo + bar + number6
  }
  // Answer - NO



  def func7(foo: String, bar: Int): String = {
    val number7 = Math.sin(13)
    foo + bar + number7
  }
  // Answer - YES



  def func8(foo: String, bar: Int): String = {
    println("I'm pure!")
    foo + bar
  }
  // Answer - NO :)



  def func9(foo: String, bar: Int): String = {
    val result = func8(foo, bar)
    result + foo
  }
  // Answer - NO
}
