package lecture

object Def extends App {

  def func1(foo: String, bar: Int): String = {
    foo + bar
  }

  def func2(foo: String, bar: Int): String =
    foo + bar

  def func3(foo: String, bar: Int): String = {
    val value = 5
    foo + bar + value
  }

  val number = 10

  def func4(foo: String, bar: Int): String =
    foo + bar + number

  println(func1("hello", 2))
}

trait Def {
  def func5(foo: String, bar: Int): String =
    foo + bar
}

class DefClass {
  def func5(foo: String, bar: Int): String =
    foo + bar
}
