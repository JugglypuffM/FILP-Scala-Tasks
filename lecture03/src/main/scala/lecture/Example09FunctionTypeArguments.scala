package lecture

object Example09FunctionTypeArguments extends App {
  def identityAny(a: Any): Any = a
  def identityStr(a: String): String = a
  def identityInt(a: Int): Int = a
  //...






  def identity[T](a: T): T = a

  identity[Int](1)
  identity[String]("1")
}
