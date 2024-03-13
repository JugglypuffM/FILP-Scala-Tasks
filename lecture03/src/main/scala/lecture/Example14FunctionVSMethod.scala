package lecture

object Example14FunctionVSMethod extends App {

  def method(arg: Int): String = arg.toString

  val function: Int => String = arg => arg.toString

  Example14FunctionVSMethod.method(2)

  method(2)

  this.method(2)

  function(5)

  val f1: Int => String = function
//  val f2 = method

  println(function)
//  println(method)

  val functions: List[Int => String] = List(function)
//  val methods = List(method)

  val f3: Int => String = method _ // eta-expansion

  val f4: Int => String = (arg: Int) => method(arg)

  val f5: Int => String = method

  List(1, 2, 3).map(method)
}
