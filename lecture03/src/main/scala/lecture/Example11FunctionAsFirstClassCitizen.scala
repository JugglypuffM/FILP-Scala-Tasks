package lecture

object Example11FunctionAsFirstClassCitizen {

  // function as first class citizen or first class elements

  def function(value: Int): Int = ???

  def otherFunction(f: Int => Int): Int = ???

  otherFunction(function)

  def otherFunction2(): Int => Int = function
}
