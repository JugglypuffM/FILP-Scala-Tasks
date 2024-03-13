package lecture

object Example16Curry extends App {

  def adder(x: Int, y: Int): Int = x + y

//  val add1 = adder(1, ...)

  def adder1(x: Int)(y: Int): Int = x + y

  val add2: Int => Int = adder1(2) _
  val add5: Int => Int = adder1(5) _

  println(add2(8))
  println(add5(5))
  println(adder1(3)(7))

  // HOFs - higher order functions
  // map, flatMap, filter

  val adder2: Int => Int => Int = (x: Int) => (y: Int) => x + y

  val add3: Int => Int = adder2(3)
  val add6: Int => Int = adder2(6)

//  println(add3(7))
//  println(add6(4))
//  println(adder2(9)(1))
  println()

  val adder3: (Int, Int) => Int = (x: Int, y: Int) => x + y

  val curried: Int => (Int => Int) = adder3.curried

  def formatter(s: String, x: Double): String = s.format(x)

  val curriedFormatter: String => (Double => String) = (formatter _).curried

  val standardFormat: Double => String = curriedFormatter("%1.2f")
  val preciseFormat: Double => String  = curriedFormatter("%1.5f")

//  println(standardFormat(Math.PI))
//  println(preciseFormat(Math.PI))
}
