package lecture

object Example13AnonymousFunction extends App {

  val doubler = new Function[Int, Int] {
    override def apply(x: Int): Int = x * 2
  }

  // anonymous function or lambda

  val doubler2 = (x: Int) => x * 2

  val doubler3: Int => Int = (x: Int) => x * 2

  val doubler4: Int => Int = x => x * 2

  val doubler5: Int => Int = _ * 2

  val adder = (a: Int, b: Int) => a + b

  val adder2: (Int, Int) => Int = _ + _

  val noParams = () => 2

  println(noParams)
  println(noParams())

  val curlyBraces = { (x: Int) =>
    x.toString
  }
}
