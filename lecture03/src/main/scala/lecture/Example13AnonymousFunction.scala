package lecture

object Example13AnonymousFunction extends App {

  val doubler: Function[Int,Int] = new Function[Int, Int] {
    override def apply(x: Int): Int = x * 2
  }

  // anonymous function or lambda

  val doubler2: Int => Int = (x: Int) => x * 2

  val doubler3: Int => Int = (x: Int) => x * 2

  val doubler4: Int => Int = x => x * 2

  val doubler5: Int => Int = _ * 2

  val adder: (Int, Int) => Int = (a: Int, b: Int) => a + b

  val adder2: (Int, Int) => Int = _ + _

  val noParams: () => Int = () => 2

  println(noParams)
  println(noParams())

  val curlyBraces: Int => String = { (x: Int) =>
    x.toString
  }
}
