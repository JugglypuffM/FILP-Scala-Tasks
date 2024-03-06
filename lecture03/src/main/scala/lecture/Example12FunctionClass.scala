package lecture

object Example12FunctionClass extends App {

  class Doubler {
    def execute(number: Int): Int = number * 2
  }

  val doubler = new Doubler()

  println(doubler.execute(2))

  class Doubler2 {
    def apply(number: Int): Int = number * 2
  }

  val doubler2 = new Doubler2()

//  println(doubler2.apply(2))
//  println(doubler2(2))

  trait MyFunction[A, B] {
    def apply(argument: A): B
  }

  val doubler3 = new MyFunction[Int, Int] {
    override def apply(argument: Int): Int = argument * 2
  }

//  println(doubler3(2))

  val function = new Function1[Int, Int] {
    override def apply(v1: Int): Int = ???
  }

  val function2 = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = ???
  }

  val adder = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

//  println(adder(2, 2))

  val adder2: Function2[Int, Int, Int] = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  val adder3: (Int, Int) => Int = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
  // Function0[R]       === () => R
  // Function1[A, R]    === A => R
  // Function2[A, B, R] === (A, B) => R
  // ...
  // Function22[...]
}
