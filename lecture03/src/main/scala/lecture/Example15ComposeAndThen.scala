package lecture

object Example15ComposeAndThen extends App {
  val add2: Int => Int   = (x: Int) => x + 2
  val times3: Int => Int = (x: Int) => x * 3

  val composed: Int => Int = add2.compose(times3) // == add2(times3(x))
  val ordered: Int => Int  = add2.andThen(times3) // == times3(add2(x))

  println(composed(4))
  println(ordered(4))

  // 4 * 3 + 2
  // (4 + 2) * 3
}
