object f02_callbacks_with_returnes extends App {

  def multiplyWithCallback(a: Int, b: Int)(callback: Int => Unit): Unit = {
    val result = a * b
    callback(result)
  }

  multiplyWithCallback(2, 2) {
    r1 => multiplyWithCallback(r1, 2) {
      r2 => multiplyWithCallback(r2, 2) {
        r3 => println(r3)
      }
    }
  }

  def factorial(n: Int)(callback: Int => Unit): Unit =
    if (n == 1) callback(1) else {
      multiplyWithCallback(n, 1) {
        result => factorial(n - 1)(f => multiplyWithCallback(f, result)(callback))
      }
    }

  factorial(10)(
    r => println(r)
  )



}
