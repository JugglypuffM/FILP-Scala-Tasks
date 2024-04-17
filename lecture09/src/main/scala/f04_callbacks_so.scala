object f04_callbacks_so extends App {

  def multiplyWithCallback(a: BigInt, b: BigInt)(callback: BigInt => Unit): Unit = {
    val result = a * b
    callback(result)
  }

  def factorial(n: BigInt)(callback: BigInt => Unit): Unit =
    if (n == 1) callback(1) else {
      multiplyWithCallback(n, 1) {
        result => factorial(n - 1)(f => multiplyWithCallback(f, result)(callback))
      }
    }


    factorial(5000)(
      r => println(r)
    )

}
