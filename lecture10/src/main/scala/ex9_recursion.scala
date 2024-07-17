

object ex9_recursion extends App {
  def factorialNoTrampoline(f: BigInt): IO[BigInt] =
    if(f == 1) {
      IO(1)
    } else {
      IO.defer(factorialNoTrampoline(f - 1))
        .map(_ * f)
    }

  def factorialTrampoline(f: BigInt, acc: BigInt = 1): IO[BigInt] =
    if(f == 1) IO(acc) else factorialTrampoline(f - 1, acc * f)

  factorialNoTrampoline(10).map(println)
    .unsafeRunSync()

  factorialTrampoline(10).map(println)
    .unsafeRunSync()
}
