package consacols

object Example15Folds extends App {
  val list = List(1, 2, 3, 4, 5, 6, 7)
  //fold
  list.fold(0)(_ + _) // i.e. sum
  list.fold(1)(_ * _) // i.e. *

  list.reduce(_ + _) // не используйте !!! падает на пустом списке
  //foldLeft
  println(
    list.foldLeft(0)(_ - _)
  )
  // foldLeft(zero)(f(b,a))  == (((((((0 - 1) - 2) - 3) - 4) - 5) - 6) - 7) == f(f(f(f(f(f(f(zero,x0),x1),x2),x3),x4),x5),x6)

  //foldRight
  println(
    list.foldRight(0)(_ - _)
  )
  // foldRight(zero)(f(a,b)) == (0 - (1 - (2 - (3 - (4 - (5 - (6 - 7))))))) == f(zero,f(x0,f(x1,f(x2,f(x3,f(x4,f(x5,x6)))))))

  // we can fold to more complex data structure,
  // for example use foldRight to copy list
  println(
    list.foldRight(List.empty[Int])((x, acc) => x :: acc)
  )
  // or compute number frequencies
  println(
    (1 :: 5 :: list).foldLeft(Map.empty[Int, Int]) { (acc, x) =>
      acc + (x -> (acc.getOrElse(x, 0) + 1))
    }
  )
}
