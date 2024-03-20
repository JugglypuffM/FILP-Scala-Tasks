package exercises06.ex01

object Run extends App {
  import exercises06.data._
  import exercises06.typeclasses._
  import exercises06.ex01.Exercise01.Instances._
  import exercises06.ex01.Exercise01.Syntax._

  println("Semigroup syntax:")

  println(1 |+| 2)                                               // 3
  println("a" |+| "b")                                           // ab
  println(NonEmptyList.of(1, 2, 3) |+| NonEmptyList.of(4, 5, 6)) // NonEmptyList(1,List(2, 3, 4, 5, 6))
  println(List(1, 2, 3) |+| List(4, 5, 6))                       // List(1, 2, 3, 4, 5, 6)

  println()
  println("Applicative pure syntax:")

  println(1.pure[Option])       // Some(1)
  println(1.pure[List])         // List(1)
  println("a".pure[List])       // List(a)
  println(2.pure[NonEmptyList]) //NonEmptyList(2,List())

  println()
  println("Foldable + Monoid syntax")

  println(List("a", "b", "c").combineAll)                // abc
  println(List(1, 2, 3).combineAll)                      // 6
  println(List(List(1, 2, 3), List(3, 4, 5)).combineAll) // List(1, 2, 3, 3, 4, 5)
  println(NonEmptyList.of("a", "b", "c").combineAll)     // abc
  println(NonEmptyList.of(1, 2, 3).combineAll)           // 6
  println(NonEmptyList.of(2, 3, 4).foldLeft(1)(_ * _))   // 24
  println(List.empty[Int].combineAll)                    // 0

  println()
  println("Applicative syntax")

  println(List(1, 2, 3).aproduct(List("a", "b", "c")))                              // List((1,a), (2,b), (3,c))
  println(List(1, 2, 3).aproduct(List("a", "b")))                                   // List((1,a), (2,b))
  println(Option(1).aproduct(Option("a")))                                          // Some((1, a))
  println(Option(1).aproduct(None))                                                 // None
  println(NonEmptyList.of(1, 2, 3).aproduct(NonEmptyList.of(4, 5, 6)))              // NonEmptyList((1,4),List((2,5), (3,6)))
  println(Applicative[List].ap[Int, Int](List(_ + 1, _ + 2, _ + 3))(List(1, 2, 3))) // List(2, 4, 6)

  println()
  println("Traverse syntax")

  println(List(1, 2, 3, 8).traverse(getTag))            // Some(List(Less, Less, Less, Greater))
  println(List(6, 10).traverse(getTag))                 // None
  println(List.empty[Int].traverse(getTag))             // Some(List())
  println(Option(1).traverse(getTag))                   // Some(Some(Less))
  println(Option.empty[Int].traverse(getTag))           // Some(None)
  println(NonEmptyList.of(1, 2, 3, 8).traverse(getTag)) // Some(NonEmptyList(Less,List(Less, Less, Greater)))
  println(NonEmptyList.of(6, 10).traverse(getTag))      // None

  def getTag(id: Int): Option[String] =
    if (id < 5) Some("Less")
    else if (id > 7) Some("Greater")
    else None

  println()
  println("Functor syntax")

  println(NonEmptyList.of(1, 2, 3).map(_ + 1)) // NonEmptyList(2,List(3, 4))
}
