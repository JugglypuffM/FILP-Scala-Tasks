package examples

import examples.typeclasses.Monoid
import examples.typeclasses.MonoidInstances._
import examples.typeclasses.MonoidSyntax._

object Example02_Monoid extends App {

  def combineAll[A: Monoid](list: List[A]): A = {
    list.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  val res1: Int = combineAll(List(1, 2, 3))
  println(res1)
  // result: Int = 6

  val res2: Int = combineAll[Int](Nil)
  println(res2)
  // result: Int = 0

  val res3: String = combineAll(List("Hello", "my", "little", "pony"))
  println(res3)
  // result: String = Hellomylittlepony

  val res4: Option[Int] = combineAll[Option[Int]](List(Some(1), Some(2), Some(3)))
  println(res4)
  // result: Option[Int] = Some(6)

  // Laws

  // Associativity
  val associativity: Boolean = ((1 |+| 2) |+| 3) == (1 |+| (2 |+| 3))

  // Left identity
  val leftIdentity: Boolean  = (Monoid[Int].empty |+| 42) == 42
  val rightIdentity: Boolean = (42 |+| Monoid[Int].empty) == 42
}
