package consacols

object Example08List extends App {
  abstract class LList[+A] {
    def headOption: Option[A]
    def tail: LList[A]
  }

  case object NNil extends LList[Nothing] {
    override def headOption: Option[Nothing] = None
    override def tail: LList[Nothing] =
      throw new UnsupportedOperationException("tail of empty llist")
  }

  case class Cons[+A](head: A, next: LList[A]) extends LList[A] {
    override def headOption: Option[A] = Some(head)
    override def tail: LList[A]        = next
  }

  val llist: Cons[Int] = Cons(1, Cons(2, NNil))

  // construct
  val list1: List[Int] = List(1, 2, 3, 4, 5)
  val list2: List[Int] = 1 :: 2 :: 3 :: Nil
  val list3: List[Int] = List.tabulate(10)(n => n * 2)
  println(list3)
  println("tab")

  val list5: List[Int] = (1 to 10).toList
  println(list5)
  println(list1)

  //access
  val head: Int            = list1.head // not good
  val tail: List[Int]      = list1.tail // also not good
  val headOpt: Option[Int] = list1.headOption

  // with pattern matching
  val rmatch: String = list2 match {
    case head :: f :: s :: Nil => s"$f $s"
    case h :: t                => s"$h and $t"
    case Nil                   => "this is the end"
  }

  val elem: Int        = list1(1)
  val list4: List[Int] = 0 :: list2

  println(rmatch)
}
