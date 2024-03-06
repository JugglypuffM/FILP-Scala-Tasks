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

  val llist = Cons(1, Cons(2, NNil))

  // construct
  val list1 = List(1, 2, 3, 4, 5)
  val list2 = 1 :: 2 :: 3 :: Nil
  val list3 = List.tabulate(10)(n => n * 2)
  println(list3)
  println("tab")

  val list5 = (1 to 10).toList
  println(list5)
  println(list1)

  //access
  val head    = list1.head // not good
  val tail    = list1.tail // also not good
  val headOpt = list1.headOption

  // with pattern matching
  val rmatch = list2 match {
    case head :: f :: s :: Nil => s"$f $s"
    case h :: t                => s"$h and $t"
    case Nil                   => "this is the end"
  }

  val elem  = list1(1)
  val list4 = 0 :: list2

  println(rmatch)
}
