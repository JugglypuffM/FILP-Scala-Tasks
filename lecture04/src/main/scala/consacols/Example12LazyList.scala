package consacols

object Example12LazyList extends App {
  def fibFrom(a: Int, b: Int): LazyList[Int] =
    a #:: fibFrom(b, a + b)

  val ll = fibFrom(1, 1)
  ll.head
  println(ll.tail)
  println(ll)
  println(ll.take(10).toList)
}
