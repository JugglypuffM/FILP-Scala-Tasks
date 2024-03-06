package consacols

object Example17Grouping extends App {
  val list: List[Int] = List(1, 2, 3, 4)

  // can be group by
  println(
    list.groupBy(x => x % 2)
  )

  // can be grouped to Iterator
  println(
    list.grouped(3).toList
  )

  // can be slided to Iterator
  println(
    list.sliding(3).toList
  )

  println(
    list.tails.toList
  )

  println(
    list.groupMapReduce(identity)(_ => 1)(_ + _)
  )
}
