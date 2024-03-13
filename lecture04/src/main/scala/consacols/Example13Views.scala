package consacols

import scala.collection.{SeqView, View}
object Example13Views extends App {
  val list: List[Int] = List(1, 2, 3, 4, 5, 5, 6, 7)

  val view0: SeqView[Int] = list.view

  println("filter")
  val view1: View[Int] = view0.filter { x =>
    println(s"filtering $x"); x % 2 == 0
  }
  println("map")
  val view2: View[Int] = view1.map { x =>
    println(s"mapping $x"); x
  }
  println("flatmap")
  val view3: View[Int] = view2.flatMap { x =>
    println(s"flatmapping $x"); List(x, x)
  }
  println("\n\n\nresult\n\n\n")
  println(
    view3.take(2).toList
  )
  println("\n\n\nresult\n\n\n")

  println(
    view3.toList
  )
}
