package consacols

object Example13Views extends App {
  val list = List(1, 2, 3, 4, 5, 5, 6, 7)

  val view0 = list.view

  println("filter")
  val view1 = view0.filter { x =>
    println(s"filtering $x"); x % 2 == 0
  }
  println("map")
  val view2 = view1.map { x =>
    println(s"mapping $x"); x
  }
  println("flatmap")
  val view3 = view2.flatMap { x =>
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
