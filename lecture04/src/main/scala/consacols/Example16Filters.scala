package consacols

object Example16Filters extends App {
  val map: Map[String, Int] = Map("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
  // for all traversables
  println(
    map.filter { case (k, v) => v >= 3 }
  )
  println(
    map.filterNot { case (k, v) => k == "three" }
  )

  println {
    map.collect { case (k, v) if v >= 3 => k -> v * 3 }
  }
  println {
    map.filter { case (k, v) => v >= 3 }.map { case (k, v) => k -> v * 3 }
  }
}
