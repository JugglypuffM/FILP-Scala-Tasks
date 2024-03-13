package consacols

object Example11Map {
  val map: Map[String, Int] = Map("one" -> 1, "two" -> 2, "three" -> 3)

  val elem: Int            = map("one")
  val elemOpt: Option[Int] = map.get("one")

  val fun: PartialFunction[String, Int] = map

  val map1: Map[String, Int] = map + ("four" -> 4)
  val map2: Map[String, Int] = map - "one"
}
