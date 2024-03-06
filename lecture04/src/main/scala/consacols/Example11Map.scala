package consacols

object Example11Map {
  val map = Map("one" -> 1, "two" -> 2, "three" -> 3)

  val elem    = map("one")
  val elemOpt = map.get("one")

  val fun: PartialFunction[String, Int] = map

  val map1 = map + ("four" -> 4)
  val map2 = map - "one"
}
