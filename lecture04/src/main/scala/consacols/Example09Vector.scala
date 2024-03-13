package consacols

object Example09Vector extends App {
  // construct
  val vec1: Vector[Int] = Vector(1, 2, 3, 4, 5)
  val vec2: Vector[Int] = Vector.tabulate(5)(x => x * x)

  val head: Int            = vec1.head
  val tail: Vector[Int]    = vec1.tail
  val headOpt: Option[Int] = vec1.headOption
  val elem: Int            = vec1(2)

  vec2 match {
    case tail +: head => s"$head and $tail"
    case _            => "this is the end"
  }

  val vec3: Vector[Int] = 0 +: vec2
  val vec4: Vector[Int] = vec2 :+ 0
  val vec5: Vector[Int] = vec2.updated(1, 42)

  println(vec4)
}
