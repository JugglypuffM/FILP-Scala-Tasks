package adt

object Example04 extends App {
  // Tuples - альтернативный вариант product типа

  val a: (String, Int) = ("str", 1)

  val a2: Tuple2[String, Int] = Tuple2("str", 1)

  println(a == a2) // true

  println(a._1) // "str"
  print(a._2)   // 1

  println {
    a match {
      case (first, second) => s"$first, $second"
    }
  }
}
