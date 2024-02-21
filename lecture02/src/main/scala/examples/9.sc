// Циклы

val numbers = List(1, 2, 3)
val returned =
  for (n <- numbers) println(n)

numbers.foreach(println)


val squares =
  for (n <- numbers) yield n * n

val otherNumbers = List(4, 5, 6)

val complexStuff: List[Int] = for {
  low <- numbers
  high <- otherNumbers
} yield low * high
