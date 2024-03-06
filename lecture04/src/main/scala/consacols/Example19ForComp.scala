package consacols

object Example19ForComp extends App {
  val list  = List(1, 2, 3, 4, 5, 5, 6, 7)
  val list2 = List(1, 2, 3, 4, 5, 5, 6, 7)
  val list3 = List(1, 2, 3, 4, 5, 5, 6, 7)
  val list4 = List(1, 2, 3, 4, 5, 5, 6, 7)
  val list5 = List(1, 2, 3, 4, 5, 5, 6, 7)

//  list.flatMap{
  //  x => list2.flatMap(y => list3.flatMap(z => ...)
  //}

  for {
    x <- list
    y <- list2
    z <- list3
    // ...
  } yield x + y + z // + ...

}
