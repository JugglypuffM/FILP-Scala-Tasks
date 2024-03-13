package consacols

import scala.collection.mutable

object Example10Set extends App {
  // construct
  val setIM: Set[String] =
    Set("one", "two", "three", "one1", "two2", "three3")

  //access
  val elemIM: Boolean = setIM("one") // set is like function

  //updates
  val setIM1: Set[String] = setIM + "four"
  val setIM2: Set[String] = setIM - "one"

  val setM: mutable.Set[String] = mutable.Set("one", "two", "three")

  //access
  val elem: Boolean = setM("one") // set is like function

  //updates in place
  setM += "four"
  setM -= "three"

  println(setM)
}
