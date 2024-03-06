package consacols

import scala.collection.mutable

object Example10Set extends App {
  // construct
  val setIM =
    Set("one", "two", "three", "one1", "two2", "three3")

  //access
  val elemIM = setIM("one") // set is like function

  //updates
  val setIM1 = setIM + "four"
  val setIM2 = setIM - "one"

  val setM = mutable.Set("one", "two", "three")

  //access
  val elem = setM("one") // set is like function

  //updates in place
  setM += "four"
  setM -= "three"

  println(setM)
}
