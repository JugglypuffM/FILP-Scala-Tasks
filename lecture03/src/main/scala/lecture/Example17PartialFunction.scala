package lecture

object Example17PartialFunction extends App {

  val multiply: Int => Int = _ * 2



  val odd: PartialFunction[Int, Int] = {
    case x if x % 2 == 1  =>  x * 3
  }

  val even: PartialFunction[Int, Int]  = {
    case x if x % 2 == 0  =>  x / 2
  }

//  println(odd(2))
  println(odd(3))
//  println(even(3))
  println(even(2))


//  println(even.isDefinedAt(2))


  val lifted: Int => Option[Int] = even.lift

//  println(lifted(2))
//  println(lifted(3))


  val f: PartialFunction[Int, Int] = even.orElse(odd)

//  println(f(2))
//  println(f(3))



  //partial by hand
  val evenByHand = new PartialFunction[Int,Int] {
    override def isDefinedAt(x: Int): Boolean = x % 2 == 0
    override def apply(arg: Int): Int = arg match {
      case x if x % 2 == 0  =>  x / 2
    }
  }

//  println(evenByHand(2))
//  println(evenByHand(3))



  val aMappedList = List(1, 2, 3).map {
    case 1 => "A"
    case 2 => "B"
    case 3 => "C"
  }

//  println(aMappedList)


  val collectedList = List(1, 2, 3, 4).collect {
      case x if x % 2 == 0 => s"even - $x"
    }

//  println(collectedList)


  val collectedList2 = List(1, "two", 3, "four").collect {
    case x: String => x * 2
  }

//  println(collectedList2)
}
