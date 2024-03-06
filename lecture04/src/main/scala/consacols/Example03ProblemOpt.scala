package consacols

import consacols.Example02Option.{MyNone, MyOption, MySome}

// calculator try 2/option
object Example03ProblemOpt extends App {

  def calculate(a: Int, b: Int): MyOption[Int] =
    b match {
      case 0 => MyNone
      case _ => MySome(a / b)
    }

  println(calculate(1, 2)) // ok

  println(calculate(1, 0)) // runtime Failure

}
