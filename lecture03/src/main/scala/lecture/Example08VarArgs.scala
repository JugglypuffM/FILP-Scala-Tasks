package lecture

object Example08VarArgs extends App {

  def sum(items: Int*): Int = {
    var total = 0
    for (i <- items) total += i
    total
  }

  println(sum(1, 2, 3, 4, 5))

  //convert Seq to vararg
  println(sum(Seq(1, 2, 3, 4, 5): _*))
}
