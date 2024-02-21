import scala.annotation.tailrec
// хвостовая рекурсия

val numbers = List(1, 2, 3, 4)

def sum(values: List[Int]): Int = {
  var result = 0
  val it = values.iterator
  while (it.hasNext) {
    result = result + it.next()
  }
  result
}

@tailrec
def sumTailRecIf(acc: Int, values: List[Int]): Int =
  if (values.nonEmpty)
    sumTailRecIf(acc + values.head, values.tail)
  else
    acc

@tailrec
def sumTailRec(acc: Int, values: List[Int]): Int =
  values match {
    case (head :: tail) => sumTailRec(acc + head, tail)
    case Nil            => acc
  }

sum(numbers)
sumTailRec(0, numbers)
sumTailRecIf(0, numbers)