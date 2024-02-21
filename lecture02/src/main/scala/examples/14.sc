import scala.annotation.tailrec
// Nothing, Null и Unit

def unitFunction(): Unit = println("123")

val a: List[Nothing] = Nil

val b: Null = null

val c: String = null

def nothingFunction(): Nothing = throw new NotImplementedError()

@tailrec
def nothingFunction2(): Nothing = nothingFunction2()


