// неизменяемые переменные

val fruit = "apple"
//fruit = "orange"
/*
reassignment to val
fruit = "orange"
*/

val laws = List("#1", "#1")
//laws(0) = "#0" // value update is not a member of List[String]

val updated = laws.updated(0, "#0")

laws // не изменился

val newLaws = "#-1" :: updated
