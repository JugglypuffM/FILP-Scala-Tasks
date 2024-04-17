package lecture8

object t1_Task {
  def task(id: Int): Unit = {
    List.range(0, 4).foreach(
      el => {
        Thread.sleep(1000)
        println(s"Task $id : $el")
      }
    )
    println(s"Task $id is done")
  }
}
