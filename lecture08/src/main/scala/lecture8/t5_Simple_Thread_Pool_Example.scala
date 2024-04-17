package lecture8



object t5_Simple_Thread_Pool_Example extends App {
  val simpleThreadPool = new SimpleThreadPool(10)

  List.range(0, 10).foreach(
    i => simpleThreadPool.execute(
      () => task(i)
    )
  )


  Thread.sleep(10000)
}
