package exercises08

trait MyThreadFactory {
  def startThread(task: () => Unit): Thread
}
