object f03_callback_hell extends App {

  def multiplyWithCallback(a: Int, b: Int)(callback: Int => Unit): Unit = {
    val result = a * b
    callback(result)
  }

  val i128 = 2 * 2 * 2 * 2 * 2 * 2 * 2

  multiplyWithCallback(2, 2) {
    multiplyWithCallback(_, 2) {
        multiplyWithCallback(_, 2) {
            multiplyWithCallback(_, 2) {
              multiplyWithCallback(_, 2) {
                multiplyWithCallback(_, 2) {
                  multiplyWithCallback(_, 2) {
                    println(_)
                  }
                }
              }
            }
          }
      }
  }



}
