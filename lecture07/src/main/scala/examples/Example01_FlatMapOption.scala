package examples

object Example01_FlatMapOption extends App {

  Option("42")
    .map(value => value.toIntOption) // Option[Option[Int]]
    .flatten                         // Option[Int]

  Option("42").flatMap(value => value.toIntOption) // Option[Int]

}
