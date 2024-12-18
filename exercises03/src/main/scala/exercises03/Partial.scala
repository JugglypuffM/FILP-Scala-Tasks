package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] =
    func => funcs.map(_.lift(func)).collectFirst({ case Some(res) => res })
}
