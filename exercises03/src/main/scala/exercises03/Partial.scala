package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] =
    I => funcs.map(_.lift(I)).collectFirst({case Some(res) => res})

}
