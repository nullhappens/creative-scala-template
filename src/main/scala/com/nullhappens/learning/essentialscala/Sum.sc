sealed trait Sum[+A, +B] {
  def fold[C](left: A => C , right: B => C): C =
    this match {
      case Failure(x) => left(x)
      case Success(x) => right(x)
    }

  def map[C](fn: B => C): Sum[A, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(y) => Success(fn(y))
    }

  def flatMap[AA >: A, C](fn: B => Sum[AA,C]): Sum[AA, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(y) => fn(y)
    }
}
final case class Failure[A, B](value: A) extends Sum[A, Nothing]
final case class Success[A, B](value: B) extends Sum[Nothing, B]

Failure[Int, String](1).value
Success[Int, String]("foo").value

val sum: Sum[Int, String] = Success("foo")
sum match {
  case Failure(x) => x.toString
  case Success(x) => x
}

sum.map(x => x + " bar")