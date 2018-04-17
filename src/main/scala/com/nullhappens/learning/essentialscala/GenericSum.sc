sealed trait Sum[A, B] {
  def fold[C](left: A => C , right: B => C): C =
    this match {
      case Left(x) => left(x)
      case Right(x) => right(x)
    }     
}
final case class Left[A, B](value: A) extends Sum[A, B]
final case class Right[A, B](value: B) extends Sum[A, B]

Left[Int, String](1).value
Right[Int, String]("foo").value

val sum: Sum[Int, String] = Right("foo")
sum match {
  case Left(x) => x.toString
  case Right(x) => x
}
